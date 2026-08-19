;;; ghostel-viewport-editor-integration-test.el --- Ghostel integration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests exercise real Ghostel PTYs, the generated helper, dtach, and
;; zsh startup ordering.  Optional programs cause their tests to be skipped.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ghostel)
(require 'ghostel-viewport-editor)

(defun ghostel-viewport-editor-integration-test-run-batch (&optional strict)
  "Run integration tests, failing on skips when STRICT is non-nil."
  (let* ((stats (ert-run-tests-batch t))
         (unexpected (ert-stats-completed-unexpected stats))
         (skipped (ert-stats-skipped stats)))
    (when (and strict (> skipped 0))
      (message "Strict integration run skipped %d test(s)" skipped))
    (kill-emacs (if (or (> unexpected 0)
                        (and strict (> skipped 0)))
                    1
                  0))))

(defun ghostel-viewport-editor-integration-test--wait-until
    (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 8)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    value))

(defun ghostel-viewport-editor-integration-test--viewport (source)
  "Return the live viewport belonging to Ghostel SOURCE, or nil."
  (when (buffer-live-p source)
    (when-let* ((request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source))
                (viewport
                 (ghostel-viewport-editor--request-viewport-buffer request))
                ((buffer-live-p viewport)))
      viewport)))

(defun ghostel-viewport-editor-integration-test--without-env
    (names environment)
  "Return ENVIRONMENT without variables named by NAMES."
  (cl-remove-if
   (lambda (entry)
     (cl-some
      (lambda (name) (string-prefix-p (concat name "=") entry))
      names))
   environment))

(defun ghostel-viewport-editor-integration-test--kill-dtach (pid-file)
  "Terminate the child and dtach server recorded through PID-FILE."
  (when (file-readable-p pid-file)
    (let ((pid
           (with-temp-buffer
             (insert-file-contents pid-file)
             (string-to-number (string-trim (buffer-string))))))
      (when (> pid 1)
        (let* ((attributes (process-attributes pid))
               (server-pid (and attributes (alist-get 'ppid attributes))))
          (when (and server-pid (> server-pid 1))
            (ignore-errors (signal-process server-pid 15)))
          (ignore-errors (signal-process pid 15))
          (ghostel-viewport-editor-integration-test--wait-until
           (lambda ()
             (and (null (process-attributes pid))
                  (or (null server-pid)
                      (null (process-attributes server-pid)))))
           2))))))

(defun ghostel-viewport-editor-integration-test--make-fallback
    (directory)
  "Create a fallback editor and marker below DIRECTORY."
  (let ((fallback (expand-file-name "fallback" directory))
        (marker (expand-file-name "fallback-ran" directory)))
    (with-temp-file fallback
      (insert "#!/bin/sh\nprintf invoked > "
              (shell-quote-argument marker) "\n"))
    (set-file-modes fallback #o700)
    (cons fallback marker)))

(defmacro ghostel-viewport-editor-integration-test--with-runtime
    (&rest body)
  "Run BODY with isolated package and Ghostel process state."
  (declare (indent 0) (debug t))
  `(let* ((test-state-directory
           (make-temp-file "ghostel-viewport-integration-" t))
          (ghostel-viewport-editor-state-directory
           (file-name-as-directory test-state-directory))
          (ghostel-viewport-editor--token nil)
          (ghostel-viewport-editor--token-state-root nil)
          (ghostel-viewport-editor--instance-id nil)
          (ghostel-viewport-editor--helper-file nil)
          (ghostel-viewport-editor--activation-file nil)
          (ghostel-viewport-editor--response-directory nil)
          (ghostel-viewport-editor--requests
           (make-hash-table :test #'equal))
          (ghostel-viewport-editor-global-mode nil)
          (ghostel-eval-cmds (copy-tree ghostel-eval-cmds))
          (ghostel-pre-spawn-hook (copy-sequence ghostel-pre-spawn-hook))
          (ghostel-mode-hook (copy-sequence ghostel-mode-hook))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          (ghostel-use-native-pty nil)
          (ghostel-kill-buffer-on-exit nil))
     (unwind-protect
         (progn ,@body)
       (when ghostel-viewport-editor-global-mode
         (ghostel-viewport-editor-global-mode -1))
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (bound-and-true-p ghostel-viewport-editor-mode)
             (setq ghostel-viewport-editor--suppress-kill-query t)
             (set-buffer-modified-p nil)
             (kill-buffer buffer))))
       (when (file-directory-p test-state-directory)
         (delete-directory test-state-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-fresh-ghostel-round-trip ()
  "A new Ghostel child should finish without invoking its fallback."
  (skip-unless (ghostel-viewport-editor--supported-host-p))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-fresh-" t))
          (fallback-pair
           (ghostel-viewport-editor-integration-test--make-fallback
            work-directory))
          (fallback (car fallback-pair))
          (fallback-marker (cdr fallback-pair))
          (target (expand-file-name "prompt.md" work-directory))
          (source (generate-new-buffer " *ghostel-viewport-fresh*"))
          (process-environment
           (append
            (list (concat "EDITOR=" fallback)
                  (concat "VISUAL=" fallback))
            (ghostel-viewport-editor-integration-test--without-env
             '("EDITOR" "VISUAL") process-environment)))
          process)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (ghostel-viewport-editor-global-mode 1)
           (setq process
                 (ghostel-exec
                  source "/bin/sh"
                  (list "-c" "exec $EDITOR \"$1\""
                        "viewport-fresh" target)))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (ghostel-viewport-editor-integration-test--viewport
                source))))
           (with-current-buffer source
             (should ghostel-viewport-editor--source-input-locked)
             (should (eq overriding-local-map
                         ghostel-viewport-editor--source-lock-map))
             (when-let* ((window (get-buffer-window source)))
               (should (eq (window-parameter window 'no-other-window) t))))
           (let ((viewport
                  (ghostel-viewport-editor-integration-test--viewport
                   source)))
             (with-current-buffer viewport
               (erase-buffer)
               (insert "edited in Emacs\n")
               (ghostel-viewport-editor-finish)))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda () (not (process-live-p process)))))
           (should (zerop (process-exit-status process)))
           (should-not (file-exists-p fallback-marker))
           (with-current-buffer source
             (should-not ghostel-viewport-editor--source-input-locked)
             (should-not (local-variable-p 'overriding-local-map)))
           (with-temp-buffer
             (insert-file-contents target)
             (should (equal (buffer-string) "edited in Emacs\n"))))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-cancel-refreshes-restored-source ()
  "Canceling a viewport should render pending Ghostel output when it returns."
  (skip-unless (ghostel-viewport-editor--supported-host-p))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-cancel-refresh-" t))
          (target (expand-file-name "prompt.md" work-directory))
          (source (generate-new-buffer " *ghostel-viewport-refresh*"))
          forced-windows
          redisplayed
          process)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (ghostel-viewport-editor-global-mode 1)
           (setq process
                 (ghostel-exec
                  source "/bin/sh"
                  (list "-c" "exec $EDITOR \"$1\""
                        "viewport-refresh" target)))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (ghostel-viewport-editor-integration-test--viewport
                source))))
           (let ((original-force-window-update
                  (symbol-function 'force-window-update))
                 (original-redisplay (symbol-function 'redisplay))
                 (original-force-redraw
                  (symbol-function 'ghostel-force-redraw))
                 redrawn)
             (cl-letf (((symbol-function 'force-window-update)
                        (lambda (window)
                          (push window forced-windows)
                          (funcall original-force-window-update window)))
                       ((symbol-function 'redisplay)
                        (lambda (&optional force)
                          (setq redisplayed force)
                          (funcall original-redisplay force)))
                       ((symbol-function 'ghostel-force-redraw)
                        (lambda ()
                          (setq redrawn t)
                          (funcall original-force-redraw))))
               (with-current-buffer
                   (ghostel-viewport-editor-integration-test--viewport source)
                 (ghostel-viewport-editor-cancel)))
             (should redrawn))
           (should (eq (window-buffer (selected-window)) source))
           (should
            (= (window-point (selected-window))
               (with-current-buffer source (ghostel-cursor-point))))
           (should (memq (selected-window) forced-windows))
           (should redisplayed))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-finish-refreshes-restored-source ()
  "Finishing a viewport should repaint after its terminal application returns."
  (skip-unless (ghostel-viewport-editor--supported-host-p))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-finish-refresh-" t))
          (target (expand-file-name "prompt.md" work-directory))
          (source (generate-new-buffer " *ghostel-viewport-finish-refresh*"))
          redrawn
          resumed
          redrawn-after-resume
          process)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (ghostel-viewport-editor-global-mode 1)
           (setq process
                 (ghostel-exec
                  source "/bin/sh"
                  (list "-c"
                        "exec ${EDITOR} \"$1\""
                        "viewport-finish-refresh" target)))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (ghostel-viewport-editor-integration-test--viewport source))))
           (let ((original-force-redraw
                  (symbol-function 'ghostel-force-redraw)))
             (cl-letf (((symbol-function 'ghostel-force-redraw)
                        (lambda ()
                          (if resumed
                              (setq redrawn-after-resume t)
                            (setq redrawn t))
                          (funcall original-force-redraw))))
               (with-current-buffer
                   (ghostel-viewport-editor-integration-test--viewport source)
                 (insert "edited")
                 (ghostel-viewport-editor-finish))
               (should redrawn)
               (setq resumed t)
               (should (eq (window-buffer (selected-window)) source))
               (should
                (ghostel-viewport-editor-integration-test--wait-until
                 (lambda () (not (process-live-p process)))))
               (should
                (ghostel-viewport-editor-integration-test--wait-until
                 (lambda () redrawn-after-resume) 1)))))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-repeated-cancel-restores-output ()
  "Repeated empty viewport cancellations should keep Ghostel output visible."
  (skip-unless (ghostel-viewport-editor--supported-host-p))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-repeat-cancel-" t))
          (target (expand-file-name "prompt.md" work-directory))
          (source (generate-new-buffer " *ghostel-viewport-repeat-cancel*"))
          process)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (ghostel-viewport-editor-global-mode 1)
           (setq process
                 (ghostel-exec
                  source "/bin/sh"
                  (list
                   "-c"
                   (concat
                    "i=1; while [ $i -le 3 ]; do "
                    "$EDITOR \"$1\"; printf 'resumed-%s\\n' \"$i\"; "
                    "i=$((i + 1)); done; sleep 1")
                   "viewport-repeat-cancel" target)))
           (dotimes (_ 3)
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (ghostel-viewport-editor-integration-test--viewport
                  source))))
             (with-current-buffer
                 (ghostel-viewport-editor-integration-test--viewport source)
               (call-interactively (key-binding (kbd "C-c C-k")))))
           (should (eq (window-buffer (selected-window)) source))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (with-current-buffer source
                 (string-match-p "resumed-3" (buffer-string))))))
           (should (get-buffer-window source (selected-frame))))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-dtach-reannounces-after-attach ()
  "A helper started while detached should present after Ghostel attaches."
  (skip-unless (and (ghostel-viewport-editor--supported-host-p)
                    (executable-find "dtach")))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-dtach-" t))
          (socket-directory
           (make-temp-file "ghostel-viewport-socket-" t))
          (socket (expand-file-name "session" socket-directory))
          (pid-file (expand-file-name "child.pid" socket-directory))
          (target (expand-file-name "prompt.md" work-directory))
          (fallback-pair
           (ghostel-viewport-editor-integration-test--make-fallback
            work-directory))
          (fallback (car fallback-pair))
          (fallback-marker (cdr fallback-pair))
          (source (generate-new-buffer " *ghostel-viewport-dtach*"))
          (base-environment
           (append
            (list (concat "EDITOR=" fallback)
                  (concat "VISUAL=" fallback))
            (ghostel-viewport-editor-integration-test--without-env
             '("EDITOR" "VISUAL") process-environment)))
          attach-process)
     (unwind-protect
         (save-window-excursion
           (ghostel-viewport-editor-global-mode 1)
           (let ((process-environment
                  (ghostel-viewport-editor-environment
                   base-environment work-directory)))
             (should
              (zerop
               (call-process
                (executable-find "dtach") nil nil nil
                "-n" socket "-E" "/bin/sh" "-c"
                (concat
                 "printf '%s\\n' \"$$\" > \"$1\"; "
                 "shift; exec $EDITOR \"$1\"")
                "viewport-dtach" pid-file target))))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (and (file-exists-p socket)
                    (file-exists-p pid-file)
                    (directory-files
                     ghostel-viewport-editor--response-directory
                     nil "\\`request\\.")))))
           ;; Prove the waiting request does not time out while detached.
           (accept-process-output nil 0.35)
           (should-not (file-exists-p fallback-marker))
           (switch-to-buffer source)
           (setq attach-process
                 (ghostel-exec
                  source (executable-find "dtach")
                  (list "-a" socket "-E" "-r" "winch")))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (ghostel-viewport-editor-integration-test--viewport
                source))))
           (with-current-buffer
               (ghostel-viewport-editor-integration-test--viewport source)
             (erase-buffer)
             (insert "after attach\n")
             (ghostel-viewport-editor-finish))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda () (not (process-live-p attach-process)))))
           (should-not (file-exists-p fallback-marker))
           (with-temp-buffer
             (insert-file-contents target)
             (should (equal (buffer-string) "after attach\n"))))
       (when (process-live-p attach-process)
         (delete-process attach-process))
       (ghostel-viewport-editor-integration-test--kill-dtach pid-file)
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)
       (delete-directory socket-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-explicit-existing-dtach-enable ()
  "An existing zsh under dtach should be enabled only by explicit command."
  (skip-unless (and (ghostel-viewport-editor--supported-host-p)
                    (executable-find "dtach")
                    (executable-find "zsh")))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-existing-" t))
          (socket-directory
           (make-temp-file "ghostel-viewport-existing-socket-" t))
          (zdot-directory
           (make-temp-file "ghostel-viewport-existing-zdot-" t))
          (socket (expand-file-name "session" socket-directory))
          (pid-file (expand-file-name "shell.pid" socket-directory))
          (fallback-pair
           (ghostel-viewport-editor-integration-test--make-fallback
            work-directory))
          (fallback (car fallback-pair))
          (fallback-marker (cdr fallback-pair))
          (source (generate-new-buffer " *ghostel-viewport-existing*"))
          (process-environment
           (append
            (list (concat "EDITOR=" fallback)
                  (concat "VISUAL=" fallback)
                  (concat "ZDOTDIR=" zdot-directory))
            (ghostel-viewport-editor-integration-test--without-env
             '("EDITOR" "VISUAL" "ZDOTDIR"
               "GHOSTEL_VIEWPORT_EDITOR_TOKEN")
             process-environment)))
          attach-process)
     (unwind-protect
         (save-window-excursion
           (with-temp-file (expand-file-name ".zshenv" zdot-directory)
             (insert "skip_global_compinit=1\n"))
           (with-temp-file (expand-file-name ".zshrc" zdot-directory)
             (insert
              "PROMPT='GVE> '\n"
              "autoload -Uz edit-command-line\n"
              "zle -N edit-command-line\n"
              "bindkey '^X^E' edit-command-line\n"
              "print -r -- $$ > " (shell-quote-argument pid-file) "\n"))
           ;; This server intentionally predates package activation.
           (should
            (zerop
             (call-process
              (executable-find "dtach") nil nil nil
              "-n" socket "-E" (executable-find "zsh") "-i")))
           (should
            (ghostel-viewport-editor-integration-test--wait-until
             (lambda ()
               (and (file-exists-p socket) (file-exists-p pid-file)))))
           (ghostel-viewport-editor-global-mode 1)
           (switch-to-buffer source)
           (setq attach-process
                 (ghostel-exec
                  source (executable-find "dtach")
                  (list "-a" socket "-E" "-r" "winch")))
           ;; dtach does not replay the prompt printed while detached.
           (with-current-buffer source
             (ghostel-send-string "\r"))
           (ert-info ("waiting for existing zsh prompt")
             (unless
                 (ghostel-viewport-editor-integration-test--wait-until
                  (lambda ()
                    (with-current-buffer source
                      (string-match-p "GVE> " (buffer-string)))))
               (ert-fail
                (format "source=%S process=%S live=%S status=%S"
                        (with-current-buffer source (buffer-string))
                        attach-process
                        (process-live-p attach-process)
                        (process-status attach-process)))))
           (with-current-buffer source
             (cl-letf (((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) t)))
               (ghostel-viewport-editor-enable-current-shell)))
           (ert-info ("waiting for prompt after activation")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (>= (how-many "GVE> " (point-min) (point-max)) 2))))))
           (with-current-buffer source
             (should-not
              (string-match-p "if eval\\|eval \\\"\\$("
                              (buffer-string)))
             (ghostel-send-string "echo before\C-x\C-e"))
           (ert-info ("waiting for viewport after explicit activation")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (ghostel-viewport-editor-integration-test--viewport
                  source)))))
           (with-current-buffer
               (ghostel-viewport-editor-integration-test--viewport source)
             (erase-buffer)
             (insert "echo after\n")
             (ghostel-viewport-editor-finish))
           (should-not (file-exists-p fallback-marker))
           (ert-info ("waiting for prompt after explicit activation return")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (when-let* ((cursor (ghostel-cursor-point)))
                     (save-excursion
                       (goto-char cursor)
                       (equal
                        (buffer-substring-no-properties
                         (line-beginning-position) cursor)
                        "GVE> echo after"))))))))
           (with-current-buffer source
             (should (= (point) (ghostel-cursor-point)))))
       (when (process-live-p attach-process)
         (delete-process attach-process))
       (ghostel-viewport-editor-integration-test--kill-dtach pid-file)
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)
       (delete-directory socket-directory t)
       (delete-directory zdot-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-dtach-survives-emacs-restart ()
  "A routed dtach zsh should reconnect through the stable dispatcher."
  (skip-unless (and (ghostel-viewport-editor--supported-host-p)
                    (executable-find "dtach")
                    (executable-find "zsh")))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-restart-" t))
          (socket-directory
           (make-temp-file "ghostel-viewport-restart-socket-" t))
          (zdot-directory
           (make-temp-file "ghostel-viewport-restart-zdot-" t))
          (socket (expand-file-name "session" socket-directory))
          (pid-file (expand-file-name "shell.pid" socket-directory))
          (fallback-pair
           (ghostel-viewport-editor-integration-test--make-fallback
            work-directory))
          (fallback (car fallback-pair))
          (fallback-marker (cdr fallback-pair))
          (source (generate-new-buffer " *ghostel-viewport-restart*"))
          (base-environment
           (append
            (list (concat "EDITOR=" fallback)
                  (concat "VISUAL=" fallback)
                  (concat "ZDOTDIR=" zdot-directory))
            (ghostel-viewport-editor-integration-test--without-env
             '("EDITOR" "VISUAL" "ZDOTDIR"
               "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
               "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY")
             process-environment)))
          old-environment
          old-activation
          old-helper
          old-instance
          old-token
          attach-process)
     (unwind-protect
         (save-window-excursion
           (with-temp-file (expand-file-name ".zshenv" zdot-directory)
             (insert "skip_global_compinit=1\n"))
           (with-temp-file (expand-file-name ".zshrc" zdot-directory)
             (insert
              "PROMPT='GVE> '\n"
              "autoload -Uz edit-command-line\n"
              "zle -N edit-command-line\n"
              "bindkey '^X^E' edit-command-line\n"
              "print -r -- $$ > " (shell-quote-argument pid-file) "\n"))
           (ghostel-viewport-editor-global-mode 1)
           (setq old-environment
                 (ghostel-viewport-editor-environment
                  base-environment work-directory)
                 old-activation ghostel-viewport-editor--activation-file
                 old-helper ghostel-viewport-editor--helper-file
                 old-instance (ghostel-viewport-editor--instance-id)
                 old-token (ghostel-viewport-editor--token))
           (should
            (equal old-helper
                   (expand-file-name
                    "editor" ghostel-viewport-editor-state-directory)))
           (should
            (equal old-activation
                   (expand-file-name
                    "activate.sh" ghostel-viewport-editor-state-directory)))
           (let ((process-environment old-environment))
             (should
              (zerop
               (call-process
                (executable-find "dtach") nil nil nil
                "-n" socket "-E" (executable-find "zsh") "-i"))))
           (ert-info ("waiting for old persistent zsh")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (and (file-exists-p socket) (file-exists-p pid-file))))))
           ;; The persistent shell keeps its original environment while the
           ;; in-memory state is replaced as it would be by a new Emacs.
           (ghostel-viewport-editor-global-mode -1)
           (setq ghostel-viewport-editor--token nil
                 ghostel-viewport-editor--token-state-root nil
                 ghostel-viewport-editor--instance-id nil
                 ghostel-viewport-editor--helper-file nil
                 ghostel-viewport-editor--activation-file nil
                 ghostel-viewport-editor--response-directory nil
                 ghostel-viewport-editor--requests
                 (make-hash-table :test #'equal)
                 ghostel-eval-cmds
                 (cl-remove-if
                  (lambda (entry)
                    (equal (car-safe entry)
                           ghostel-viewport-editor--command))
                  ghostel-eval-cmds))
           (ghostel-viewport-editor-global-mode 1)
           (should (equal (ghostel-viewport-editor--token) old-token))
           (should-not (equal (ghostel-viewport-editor--instance-id)
                              old-instance))
           (should (equal ghostel-viewport-editor--helper-file old-helper))
           (should (equal ghostel-viewport-editor--activation-file
                          old-activation))
           (should (file-executable-p old-helper))
           (switch-to-buffer source)
           (setq attach-process
                 (ghostel-exec
                  source (executable-find "dtach")
                  (list "-a" socket "-E" "-r" "winch")))
           ;; dtach does not replay the prompt printed while detached.
           (with-current-buffer source
             (ghostel-send-string "\r"))
           (ert-info ("waiting for restored zsh prompt")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (string-match-p "GVE> " (buffer-string)))))))
           ;; No call to `ghostel-viewport-editor-enable-current-shell' is
           ;; allowed here: the persistent stable helper must route it.
           (with-current-buffer source
             (ghostel-send-string "echo before\C-x\C-e"))
           (ert-info ("waiting for viewport after simulated restart")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (or (file-exists-p fallback-marker)
                     (ghostel-viewport-editor-integration-test--viewport
                      source))))))
           (should-not (file-exists-p fallback-marker))
           (with-current-buffer
               (ghostel-viewport-editor-integration-test--viewport source)
             (erase-buffer)
             (insert "echo after\n")
             (ghostel-viewport-editor-finish))
           (ert-info ("waiting for the restored helper to finish")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (null
                  (directory-files
                   ghostel-viewport-editor--response-directory
                   nil "\\`request\\."))))))
           (should-not (file-exists-p fallback-marker)))
       (when (process-live-p attach-process)
         (delete-process attach-process))
       (ghostel-viewport-editor-integration-test--kill-dtach pid-file)
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)
       (delete-directory socket-directory t)
       (delete-directory zdot-directory t)))))

(ert-deftest
    ghostel-viewport-editor-integration-test-zshrc-override-is-reapplied ()
  "Fresh zsh routing should be installed after the user's zshrc editor."
  (skip-unless (and (ghostel-viewport-editor--supported-host-p)
                    (executable-find "dtach")
                    (executable-find "zsh")))
  (ghostel-viewport-editor-integration-test--with-runtime
   (let* ((work-directory
           (make-temp-file "ghostel-viewport-zsh-" t))
          (socket-directory
           (make-temp-file "ghostel-viewport-zsh-socket-" t))
          (zdot-directory
           (make-temp-file "ghostel-viewport-zsh-zdot-" t))
          (socket (expand-file-name "session" socket-directory))
          (pid-file (expand-file-name "shell.pid" socket-directory))
          (fallback-pair
           (ghostel-viewport-editor-integration-test--make-fallback
            work-directory))
          (fallback (car fallback-pair))
          (fallback-marker (cdr fallback-pair))
          (source (generate-new-buffer " *ghostel-viewport-zsh*"))
          (root (ghostel--resource-root))
          (bootstrap
           (expand-file-name "etc/shell/bootstrap/zsh/" root))
          (process-environment
           (append
            (list (concat "EDITOR=" fallback)
                  (concat "VISUAL=" fallback)
                  (concat "SHELL=" (executable-find "zsh"))
                  (concat "EMACS_GHOSTEL_PATH=" root)
                  (concat "GHOSTEL_ZSH_ZDOTDIR=" zdot-directory)
                  (concat "ZDOTDIR=" bootstrap))
            (ghostel-viewport-editor-integration-test--without-env
             '("EDITOR" "VISUAL" "SHELL" "EMACS_GHOSTEL_PATH"
               "GHOSTEL_ZSH_ZDOTDIR" "ZDOTDIR")
             process-environment)))
          attach-process)
     (unwind-protect
         (save-window-excursion
           (with-temp-file (expand-file-name ".zshenv" zdot-directory)
             (insert "skip_global_compinit=1\n"))
           (with-temp-file (expand-file-name ".zshrc" zdot-directory)
             (insert
              "PROMPT='GVE> '\n"
              "export EDITOR=" (shell-quote-argument fallback) "\n"
              "export VISUAL=" (shell-quote-argument fallback) "\n"
              "autoload -Uz edit-command-line\n"
              "zle -N edit-command-line\n"
              "bindkey '^X^E' edit-command-line\n"
              "print -r -- $$ > " (shell-quote-argument pid-file) "\n"))
           (ghostel-viewport-editor-global-mode 1)
           (let ((process-environment (copy-sequence process-environment)))
             (run-hooks 'ghostel-pre-spawn-hook)
             (should
              (zerop
               (call-process
                (executable-find "dtach") nil nil nil
                "-n" socket "-E" (executable-find "zsh") "-i"))))
           (ert-info ("waiting for fresh zsh server")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (and (file-exists-p socket) (file-exists-p pid-file))))))
           (switch-to-buffer source)
           (setq attach-process
                 (ghostel-exec
                  source (executable-find "dtach")
                  (list "-a" socket "-E" "-r" "winch")))
           (with-current-buffer source
             (ghostel-send-string "\r"))
           (ert-info ("waiting for fresh zsh prompt")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (string-match-p "GVE> " (buffer-string)))))))
           (with-current-buffer source
             (ghostel-send-string "printf 'line-%02d\\n' {1..30}\r"))
           (ert-info ("waiting for scrollback before fresh-zsh editor")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (and (string-match-p "line-30" (buffer-string))
                        (>= (how-many "GVE> " (point-min) (point-max))
                            2)))))))
           (with-current-buffer source
             (ghostel-send-string
              "for i in *; do echo \"$i\"; done\C-x\C-e"))
           (ert-info ("waiting for routed fresh-zsh editor")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (or (file-exists-p fallback-marker)
                     (ghostel-viewport-editor-integration-test--viewport
                      source))))))
           (should-not (file-exists-p fallback-marker))
           (with-current-buffer
               (ghostel-viewport-editor-integration-test--viewport source)
             (should (> (buffer-size) 0))
             (should (= (point) (point-max)))
             (erase-buffer)
             (insert "for i in *;\n")
             (ghostel-viewport-editor-finish))
           (with-current-buffer source
             (should (integer-or-marker-p (ghostel-cursor-point)))
             (should (= (point) (ghostel-cursor-point))))
           (ert-info ("waiting for edited command and prompt after return")
             (should
              (ghostel-viewport-editor-integration-test--wait-until
               (lambda ()
                 (with-current-buffer source
                   (when-let* ((cursor (ghostel-cursor-point)))
                     (save-excursion
                       (goto-char cursor)
                       (equal
                        (buffer-substring-no-properties
                         (line-beginning-position) cursor)
                        "GVE> for i in *;"))))))))
           (with-current-buffer source
             (should (= (point) (ghostel-cursor-point)))))
       (when (process-live-p attach-process)
         (delete-process attach-process))
       (ghostel-viewport-editor-integration-test--kill-dtach pid-file)
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory work-directory t)
       (delete-directory socket-directory t)
       (delete-directory zdot-directory t)))))

(provide 'ghostel-viewport-editor-integration-test)
;;; ghostel-viewport-editor-integration-test.el ends here
