;;; ghostel-viewport-editor-test.el --- Tests for viewport editor -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests exercise observable package behavior.  A few transport tests
;; cross private seams to validate the generated helper and response channel.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ghostel-viewport-editor)

(defun ghostel-viewport-editor-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 5)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.03))
    value))

(defun ghostel-viewport-editor-test--response-files ()
  "Return helper response files belonging to the current test runtime."
  (when (file-directory-p ghostel-viewport-editor--response-directory)
    (directory-files ghostel-viewport-editor--response-directory t
                     "\\`request\\.")))

(defun ghostel-viewport-editor-test--make-response ()
  "Create and return one valid empty response file."
  (let ((file
         (make-temp-file
          (expand-file-name
           "request." ghostel-viewport-editor--response-directory))))
    (set-file-modes file #o600)
    file))

(defun ghostel-viewport-editor-test--file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ghostel-viewport-editor-test--kill-viewports ()
  "Kill viewport buffers without prompting."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p ghostel-viewport-editor-mode)
        (setq ghostel-viewport-editor--suppress-kill-query t)
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defmacro ghostel-viewport-editor-test--with-runtime (&rest body)
  "Run BODY with isolated generated files and request state."
  (declare (indent 0) (debug t))
  `(let* ((test-state-directory
           (make-temp-file "ghostel-viewport-editor-test-" t))
          (ghostel-viewport-editor-state-directory
           (file-name-as-directory test-state-directory))
          (ghostel-viewport-editor--token nil)
          (ghostel-viewport-editor--token-state-root nil)
          (ghostel-viewport-editor--instance-id (make-string 64 ?a))
          (ghostel-viewport-editor--helper-file nil)
          (ghostel-viewport-editor--activation-file nil)
          (ghostel-viewport-editor--response-directory nil)
          (ghostel-viewport-editor--requests
           (make-hash-table :test #'equal))
          (ghostel-viewport-editor-global-mode nil)
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn
           (ghostel-viewport-editor--ensure-runtime)
           ,@body)
       (ghostel-viewport-editor-test--kill-viewports)
       (when (file-directory-p test-state-directory)
         (delete-directory test-state-directory t)))))

(ert-deftest ghostel-viewport-editor-test-generates-small-valid-support-files ()
  "Generated helper and activation files should be private and valid."
  (ghostel-viewport-editor-test--with-runtime
   (let ((token (ghostel-viewport-editor--token))
         (file (expand-file-name "token"
                                 ghostel-viewport-editor-state-directory)))
     (should (= (file-modes file) #o600))
     (setq ghostel-viewport-editor--token nil)
     (should (equal (ghostel-viewport-editor--token) token)))
   (should (file-executable-p ghostel-viewport-editor--helper-file))
   (should (= (file-modes ghostel-viewport-editor--helper-file) #o700))
   (should (= (file-modes ghostel-viewport-editor--activation-file) #o600))
   (should
    (zerop
     (call-process "/bin/sh" nil nil nil
                   "-n" ghostel-viewport-editor--helper-file)))
   (let ((activation
          (ghostel-viewport-editor-test--file-string
           ghostel-viewport-editor--activation-file)))
     (should-not (string-match-p "\\beval\\b" activation))
     (should (string-match-p "GHOSTEL_VIEWPORT_EDITOR_TOKEN" activation))
     (should (string-match-p "--kind EDITOR" activation))
     (should (string-match-p "zle reset-prompt" activation)))))

(ert-deftest ghostel-viewport-editor-test-state-switch-uses-new-token ()
  "Changing the state directory should switch all generated support."
  (ghostel-viewport-editor-test--with-runtime
   (let ((first-token (ghostel-viewport-editor--token))
         (second-directory
          (file-name-as-directory
           (make-temp-file "ghostel-viewport-editor-state-" t))))
     (unwind-protect
         (progn
           (setq ghostel-viewport-editor-state-directory second-directory)
           (ghostel-viewport-editor--ensure-runtime)
           (should
            (file-exists-p (expand-file-name "token" second-directory)))
           (should-not
            (equal first-token (ghostel-viewport-editor--token)))
           (should
            (file-in-directory-p ghostel-viewport-editor--helper-file
                                 second-directory)))
       (delete-directory second-directory t)))))

(ert-deftest ghostel-viewport-editor-test-environment-is-scoped-and-idempotent ()
  "Environment transformation should preserve its input and exact fallback."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((input '("EDITOR=emacs -nw" "VISUAL=vim" "KEEP=value"))
          (input-copy (copy-sequence input))
          (outside (copy-sequence process-environment))
          (default-directory temporary-file-directory)
          transformed reinjected)
     (cl-letf (((symbol-function
                 'ghostel-viewport-editor--ensure-supported)
                #'ghostel-viewport-editor--ensure-runtime))
       (setq transformed
             (ghostel-viewport-editor-environment input default-directory)
             reinjected
             (ghostel-viewport-editor-environment
              transformed default-directory)))
     (should (equal input input-copy))
     (should (equal process-environment outside))
     (should (equal (getenv-internal "KEEP" transformed) "value"))
     (should
      (string-match-p
       (regexp-quote ghostel-viewport-editor--helper-file)
       (getenv-internal "EDITOR" transformed)))
     (should
      (equal
       (getenv-internal "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR"
                        transformed)
       "emacs -nw"))
     (should
      (equal
       (getenv-internal "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR"
                        reinjected)
       "emacs -nw"))
     (dolist (name ghostel-viewport-editor--editor-variables)
       (should (getenv-internal name transformed))))))

(ert-deftest ghostel-viewport-editor-test-remote-environment-is-unchanged ()
  "Remote process environments should not receive local helper paths."
  (ghostel-viewport-editor-test--with-runtime
   (let ((input '("EDITOR=vim" "KEEP=value")))
     (cl-letf (((symbol-function
                 'ghostel-viewport-editor--ensure-supported)
                #'ghostel-viewport-editor--ensure-runtime))
       (should
        (equal
         (ghostel-viewport-editor-environment input "/ssh:host:/tmp/")
         input))))))

(ert-deftest ghostel-viewport-editor-test-manual-shell-enable-sources-file ()
  "Manual activation should source a file and never inject eval."
  (ghostel-viewport-editor-test--with-runtime
   (let ((ghostel-viewport-editor-global-mode t)
         sent)
     (cl-letf (((symbol-function 'derived-mode-p)
                (lambda (&rest _) t))
               ((symbol-function 'file-remote-p)
                (lambda (&rest _) nil))
               ((symbol-function
                 'ghostel-viewport-editor--ensure-supported)
                #'ghostel-viewport-editor--ensure-runtime)
               ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'ghostel-send-string)
                (lambda (string) (setq sent string))))
       (ghostel-viewport-editor-enable-current-shell))
     (should (string-prefix-p ". '" sent))
     (should (string-suffix-p "\r" sent))
     (should-not (string-match-p "\\beval\\b" sent)))))

(ert-deftest ghostel-viewport-editor-test-trigger-forwards-control-sequence ()
  "The source command should send C-x C-e through Ghostel's public function."
  (let ((ghostel-viewport-editor-global-mode t)
        sent)
    (cl-letf (((symbol-function 'ghostel-send-string)
               (lambda (string) (setq sent string))))
      (ghostel-viewport-editor-trigger))
    (should (equal sent "\C-x\C-e"))))

(ert-deftest ghostel-viewport-editor-test-active-source-rejects-focus-and-input ()
  "An active editor should make its visible source window noninteractive."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-source-lock-" t))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-source-lock*"))
          (original-map (make-sparse-keymap))
          source-window viewport-window request viewport)
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer source)
           (setq-local overriding-local-map original-map)
           (setq source-window (selected-window))
           (set-window-parameter source-window 'no-other-window 'preserve)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.txt")))
           (setq request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source)
                 viewport
                 (ghostel-viewport-editor--request-viewport-buffer request)
                 viewport-window (get-buffer-window viewport))
           (should (window-live-p viewport-window))
           (should (eq (selected-window) viewport-window))
           (should (eq (window-parameter source-window 'no-other-window) t))
           (with-current-buffer source
             (should ghostel-viewport-editor--source-input-locked)
             (should (eq overriding-local-map
                         ghostel-viewport-editor--source-lock-map))
             (should (eq (key-binding (kbd "a"))
                         #'ghostel-viewport-editor--reject-source-input))
             (should (eq (key-binding [down-mouse-1])
                         #'ghostel-viewport-editor--reject-source-input))
             (call-interactively (key-binding (kbd "a"))))
           (should (eq (selected-window) viewport-window))
           (other-window 1)
           (should (eq (selected-window) viewport-window))
           (select-window source-window)
           (with-current-buffer source
             (ghostel-viewport-editor--source-post-command))
           (should (eq (selected-window) viewport-window))
           (with-current-buffer viewport
             (ghostel-viewport-editor-cancel))
           (should (eq (selected-window) source-window))
           (should
            (eq (window-parameter source-window 'no-other-window) 'preserve))
           (with-current-buffer source
             (should-not ghostel-viewport-editor--source-input-locked)
             (should (eq overriding-local-map original-map))
             (should-not
              (memq #'ghostel-viewport-editor--source-post-command
                    post-command-hook))))
       (when (buffer-live-p viewport)
         (with-current-buffer viewport
           (setq ghostel-viewport-editor--suppress-kill-query t)
           (set-buffer-modified-p nil))
         (kill-buffer viewport))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-c-g-keeps-keyboard-quit ()
  "C-g should retain its standard meaning inside viewport buffers."
  (with-temp-buffer
    (ghostel-viewport-editor-mode 1)
    (should (eq (key-binding (kbd "C-g")) #'keyboard-quit))
    (should (eq (key-binding (kbd "C-c C-k"))
                #'ghostel-viewport-editor-cancel))))

(ert-deftest ghostel-viewport-editor-test-backspace-deletes-active-region ()
  "Viewport Backspace should edit text even when View mode was active."
  (with-temp-buffer
    (insert "replace me")
    (view-mode 1)
    (ghostel-viewport-editor-mode 1)
    (should-not view-mode)
    (should-not buffer-read-only)
    (should (eq (key-binding (kbd "DEL"))
                #'ghostel-viewport-editor-backward-delete))
    (let ((transient-mark-mode t))
      (goto-char (point-max))
      (set-mark (point-min))
      (activate-mark)
      (call-interactively (key-binding (kbd "DEL"))))
    (should (string-empty-p (buffer-string)))))

(ert-deftest ghostel-viewport-editor-test-major-mode-change-keeps-request ()
  "Changing major mode should not orphan an accepted editor request."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-major-mode-" t))
          (file (expand-file-name "prompt.txt" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-major-mode*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          request viewport)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.txt")))
           (setq request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source)
                 viewport
                 (ghostel-viewport-editor--request-viewport-buffer request))
           (with-current-buffer viewport
             (should-error (ghostel-viewport-editor-mode -1)
                           :type 'user-error)
             (should ghostel-viewport-editor-mode)
             (text-mode)
             (should ghostel-viewport-editor-mode)
             (should (eq ghostel-viewport-editor--request request))
             (should (memq #'ghostel-viewport-editor--kill-query
                           kill-buffer-query-functions))
             (should (memq #'ghostel-viewport-editor--viewport-killed
                           kill-buffer-hook))
             (should (equal header-line-format
                            ghostel-viewport-editor--header-line-format))
             (ghostel-viewport-editor-cancel))
           (should-not (buffer-live-p viewport))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "done\n")))
       (when (buffer-live-p viewport)
         (with-current-buffer viewport
           (setq ghostel-viewport-editor--suppress-kill-query t)
           (set-buffer-modified-p nil))
         (kill-buffer viewport))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-global-mode-installs-and-uninstalls ()
  "Global mode should install and fully remove its Ghostel adapters."
  (let ((ghostel-viewport-editor-global-mode nil)
        (pre-spawn-bound (boundp 'ghostel-pre-spawn-hook))
        (mode-hook-bound (boundp 'ghostel-mode-hook))
        (old-pre-spawn (and (boundp 'ghostel-pre-spawn-hook)
                            (default-value 'ghostel-pre-spawn-hook)))
        (old-mode-hook (and (boundp 'ghostel-mode-hook)
                            (default-value 'ghostel-mode-hook))))
    (unwind-protect
        (progn
          (set-default 'ghostel-pre-spawn-hook nil)
          (set-default 'ghostel-mode-hook nil)
          (with-temp-buffer
            (setq major-mode 'ghostel-mode)
            (cl-letf (((symbol-function
                        'ghostel-viewport-editor--ensure-supported)
                       #'ignore))
              (ghostel-viewport-editor-global-mode 1)
              (should ghostel-viewport-editor-global-mode)
              (should ghostel-viewport-editor-source-mode)
              (should
               (memq #'ghostel-viewport-editor--inject-environment
                     (default-value 'ghostel-pre-spawn-hook)))
              (should
               (memq #'ghostel-viewport-editor--setup-source-buffer
                     (default-value 'ghostel-mode-hook)))
              (ghostel-viewport-editor-global-mode -1))
            (should-not ghostel-viewport-editor-global-mode)
            (should-not ghostel-viewport-editor-source-mode)
            (should-not
             (memq #'ghostel-viewport-editor--inject-environment
                   (default-value 'ghostel-pre-spawn-hook)))
            (should-not
             (memq #'ghostel-viewport-editor--setup-source-buffer
                   (default-value 'ghostel-mode-hook)))))
      (if pre-spawn-bound
          (set-default 'ghostel-pre-spawn-hook old-pre-spawn)
        (makunbound 'ghostel-pre-spawn-hook))
      (if mode-hook-bound
          (set-default 'ghostel-mode-hook old-mode-hook)
        (makunbound 'ghostel-mode-hook)))))

(ert-deftest ghostel-viewport-editor-test-global-mode-rolls-back-install-error ()
  "A failed global-mode install should leave no partial adapters behind."
  (let ((ghostel-viewport-editor-global-mode nil)
        (pre-spawn-bound (boundp 'ghostel-pre-spawn-hook))
        (mode-hook-bound (boundp 'ghostel-mode-hook))
        (old-pre-spawn (and (boundp 'ghostel-pre-spawn-hook)
                            (default-value 'ghostel-pre-spawn-hook)))
        (old-mode-hook (and (boundp 'ghostel-mode-hook)
                            (default-value 'ghostel-mode-hook))))
    (unwind-protect
        (progn
          (set-default 'ghostel-pre-spawn-hook nil)
          (set-default 'ghostel-mode-hook nil)
          (cl-letf (((symbol-function 'ghostel-viewport-editor--install)
                     (lambda ()
                       (add-hook
                        'ghostel-pre-spawn-hook
                        #'ghostel-viewport-editor--inject-environment)
                       (add-hook
                        'ghostel-mode-hook
                        #'ghostel-viewport-editor--setup-source-buffer)
                       (error "install failed"))))
            (should-error (ghostel-viewport-editor-global-mode 1)
                          :type 'error))
          (should-not ghostel-viewport-editor-global-mode)
          (should-not
           (memq #'ghostel-viewport-editor--inject-environment
                 (default-value 'ghostel-pre-spawn-hook)))
          (should-not
           (memq #'ghostel-viewport-editor--setup-source-buffer
                 (default-value 'ghostel-mode-hook))))
      (if pre-spawn-bound
          (set-default 'ghostel-pre-spawn-hook old-pre-spawn)
        (makunbound 'ghostel-pre-spawn-hook))
      (if mode-hook-bound
          (set-default 'ghostel-mode-hook old-mode-hook)
        (makunbound 'ghostel-mode-hook)))))

(ert-deftest ghostel-viewport-editor-test-trigger-reveals-active-viewport ()
  "The source command should reveal its existing accepted viewport."
  (let* ((ghostel-viewport-editor-global-mode t)
         (source (generate-new-buffer " *ghostel-viewport-source*"))
         (viewport (generate-new-buffer " *ghostel-viewport-active*"))
         (request
          (ghostel-viewport-editor--make-request
           :source-buffer source
           :viewport-buffer viewport
           :phase 'accepted))
         sent)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (with-current-buffer source
            (setq ghostel-viewport-editor--active-request request))
          (let ((ghostel-viewport-editor-display-function
                 #'ghostel-viewport-editor-display-same-window))
            (cl-letf (((symbol-function 'ghostel-send-string)
                       (lambda (string) (setq sent string))))
              (with-current-buffer source
                (ghostel-viewport-editor-trigger))))
          (should (eq (window-buffer (selected-window)) viewport))
          (should (eq (get-buffer-window viewport (selected-frame))
                      (selected-window)))
          (should-not sent))
      (kill-buffer source)
      (kill-buffer viewport))))

(ert-deftest ghostel-viewport-editor-test-parses-one-file-and-position ()
  "Only the documented local single-file syntax should be accepted."
  (let* ((directory (make-temp-file "ghostel-viewport-parse-" t))
         (file (expand-file-name "prompt.md" directory)))
    (unwind-protect
        (progn
          (should
           (equal
            (ghostel-viewport-editor--parse-file-arguments
             directory '("prompt.md"))
            (list file nil nil)))
          (should
           (equal
            (ghostel-viewport-editor--parse-file-arguments
             directory '("+12:3" "--" "prompt.md"))
            (list file 12 3)))
          (should-not
           (ghostel-viewport-editor--parse-file-arguments
            directory '("-f" "prompt.md")))
          (should-not
           (ghostel-viewport-editor--parse-file-arguments
            directory '("-f")))
          (should
           (equal
            (ghostel-viewport-editor--parse-file-arguments
             directory '("--" "-f"))
            (list (expand-file-name "-f" directory) nil nil)))
          (should-not
           (ghostel-viewport-editor--parse-file-arguments
            directory '("one" "two")))
          (should-not
           (ghostel-viewport-editor--parse-file-arguments
            directory '("+0" "prompt.md")))
          (should-not
           (ghostel-viewport-editor--parse-file-arguments
            directory (list directory))))
      (delete-directory directory t))))

(ert-deftest ghostel-viewport-editor-test-initial-point-prefers-text-end ()
  "Nonempty editors start at the end unless a position was requested."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-point-" t))
          (file (expand-file-name "prompt.md" directory))
          (first-response (ghostel-viewport-editor-test--make-response))
          (second-response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-initial-point*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          viewports)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (with-temp-file file
             (insert "one\ntwo\nthree\n"))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (ghostel-viewport-editor--receive-request
              source
              (list :response-file first-response :directory directory
                    :kind "EDITOR" :arguments '("prompt.md")))
             (let ((viewport
                    (ghostel-viewport-editor--request-viewport-buffer
                     (buffer-local-value
                      'ghostel-viewport-editor--active-request source))))
               (push viewport viewports)
               (with-current-buffer viewport
                 (should (= (point) (point-max)))
                 (ghostel-viewport-editor-cancel)))
             (ghostel-viewport-editor--receive-request
              source
              (list :response-file second-response :directory directory
                    :kind "EDITOR" :arguments '("+2:2" "prompt.md")))
             (let ((viewport
                    (ghostel-viewport-editor--request-viewport-buffer
                     (buffer-local-value
                      'ghostel-viewport-editor--active-request source))))
               (push viewport viewports)
               (with-current-buffer viewport
                 (should (= (line-number-at-pos) 2))
                 (should (= (current-column) 1))
                 (ghostel-viewport-editor-cancel)))))
       (dolist (viewport viewports)
         (when (buffer-live-p viewport)
           (with-current-buffer viewport
             (setq ghostel-viewport-editor--suppress-kill-query t)
             (set-buffer-modified-p nil))
           (kill-buffer viewport)))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-decodes-bounded-nul-payload ()
  "Transport decoding should preserve arguments and reject malformed input."
  (let* ((fields (list ghostel-viewport-editor--protocol-version
                       "/tmp/response" "/tmp" "EDITOR" "2"
                       "" "file"))
         (encoded
          (base64-encode-string
           (concat (mapconcat #'identity fields "\0") "\0") t))
         (decoded (ghostel-viewport-editor--decode-payload encoded)))
    (should (equal (plist-get decoded :arguments) '("" "file")))
    (should-not
     (ghostel-viewport-editor--decode-payload
      (base64-encode-string "3\0bad\0" t)))
    (should-not
     (ghostel-viewport-editor--decode-payload
      (make-string (1+ ghostel-viewport-editor--max-payload-size) ?A)))))

(ert-deftest ghostel-viewport-editor-test-osc-authenticates-before-scheduling ()
  "Only the state directory token should schedule request work."
  (let* ((fields (list ghostel-viewport-editor--protocol-version
                       "/tmp/r" "/tmp" "EDITOR" "1" "file"))
         (encoded
          (base64-encode-string
           (concat (mapconcat #'identity fields "\0") "\0") t))
         scheduled)
    (cl-letf (((symbol-function 'ghostel-viewport-editor--token)
               (lambda () "right-token"))
              ((symbol-function 'run-at-time)
               (lambda (&rest arguments)
                 (setq scheduled arguments))))
      (ghostel-viewport-editor--handle-osc "wrong-token" encoded)
      (should-not scheduled)
      (ghostel-viewport-editor--handle-osc "right-token" encoded)
      (should scheduled)
      (should (eq (nth 2 scheduled)
                  #'ghostel-viewport-editor--receive-request)))))

(ert-deftest ghostel-viewport-editor-test-response-write-is-contained ()
  "Only private regular response files should be atomically replaced."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((response (ghostel-viewport-editor-test--make-response))
          (outside (make-temp-file "ghostel-viewport-outside-"))
          (loose (ghostel-viewport-editor-test--make-response)))
     (unwind-protect
         (progn
           (ghostel-viewport-editor--write-response response 'accepted)
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   (format "accepted:%s\n"
                           (ghostel-viewport-editor--owner-identity))))
           (should-error
            (ghostel-viewport-editor--write-response outside 'done))
           (set-file-modes loose #o644)
           (should-error
            (ghostel-viewport-editor--write-response loose 'done)))
       (dolist (file (list outside loose))
         (when (file-exists-p file)
           (delete-file file)))))))

(ert-deftest ghostel-viewport-editor-test-claim-uses-instance-identity ()
  "A reused PID must not make another Emacs instance own a claim."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((response (ghostel-viewport-editor-test--make-response))
          (first-instance (make-string 64 ?a))
          (second-instance (make-string 64 ?b))
          (ghostel-viewport-editor--instance-id first-instance))
     (should (ghostel-viewport-editor--claim-response response))
     (should
      (string-match-p
       (format "\\`%s:[0-9a-f]\\{64\\}:.+\\'" (emacs-pid))
       (ghostel-viewport-editor--owner-identity)))
     (should
      (equal
       (ghostel-viewport-editor-test--file-string (concat response ".claim"))
       (concat (ghostel-viewport-editor--owner-identity) "\n")))
     (let ((ghostel-viewport-editor--instance-id second-instance))
       (should-not (ghostel-viewport-editor--claim-response response))))))

(ert-deftest ghostel-viewport-editor-test-rejected-request-runs-fallback-state ()
  "A predicate rejection should not open a viewport."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-reject-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-reject*"))
          (ghostel-viewport-editor-accept-function (lambda (_) nil)))
     (unwind-protect
         (progn
           (with-temp-file file (insert "before\n"))
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "fallback\n"))
           (should-not
            (buffer-local-value
             'ghostel-viewport-editor--active-request source)))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-finish-saves-and-releases ()
  "Finishing should survive save hooks and a retryable release failure."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-finish-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-finish*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          after-request)
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer source)
           (let ((ghostel-viewport-editor-after-finish-function
                  (lambda (request) (setq after-request request))))
             (ghostel-viewport-editor--receive-request
              source
              (list :response-file response :directory directory
                    :kind "EDITOR" :arguments '("prompt.md")))
             (let* ((request
                     (buffer-local-value
                      'ghostel-viewport-editor--active-request source))
                    (viewport
                     (ghostel-viewport-editor--request-viewport-buffer
                      request))
                    (base
                     (ghostel-viewport-editor--request-base-buffer request)))
               (should (buffer-live-p viewport))
               (should-not (eq viewport (find-buffer-visiting file)))
               (with-current-buffer viewport
                 (should
                  (equal header-line-format
                         ghostel-viewport-editor--header-line-format)))
               (should
                (equal
                 (ghostel-viewport-editor-test--file-string response)
                 (format "accepted:%s\n"
                         (ghostel-viewport-editor--owner-identity))))
               (with-current-buffer base
                 (add-hook 'before-save-hook
                           (lambda ()
                             (goto-char (point-max))
                             (insert "formatted\n"))
                           nil t))
               (set-file-modes response #o644)
               (with-current-buffer viewport
                 (erase-buffer)
                 (insert "after\n")
                 (should-error (ghostel-viewport-editor-finish)))
               (should (buffer-live-p viewport))
               (set-file-modes response #o600)
               (with-current-buffer viewport
                 (ghostel-viewport-editor-finish))
               (should-not (buffer-live-p viewport))))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "after\nformatted\n"))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "done\n"))
           (should (equal (plist-get after-request :file) file)))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-after-save-error-completes-save ()
  "An error after writing should not report an ambiguous failed finish."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-after-save-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-after-save*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          request viewport base)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "before\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (setq request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source)
                 viewport
                 (ghostel-viewport-editor--request-viewport-buffer request)
                 base
                 (ghostel-viewport-editor--request-base-buffer request))
           (with-current-buffer base
             (add-hook 'after-save-hook
                       (lambda () (error "after-save failed")) nil t))
           (with-current-buffer viewport
             (erase-buffer)
             (insert "after\n")
             (ghostel-viewport-editor-finish))
           (should-not (buffer-live-p viewport))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "after\n"))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "done\n")))
       (when (buffer-live-p viewport)
         (with-current-buffer viewport
           (setq ghostel-viewport-editor--suppress-kill-query t)
           (set-buffer-modified-p nil))
         (kill-buffer viewport))
       (when (buffer-live-p base)
         (with-current-buffer base (set-buffer-modified-p nil))
         (kill-buffer base))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-unchanged-finish-releases ()
  "Finishing unchanged text should release without requiring a disk write."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-unchanged-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-unchanged*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          viewport)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "unchanged\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (setq viewport
                 (ghostel-viewport-editor--request-viewport-buffer
                  (buffer-local-value
                   'ghostel-viewport-editor--active-request source)))
           (with-current-buffer viewport
             (ghostel-viewport-editor-finish))
           (should-not (buffer-live-p viewport))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "unchanged\n"))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "done\n")))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-source-kill-releases-request ()
  "Killing the source should fail its request without losing viewport text."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-source-kill-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-source-kill*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          request viewport base)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "before\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "VISUAL" :arguments '("prompt.md")))
           (setq request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source)
                 viewport
                 (ghostel-viewport-editor--request-viewport-buffer request)
                 base
                 (ghostel-viewport-editor--request-base-buffer request))
           (with-current-buffer viewport
             (goto-char (point-max))
             (insert "unsaved\n"))
           (kill-buffer source)
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "error\n"))
           (should (eq (ghostel-viewport-editor--request-phase request)
                       'complete))
           (should (eq (ghostel-viewport-editor--request-outcome request)
                       'error))
           (should (buffer-live-p viewport))
           (with-current-buffer viewport
             (should (equal (ghostel-viewport-editor--buffer-text)
                            "before\nunsaved\n")))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "before\n")))
       (when (buffer-live-p viewport)
         (with-current-buffer viewport
           (setq ghostel-viewport-editor--suppress-kill-query t)
           (set-buffer-modified-p nil))
         (kill-buffer viewport))
       (when (buffer-live-p base) (kill-buffer base))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-cancel-preserves-file ()
  "Canceling a changed viewport should confirm and leave disk unchanged."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-cancel-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-cancel*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          asked)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "before\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (let* ((request
                   (buffer-local-value
                    'ghostel-viewport-editor--active-request source))
                  (viewport
                   (ghostel-viewport-editor--request-viewport-buffer request)))
             (with-current-buffer viewport
               (goto-char (point-max))
               (insert "changed\n")
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) (setq asked t))))
                 (ghostel-viewport-editor-cancel)))
             (should asked)
             (should-not (buffer-live-p viewport)))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "before\n"))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   "done\n")))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-existing-buffer-and-conflicts ()
  "Use an unmodified backing buffer and keep a viewport after conflicts."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-conflict-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-conflict*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          base viewport)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "one\nbefore\nthree\n"))
           (setq base (find-file-noselect file))
           (with-current-buffer base
             (goto-char (point-min))
             (forward-line 1)
             (narrow-to-region (line-beginning-position)
                               (line-beginning-position 2)))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (setq viewport
                 (ghostel-viewport-editor--request-viewport-buffer
                  (buffer-local-value
                   'ghostel-viewport-editor--active-request source)))
           (should (buffer-live-p viewport))
           (should-not (eq viewport base))
           (with-current-buffer viewport
             (should (equal (buffer-string) "one\nbefore\nthree\n")))
           (with-current-buffer base
             (goto-char (point-max))
             (insert "outside change\n"))
           (with-current-buffer viewport
             (erase-buffer)
             (insert "viewport change\n")
             (should-error (ghostel-viewport-editor-finish)
                           :type 'user-error))
           (should (buffer-live-p viewport))
           (should
            (equal (ghostel-viewport-editor-test--file-string response)
                   (format "accepted:%s\n"
                           (ghostel-viewport-editor--owner-identity))))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "one\nbefore\nthree\n"))
           (with-current-buffer viewport
             (cl-letf (((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) t)))
               (ghostel-viewport-editor-cancel)))
           (let ((rejected (ghostel-viewport-editor-test--make-response)))
             (ghostel-viewport-editor--receive-request
              source
              (list :response-file rejected :directory directory
                    :kind "EDITOR" :arguments '("prompt.md")))
             (should
              (equal (ghostel-viewport-editor-test--file-string rejected)
                     "fallback\n"))))
       (when (buffer-live-p base)
         (with-current-buffer base (set-buffer-modified-p nil))
         (kill-buffer base))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-content-change-with-old-mtime-conflicts ()
  "Changed disk content should conflict even when metadata is restored."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-content-conflict-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-content-conflict*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          request viewport accepted-time)
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "first\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (setq request
                 (buffer-local-value
                  'ghostel-viewport-editor--active-request source)
                 viewport
                 (ghostel-viewport-editor--request-viewport-buffer request)
                 accepted-time
                 (file-attribute-modification-time
                  (file-attributes file 'integer)))
           (with-current-buffer viewport
             (erase-buffer)
             (insert "mine\n"))
           ;; Preserve inode, byte count, and accepted mtime while changing text.
           (write-region "other\n" nil file nil 'silent)
           (set-file-times file accepted-time)
           (with-current-buffer viewport
             (should-error (ghostel-viewport-editor-finish)
                           :type 'user-error))
           (should (buffer-live-p viewport))
           (should
            (equal (ghostel-viewport-editor-test--file-string file)
                   "other\n"))
           (with-current-buffer viewport
             (cl-letf (((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) t)))
               (ghostel-viewport-editor-cancel))))
       (when (buffer-live-p viewport)
         (with-current-buffer viewport
           (setq ghostel-viewport-editor--suppress-kill-query t)
           (set-buffer-modified-p nil))
         (kill-buffer viewport))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-release-failure-keeps-viewport ()
  "A response write failure must keep accepted edits available to retry."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-release-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-release*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window))
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "before\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (let* ((request
                   (buffer-local-value
                    'ghostel-viewport-editor--active-request source))
                  (viewport
                   (ghostel-viewport-editor--request-viewport-buffer request)))
             (with-current-buffer viewport
               (goto-char (point-max))
               (insert "changed\n"))
             (set-file-modes response #o644)
             (with-current-buffer viewport
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) t)))
                 (should-error (ghostel-viewport-editor-cancel))))
             (should (buffer-live-p viewport))
             (should (eq (ghostel-viewport-editor--request-phase request)
                         'accepted))
             (with-current-buffer viewport
               (should (buffer-modified-p)))
             (delete-file response)
             (with-current-buffer viewport
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) t)))
                 (ghostel-viewport-editor-cancel)))))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-symlink-retarget-keeps-viewport ()
  "Finishing must not write through a request path retargeted mid-edit."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-symlink-" t))
          (first (expand-file-name "first" directory))
          (second (expand-file-name "second" directory))
          (link (expand-file-name "link" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-symlink*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          base)
     (unwind-protect
         (save-window-excursion
           (with-temp-file first (insert "first\n"))
           (with-temp-file second (insert "second\n"))
           (make-symbolic-link first link)
           (setq base (find-file-noselect first))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("link")))
           (let* ((request
                   (buffer-local-value
                    'ghostel-viewport-editor--active-request source))
                  (viewport
                   (ghostel-viewport-editor--request-viewport-buffer request)))
             (delete-file link)
             (make-symbolic-link second link)
             (with-current-buffer viewport
               (erase-buffer)
               (insert "edited\n")
               (should-error (ghostel-viewport-editor-finish)
                             :type 'user-error))
             (should (buffer-live-p viewport))
             (should (equal (ghostel-viewport-editor-test--file-string first)
                            "first\n"))
             (should (equal (ghostel-viewport-editor-test--file-string second)
                            "second\n"))
             (with-current-buffer viewport
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) t)))
                 (ghostel-viewport-editor-cancel)))))
       (when (buffer-live-p base) (kill-buffer base))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-hard-link-alias-falls-back ()
  "A different hard-link path must not save through an existing buffer."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-hardlink-" t))
          (first (expand-file-name "first" directory))
          (alias (expand-file-name "alias" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-hardlink*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window)
          base)
     (unwind-protect
         (save-window-excursion
           (with-temp-file first (insert "before\n"))
           (add-name-to-file first alias)
           (setq base (find-file-noselect first))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("alias")))
           (should-not
            (buffer-local-value
             'ghostel-viewport-editor--active-request source))
           (should (equal (ghostel-viewport-editor-test--file-string response)
                          "fallback\n"))
           (should (equal (ghostel-viewport-editor-test--file-string first)
                          "before\n")))
       (when (buffer-live-p base) (kill-buffer base))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-nonempty-cancel-can-be-declined ()
  "Even an unchanged nonempty viewport should ask before cancellation."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-decline-" t))
          (file (expand-file-name "prompt.md" directory))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-viewport-decline*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window))
     (unwind-protect
         (save-window-excursion
           (with-temp-file file (insert "before\n"))
           (switch-to-buffer source)
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.md")))
           (let* ((request
                   (buffer-local-value
                    'ghostel-viewport-editor--active-request source))
                  (viewport
                   (ghostel-viewport-editor--request-viewport-buffer request)))
             (with-current-buffer viewport
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) nil)))
                 (ghostel-viewport-editor-cancel)))
             (should (buffer-live-p viewport))
             (with-current-buffer viewport
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) t)))
                 (ghostel-viewport-editor-cancel)))))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-adaptive-display-orders-directions ()
  "Adaptive display should try the long dimension, then the other one."
  (let ((buffer (generate-new-buffer " *ghostel-viewport-display*"))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'window-pixel-width)
                   (lambda (&rest _) 1000))
                  ((symbol-function 'window-pixel-height)
                   (lambda (&rest _) 500))
                  ((symbol-function
                    'ghostel-viewport-editor--display-in-direction)
                   (lambda (_buffer _anchor direction)
                     (push direction calls)
                     (and (eq direction 'below) (selected-window)))))
          (should
           (window-live-p
            (ghostel-viewport-editor-display-adaptively
             buffer (selected-window))))
          (should (equal (nreverse calls) '(right below))))
      (kill-buffer buffer))))

(ert-deftest ghostel-viewport-editor-test-directional-display-uses-half ()
  "Directional viewports should use half the source in either direction."
  (let ((below (generate-new-buffer " *ghostel-viewport-below*"))
        (right (generate-new-buffer " *ghostel-viewport-right*")))
    (unwind-protect
        (progn
          (save-window-excursion
            (delete-other-windows)
            (let* ((anchor (selected-window))
                   (height (window-total-height anchor))
                   (window
                    (ghostel-viewport-editor--display-in-direction
                     below anchor 'below)))
              (should (window-live-p window))
              (should (<= (abs (- (window-total-height window)
                                  (/ height 2)))
                          1))))
          (save-window-excursion
            (delete-other-windows)
            (let* ((anchor (selected-window))
                   (width (window-total-width anchor))
                   (window
                    (ghostel-viewport-editor--display-in-direction
                     right anchor 'right)))
              (should (window-live-p window))
              (should (<= (abs (- (window-total-width window)
                                  (/ width 2)))
                          1)))))
      (kill-buffer below)
      (kill-buffer right))))

(ert-deftest ghostel-viewport-editor-test-helper-completes-through-response ()
  "The real helper should exit promptly after an accepted request completes."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-helper-target-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-helper-output*"))
          process response)
     (unwind-protect
         (progn
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL" "1")
           (setenv "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR_SET" "0")
           (setq process
                 (make-process
                  :name "ghostel-viewport-helper"
                  :buffer output
                  :command (list ghostel-viewport-editor--helper-file
                                 "--kind" "EDITOR" target)
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (setq response
                     (car (ghostel-viewport-editor-test--response-files))))))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (with-current-buffer output
                 (string-match-p "52;e;ghostel-viewport-editor"
                                 (buffer-string))))))
           (should (ghostel-viewport-editor--claim-response response))
           (ghostel-viewport-editor--write-response response 'accepted)
           (accept-process-output process 1.1)
           (should (process-live-p process))
           (ghostel-viewport-editor--write-response response 'done)
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process))) 0.3))
           (should (zerop (process-exit-status process))))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (when (file-exists-p target) (delete-file target))))))

(ert-deftest ghostel-viewport-editor-test-helper-runs-exact-fallback ()
  "A fallback outcome should execute the editor value that was replaced."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-fallback-target-"))
          (marker (make-temp-file "ghostel-viewport-fallback-marker-"))
          (fallback (make-temp-file "ghostel-viewport-fallback-editor-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-fallback-output*"))
          process response)
     (unwind-protect
         (progn
           (delete-file marker)
           (with-temp-file fallback
             (insert "#!/bin/sh\nprintf '%s' \"$1\" > "
                     (shell-quote-argument marker) "\n"))
           (set-file-modes fallback #o700)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL" "1")
           (setenv "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR" fallback)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR_SET" "1")
           (setq process
                 (make-process
                  :name "ghostel-viewport-fallback"
                  :buffer output
                  :command (list ghostel-viewport-editor--helper-file
                                 "--kind" "EDITOR" target)
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (setq response
                     (car (ghostel-viewport-editor-test--response-files))))))
           (ghostel-viewport-editor--write-response response 'fallback)
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process)))))
           (should (zerop (process-exit-status process)))
           (should (equal (ghostel-viewport-editor-test--file-string marker)
                          target)))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (dolist (file (list target marker fallback))
         (when (file-exists-p file) (delete-file file)))))))

(ert-deftest ghostel-viewport-editor-test-helper-follows-owner-death ()
  "A running helper should follow its owner death without recovery."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-dead-owner-"))
          (helper (make-temp-file "ghostel-viewport-owner-helper-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-dead-owner-output*"))
          (nonce (make-string 64 ?c))
          owner process response identity)
     (unwind-protect
         (progn
           (with-temp-file helper
             (insert (ghostel-viewport-editor--helper-content)))
           (set-file-modes helper #o700)
           (setq owner
                 (make-process
                  :name "ghostel-viewport-temporary-owner"
                  :command (list "/bin/sh" "-c" "sleep 30")
                  :noquery t))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setq process
                 (make-process
                  :name "ghostel-viewport-dead-owner"
                  :buffer output
                  :command (list helper "--kind" "EDITOR" target)
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (setq response
                     (car (ghostel-viewport-editor-test--response-files))))))
           (setq identity
                 (ghostel-viewport-editor--process-identity
                  (process-id owner) nonce))
           (with-temp-file (concat response ".claim")
             (insert identity "\n"))
           (set-file-modes (concat response ".claim") #o600)
           (with-temp-file response
             (insert "accepted:" identity "\n"))
           (set-file-modes response #o600)
           (accept-process-output process 1.1)
           (should (process-live-p process))
           (delete-process owner)
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process))) 2))
           (should-not (zerop (process-exit-status process))))
       (when (process-live-p owner) (delete-process owner))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (when (file-exists-p helper) (delete-file helper))
       (when (file-exists-p target) (delete-file target))))))

(ert-deftest ghostel-viewport-editor-test-done-wins-over-owner-exit ()
  "A completed response should succeed even when its owner then exits."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-done-owner-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-done-owner-output*"))
          (nonce (make-string 64 ?d))
          owner process response identity)
     (unwind-protect
         (progn
           (setq owner
                 (make-process
                  :name "ghostel-viewport-done-owner"
                  :command (list "/bin/sh" "-c" "sleep 30")
                  :noquery t))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setq process
                 (make-process
                  :name "ghostel-viewport-done-helper"
                  :buffer output
                  :command (list ghostel-viewport-editor--helper-file
                                 "--kind" "EDITOR" target)
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (setq response
                     (car (ghostel-viewport-editor-test--response-files))))))
           (setq identity
                 (ghostel-viewport-editor--process-identity
                  (process-id owner) nonce))
           (with-temp-file (concat response ".claim")
             (insert identity "\n"))
           (set-file-modes (concat response ".claim") #o600)
           (with-temp-file response
             (insert "accepted:" identity "\n"))
           (set-file-modes response #o600)
           (accept-process-output process 1.1)
           (should (process-live-p process))
           (with-temp-file response (insert "done\n"))
           (delete-process owner)
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process))) 2))
           (should (zerop (process-exit-status process))))
       (when (process-live-p owner) (delete-process owner))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (when (file-exists-p target) (delete-file target))))))

(ert-deftest ghostel-viewport-editor-test-helper-rejects-reused-pid ()
  "A live PID with another start time must not keep a helper bound."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-reused-pid-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-reused-pid-output*"))
          (nonce (make-string 64 ?e))
          owner process response identity)
     (unwind-protect
         (progn
           (setq owner
                 (make-process
                  :name "ghostel-viewport-reused-owner"
                  :command (list "/bin/sh" "-c" "sleep 30")
                  :noquery t))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setq process
                 (make-process
                  :name "ghostel-viewport-reused-helper"
                  :buffer output
                  :command (list ghostel-viewport-editor--helper-file
                                 "--kind" "EDITOR" target)
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda ()
               (setq response
                     (car (ghostel-viewport-editor-test--response-files))))))
           (setq identity
                 (format "%s:%s:Thu Jan  1 00:00:00 1970"
                         (process-id owner) nonce))
           (with-temp-file (concat response ".claim")
             (insert identity "\n"))
           (set-file-modes (concat response ".claim") #o600)
           (with-temp-file response
             (insert "accepted:" identity "\n"))
           (set-file-modes response #o600)
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process))) 2))
           (should (process-live-p owner))
           (should-not (zerop (process-exit-status process))))
       (when (process-live-p owner) (delete-process owner))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (when (file-exists-p target) (delete-file target))))))

(ert-deftest ghostel-viewport-editor-test-support-path-is-stable ()
  "Another Emacs instance should use the same dispatcher path."
  (ghostel-viewport-editor-test--with-runtime
   (let ((helper ghostel-viewport-editor--helper-file)
         (activation ghostel-viewport-editor--activation-file)
         (ghostel-viewport-editor--instance-id (make-string 64 ?b)))
     (ghostel-viewport-editor--ensure-runtime)
     (should (equal helper ghostel-viewport-editor--helper-file))
     (should (equal activation ghostel-viewport-editor--activation-file)))))

(ert-deftest ghostel-viewport-editor-test-helper-falls-back-large-payload ()
  "A request too large for the protocol should not wait without a response."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((target (make-temp-file "ghostel-viewport-large-target-"))
          (marker (make-temp-file "ghostel-viewport-large-marker-"))
          (fallback (make-temp-file "ghostel-viewport-large-editor-"))
          (process-environment (copy-sequence process-environment))
          (output (generate-new-buffer " *ghostel-large-output*"))
          process)
     (unwind-protect
         (progn
           (delete-file marker)
           (with-temp-file fallback
             (insert "#!/bin/sh\nprintf fallback > "
                     (shell-quote-argument marker) "\n"))
           (set-file-modes fallback #o700)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
                   (ghostel-viewport-editor--token))
           (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
                   ghostel-viewport-editor--response-directory)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR" fallback)
           (setenv "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR_SET" "1")
           (setq process
                 (make-process
                  :name "ghostel-viewport-large"
                  :buffer output
                  :command (list ghostel-viewport-editor--helper-file
                                 "--kind" "EDITOR" target
                                 (make-string 50000 ?x))
                  :connection-type 'pty
                  :noquery t))
           (should
            (ghostel-viewport-editor-test--wait-until
             (lambda () (not (process-live-p process)))))
           (should (zerop (process-exit-status process)))
           (should (file-exists-p marker))
           (should-not (ghostel-viewport-editor-test--response-files)))
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p output) (kill-buffer output))
       (dolist (file (list target marker fallback))
         (when (file-exists-p file) (delete-file file)))))))

(ert-deftest ghostel-viewport-editor-test-restores-window-configuration ()
  "Closing one viewport should restore its exact original window layout."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((dir (make-temp-file "ghostel-viewport-win-config-" t))
          (file1 (expand-file-name "file1.txt" dir))
          (resp1 (ghostel-viewport-editor-test--make-response))
          (src1 (generate-new-buffer " *ghostel-src1*"))
          (side (generate-new-buffer " *ghostel-side*")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer src1)
           (set-window-buffer (split-window-right) side)
           (let ((original (current-window-configuration))
                 (selected (selected-window)))
             (ghostel-viewport-editor--receive-request
              src1
              (list :response-file resp1 :directory dir
                    :kind "EDITOR" :arguments '("file1.txt")))
             (let* ((req1 (buffer-local-value
                           'ghostel-viewport-editor--active-request src1))
                    (vp1 (ghostel-viewport-editor--request-viewport-buffer req1)))
               (should (buffer-live-p vp1))
               (with-current-buffer vp1
                 (ghostel-viewport-editor-finish))
               (should-not (buffer-live-p vp1))
               (should (compare-window-configurations
                        original (current-window-configuration)))
               (should (eq selected (selected-window))))))
       (when (buffer-live-p src1) (kill-buffer src1))
       (when (buffer-live-p side) (kill-buffer side))
       (delete-directory dir t)))))

(ert-deftest ghostel-viewport-editor-test-return-source-window-at-end ()
  "Returning from an editor should show the end of the Ghostel source."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((directory (make-temp-file "ghostel-viewport-return-point-" t))
          (response (ghostel-viewport-editor-test--make-response))
          (source (generate-new-buffer " *ghostel-return-point*"))
          (ghostel-viewport-editor-display-function
           #'ghostel-viewport-editor-display-same-window))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer source)
           (insert "old terminal output\n")
           (goto-char (point-min))
           (set-window-point (selected-window) (point-min))
           (ghostel-viewport-editor--receive-request
            source
            (list :response-file response :directory directory
                  :kind "EDITOR" :arguments '("prompt.txt")))
           (let* ((request
                   (buffer-local-value
                    'ghostel-viewport-editor--active-request source))
                  (viewport
                   (ghostel-viewport-editor--request-viewport-buffer request)))
             (with-current-buffer source
               (goto-char (point-max))
               (insert "new terminal output\n")
               (goto-char (point-min)))
             (with-current-buffer viewport
               (ghostel-viewport-editor-cancel))
             (should (eq (window-buffer (selected-window)) source))
             (should
              (= (window-point (selected-window))
                 (with-current-buffer source (point-max))))))
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory directory t)))))

(ert-deftest ghostel-viewport-editor-test-multiple-window-configurations ()
  "Closing one viewport should not replace another live viewport window."
  (ghostel-viewport-editor-test--with-runtime
   (let* ((dir (make-temp-file "ghostel-viewport-multi-window-" t))
          (resp1 (ghostel-viewport-editor-test--make-response))
          (resp2 (ghostel-viewport-editor-test--make-response))
          (src1 (generate-new-buffer " *ghostel-src1*"))
          (src2 (generate-new-buffer " *ghostel-src2*")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer src1)
           (ghostel-viewport-editor--receive-request
            src1
            (list :response-file resp1 :directory dir
                  :kind "EDITOR" :arguments '("file1.txt")))
           (let* ((req1 (buffer-local-value
                         'ghostel-viewport-editor--active-request src1))
                  (vp1 (ghostel-viewport-editor--request-viewport-buffer req1)))
             (delete-other-windows)
             (switch-to-buffer src2)
             (ghostel-viewport-editor--receive-request
              src2
              (list :response-file resp2 :directory dir
                    :kind "EDITOR" :arguments '("file2.txt")))
             (let* ((req2 (buffer-local-value
                           'ghostel-viewport-editor--active-request src2))
                    (vp2 (ghostel-viewport-editor--request-viewport-buffer req2))
                    (vp2-window (get-buffer-window vp2)))
               (with-current-buffer vp1
                 (ghostel-viewport-editor-finish))
               (should-not (buffer-live-p vp1))
               (should (window-live-p vp2-window))
               (should (eq (window-buffer vp2-window) vp2))
               (with-current-buffer vp2
                 (ghostel-viewport-editor-finish))
               (should-not (buffer-live-p vp2))
               (should (eq (window-buffer (selected-window)) src2)))))
       (when (buffer-live-p src1) (kill-buffer src1))
       (when (buffer-live-p src2) (kill-buffer src2))
       (delete-directory dir t)))))

(provide 'ghostel-viewport-editor-test)
;;; ghostel-viewport-editor-test.el ends here
