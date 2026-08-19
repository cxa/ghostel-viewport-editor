;;; ghostel-viewport-editor.el --- Edit Ghostel CLI requests in viewports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 realazy

;; Author: realazy <xianan.chen@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.35.0"))
;; Keywords: terminals, tools, convenience
;; URL: https://github.com/cxa/ghostel-viewport-editor
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `ghostel-viewport-editor-global-mode' routes external-editor requests from
;; new local Ghostel processes into Emacs without changing the global EDITOR,
;; VISUAL, GIT_EDITOR, or GIT_SEQUENCE_EDITOR values.
;;
;; A small editor helper announces each request through Ghostel's OSC command
;; interface and waits on a private response file.  Emacs presents one local
;; file in an isolated viewport, then finishing or canceling releases the
;; waiting program.  An accepted request remains owned by the Emacs process
;; that accepted it and is not recovered after that process exits.
;;
;; `C-c '' reveals an active viewport, or sends `C-x C-e' to a foreground
;; program without changing Ghostel's input mode.  A dtach shell routed
;; through an Emacs that has exited reconnects on its next editor call; a shell
;; that never inherited routing can be enabled with
;; `ghostel-viewport-editor-enable-current-shell'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-force-redraw "ghostel" ())
(declare-function ghostel-cursor-point "ghostel" ())

(defvar ghostel-eval-cmds)
(defvar ghostel-mode-hook)
(defvar ghostel-pre-spawn-hook)

(defgroup ghostel-viewport-editor nil
  "Edit terminal editor requests in Emacs viewports."
  :group 'ghostel
  :prefix "ghostel-viewport-editor-")

(defcustom ghostel-viewport-editor-state-directory
  (locate-user-emacs-file "ghostel-viewport-editor/")
  "Directory for generated helper and shell activation files.

Set this before enabling `ghostel-viewport-editor-global-mode'.  The directory
does not contain recoverable editor sessions."
  :type 'directory)

(defcustom ghostel-viewport-editor-accept-function nil
  "Optional function deciding whether to accept a supported request.

Nil accepts every supported request.  A function receives a request plist
whose keys include `:source-buffer', `:directory', `:arguments', `:file',
`:line', `:column', `:kind', and `:request-id'.  Returning nil or signaling an
error invokes the editor value replaced for that request."
  :type '(choice (const :tag "Accept every supported request" nil)
                 function))

(defcustom ghostel-viewport-editor-after-finish-function nil
  "Optional function called after a viewport is finished successfully.

The function receives the request plist in the originating Ghostel buffer.
It is not called after cancellation."
  :type '(choice (const :tag "No callback" nil)
                 function))

(defcustom ghostel-viewport-editor-display-function
  #'ghostel-viewport-editor-display-adaptively
  "Function used to display a viewport buffer.

The function receives the viewport buffer and an anchor window and returns the
window displaying the viewport."
  :type 'function)

(defconst ghostel-viewport-editor--command "ghostel-viewport-editor")
(defconst ghostel-viewport-editor--protocol-version "3")
(defconst ghostel-viewport-editor--max-payload-size (* 64 1024))
(defconst ghostel-viewport-editor--reannounce-interval 1)
(defconst ghostel-viewport-editor--completion-poll-interval 0.05)
(defconst ghostel-viewport-editor--completion-polls-per-owner-check 20)
(defconst ghostel-viewport-editor--editor-variables
  '("EDITOR" "VISUAL" "GIT_EDITOR" "GIT_SEQUENCE_EDITOR"))

(defvar ghostel-viewport-editor--token nil)
(defvar ghostel-viewport-editor--token-state-root nil)
(defvar ghostel-viewport-editor--instance-id nil)
(defvar ghostel-viewport-editor--helper-file nil)
(defvar ghostel-viewport-editor--activation-file nil)
(defvar ghostel-viewport-editor--response-directory nil)
(defvar ghostel-viewport-editor--requests (make-hash-table :test #'equal))
(defvar ghostel-viewport-editor-global-mode nil)

(defvar-local ghostel-viewport-editor-mode nil)
(defvar-local ghostel-viewport-editor--active-request nil)
(defvar-local ghostel-viewport-editor--request nil)
(defvar-local ghostel-viewport-editor--suppress-kill-query nil)

(cl-defstruct (ghostel-viewport-editor--request
               (:constructor ghostel-viewport-editor--make-request))
	      "Internal state for one routed editor request."
	      id
	      source-buffer
	      directory
	      arguments
	      file
	      target
	      target-id
	      line
	      column
	      kind
	      response-file
	      base-buffer
	      base-created-p
	      base-tick
	      base-text
	      base-file-digest
	      viewport-buffer
	      window-configuration
	      live-viewports
	      source-window-parameters
	      source-overriding-local-map
	      source-overriding-local-map-local-p
	      phase
	      outcome)

;;;; Generated support files

(defun ghostel-viewport-editor--state-root ()
  "Return the canonical configured state directory."
  (file-name-as-directory
   (expand-file-name ghostel-viewport-editor-state-directory)))

(defun ghostel-viewport-editor--ensure-private-directory (directory)
  "Create DIRECTORY with user-only permissions and return it."
  (make-directory directory t)
  (set-file-modes directory #o700)
  directory)

(defun ghostel-viewport-editor--random-token ()
  "Return a random token containing no shell metacharacters."
  (secure-hash
   'sha256
   (concat (format "%s:%s:%s:%s" (emacs-pid) (float-time)
                   (random most-positive-fixnum) (user-uid))
           (if (fboundp 'gnutls-random)
               (gnutls-random 32)
             ""))))

(defun ghostel-viewport-editor--instance-id ()
  "Return this Emacs process's request-owner identity nonce."
  (or ghostel-viewport-editor--instance-id
      (setq ghostel-viewport-editor--instance-id
            (ghostel-viewport-editor--random-token))))

(defun ghostel-viewport-editor--process-start-signature (pid)
  "Return `ps' start-time text for PID, or nil."
  (when-let* ((program (executable-find "ps"))
              ((integerp pid))
              ((> pid 0)))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "LC_ALL" "C")
      (with-temp-buffer
        (when (zerop
               (call-process program nil t nil
                             "-p" (number-to-string pid) "-o" "lstart="))
          (let ((signature
                 (replace-regexp-in-string
                  "[\r\n]+\\'" "" (buffer-string))))
            (and (not (string-empty-p signature)) signature)))))))

(defun ghostel-viewport-editor--process-identity (pid nonce)
  "Return the instance identity for PID and NONCE."
  (let ((start (ghostel-viewport-editor--process-start-signature pid)))
    (unless start
      (error "Could not identify viewport owner process %s" pid))
    (format "%s:%s:%s" pid nonce start)))

(defun ghostel-viewport-editor--read-token (file)
  "Return the private authentication token stored in FILE, or nil."
  (when-let* ((attributes (file-attributes file 'integer))
              ((null (file-attribute-type attributes)))
              ((equal (file-attribute-user-id attributes) (user-uid)))
              ((zerop (logand (or (file-modes file) 0) #o077))))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((token (string-trim (buffer-string))))
        (when (string-match-p "\\`[0-9a-f]\\{64\\}\\'" token)
          token)))))

(defun ghostel-viewport-editor--token ()
  "Return the private token shared by this state directory."
  (let ((directory (ghostel-viewport-editor--state-root)))
    (unless (equal directory ghostel-viewport-editor--token-state-root)
      (setq ghostel-viewport-editor--token-state-root directory
            ghostel-viewport-editor--token nil))
    (or ghostel-viewport-editor--token
        (let ((file (expand-file-name "token" directory)))
          (ghostel-viewport-editor--ensure-private-directory directory)
          (unless (file-exists-p file)
            (let ((temporary (make-temp-file
                              (expand-file-name ".token-" directory))))
              (unwind-protect
                  (condition-case nil
                      (progn
			(with-temp-file temporary
                          (insert (ghostel-viewport-editor--random-token) "\n"))
			(set-file-modes temporary #o600)
			(rename-file temporary file nil)
			(setq temporary nil))
                    (file-already-exists nil))
		(when (and temporary (file-exists-p temporary))
                  (delete-file temporary)))))
          (setq ghostel-viewport-editor--token
                (or (ghostel-viewport-editor--read-token file)
                    (error "Invalid viewport editor token file: %s"
                           file)))))))

(defun ghostel-viewport-editor--shell-quote (string)
  "Quote STRING as one POSIX shell word."
  (concat "'" (string-replace "'" "'\\''" string) "'"))

(defun ghostel-viewport-editor--ensure-file (file content mode)
  "Make FILE contain CONTENT with MODE and return FILE."
  (let ((directory
         (ghostel-viewport-editor--ensure-private-directory
          (file-name-directory file))))
    (unless (and (not (file-symlink-p file))
                 (file-regular-p file)
		 (with-temp-buffer
                   (insert-file-contents file)
                   (equal (buffer-string) content)))
      (let ((temporary (make-temp-file
                        (expand-file-name ".generated-" directory))))
        (unwind-protect
            (progn
              (with-temp-file temporary
                (insert content))
              (set-file-modes temporary mode)
              (rename-file temporary file t)
              (setq temporary nil))
          (when (and temporary (file-exists-p temporary))
            (delete-file temporary))))))
  (set-file-modes file mode)
  file)

(defun ghostel-viewport-editor--original-variable (name)
  "Return the environment variable holding NAME's fallback value."
  (concat "GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_" name))

(defun ghostel-viewport-editor--original-set-variable (name)
  "Return the environment variable recording whether NAME was set."
  (concat (ghostel-viewport-editor--original-variable name) "_SET"))

(defun ghostel-viewport-editor--installed-variable (name)
  "Return the environment variable recording NAME's installed command."
  (concat "GHOSTEL_VIEWPORT_EDITOR_INSTALLED_" name))

(defun ghostel-viewport-editor--helper-content ()
  "Return the package's sleeping POSIX editor helper."
  (concat
   "#!/bin/sh\n"
   "set -f\n"
   "if [ \"$#\" -lt 2 ] || [ \"$1\" != --kind ]; then exit 2; fi\n"
   "kind=$2\n"
   "shift 2\n"
   "case $kind in\n"
   "  EDITOR) fallback=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR-}; "
   "fallback_set=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR_SET-0} ;;\n"
   "  VISUAL) fallback=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_VISUAL-}; "
   "fallback_set=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_VISUAL_SET-0} ;;\n"
   "  GIT_EDITOR) fallback=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_EDITOR-}; "
   "fallback_set=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_EDITOR_SET-0} ;;\n"
   "  GIT_SEQUENCE_EDITOR) "
   "fallback=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_SEQUENCE_EDITOR-}; "
   "fallback_set=${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_SEQUENCE_EDITOR_SET-0} ;;\n"
   "  *) exit 2 ;;\n"
   "esac\n"
   "token=${GHOSTEL_VIEWPORT_EDITOR_TOKEN-}\n"
   "response_dir=${GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY-}\n"
   "interval=${GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL-1}\n"
   "completion_interval="
   (number-to-string ghostel-viewport-editor--completion-poll-interval) "\n"
   "owner_check_polls="
   (number-to-string
    ghostel-viewport-editor--completion-polls-per-owner-check) "\n"
   "accepted_polls=$owner_check_polls\n"
   "bound_identity=\n"
   "bound_owner=\n"
   "bound_start=\n"
   "[ -n \"$token\" ] && [ -d \"$response_dir\" ] && "
   "[ -w \"$response_dir\" ] || exit 1\n"
   "response=$(mktemp \"$response_dir/request.XXXXXX\") || exit 1\n"
   "chmod 600 \"$response\" || { rm -f \"$response\"; exit 1; }\n"
   "cleanup () { rm -f \"$response\" \"$response.claim\"; }\n"
   "trap 'cleanup; exit 1' HUP INT TERM\n"
   "validate_identity () {\n"
   "  checked_identity=$1\n"
   "  case $checked_identity in *:*:*) ;; *) return 1 ;; esac\n"
   "  checked_pid=${checked_identity%%:*}\n"
   "  checked_rest=${checked_identity#*:}\n"
   "  checked_nonce=${checked_rest%%:*}\n"
   "  checked_start=${checked_rest#*:}\n"
   "  case $checked_pid in ''|*[!0-9]*) return 1 ;; esac\n"
   "  case $checked_nonce in ''|*[!0-9a-f]*) return 1 ;; esac\n"
   "  [ \"${#checked_nonce}\" -eq 64 ] && [ -n \"$checked_start\" ]\n"
   "}\n"
   "owner_alive () {\n"
   "  [ -n \"$bound_owner\" ] || return 1\n"
   "  current_start=$(LC_ALL=C "
   (ghostel-viewport-editor--shell-quote
    (or (executable-find "ps") "ps"))
   " -p \"$bound_owner\" -o lstart= 2>/dev/null) || return 1\n"
   "  [ \"$current_start\" = \"$bound_start\" ]\n"
   "}\n"
   "payload=$(\n"
   "  {\n"
   "    printf '%s\\0' " (ghostel-viewport-editor--shell-quote
                          ghostel-viewport-editor--protocol-version)
   " \"$response\" \"$PWD\" \"$kind\" \"$#\"\n"
   "    for argument do printf '%s\\0' \"$argument\"; done\n"
   "  } | base64 | tr -d '\\n'\n"
   ") || { cleanup; exit 1; }\n"
   "announce () {\n"
   "  printf '\\033]52;e;" ghostel-viewport-editor--command
   " %s %s\\007' \"$token\" \"$payload\" > /dev/tty\n"
   "}\n"
   "run_fallback () {\n"
   "  cleanup\n"
   "  trap - HUP INT TERM\n"
   "  [ \"$fallback_set\" = 1 ] && [ -n \"$fallback\" ] || exit 1\n"
   "  if [ \"${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR_SET-0}\" = 1 ]; "
   "then EDITOR=$GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_EDITOR; export EDITOR; "
   "else unset EDITOR; fi\n"
   "  if [ \"${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_VISUAL_SET-0}\" = 1 ]; "
   "then VISUAL=$GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_VISUAL; export VISUAL; "
   "else unset VISUAL; fi\n"
   "  if [ \"${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_EDITOR_SET-0}\" = 1 ]; "
   "then GIT_EDITOR=$GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_EDITOR; "
   "export GIT_EDITOR; else unset GIT_EDITOR; fi\n"
   "  if [ \"${GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_SEQUENCE_EDITOR_SET-0}\" = 1 ]; "
   "then GIT_SEQUENCE_EDITOR=$GHOSTEL_VIEWPORT_EDITOR_ORIGINAL_GIT_SEQUENCE_EDITOR; "
   "export GIT_SEQUENCE_EDITOR; else unset GIT_SEQUENCE_EDITOR; fi\n"
   "  exec /bin/sh -c \"$fallback \\\"\\$@\\\"\" "
   "ghostel-viewport-fallback \"$@\"\n"
   "}\n"
   "if [ \"${#payload}\" -gt "
   (number-to-string ghostel-viewport-editor--max-payload-size)
   " ]; then run_fallback \"$@\"; fi\n"
   "while :; do\n"
   "  if [ -z \"$bound_identity\" ] && [ -r \"$response.claim\" ]; then\n"
   "    claimant=\n"
   "    IFS= read -r claimant < \"$response.claim\" || claimant=\n"
   "    if validate_identity \"$claimant\"; then\n"
   "      bound_identity=$checked_identity\n"
   "      bound_owner=$checked_pid\n"
   "      bound_start=$checked_start\n"
   "    fi\n"
   "  fi\n"
   "  outcome=waiting\n"
   "  IFS= read -r outcome < \"$response\" || outcome=waiting\n"
   "  case $outcome in\n"
   "    accepted:*)\n"
   "      handler=${outcome#accepted:}\n"
   "      validate_identity \"$handler\" || { cleanup; exit 1; }\n"
   "      if [ -n \"$bound_identity\" ] && "
   "[ \"$handler\" != \"$bound_identity\" ]; then cleanup; exit 1; fi\n"
   "      bound_identity=$checked_identity\n"
   "      bound_owner=$checked_pid\n"
   "      bound_start=$checked_start\n"
   "      if [ \"$accepted_polls\" -ge \"$owner_check_polls\" ]; then\n"
   "        owner_alive || { cleanup; trap - HUP INT TERM; exit 1; }\n"
   "        accepted_polls=0\n"
   "      fi\n"
   "      accepted_polls=$((accepted_polls + 1))\n"
   "      sleep \"$completion_interval\" ;;\n"
   "    done) cleanup; trap - HUP INT TERM; exit 0 ;;\n"
   "    fallback) run_fallback \"$@\" ;;\n"
   "    error) cleanup; trap - HUP INT TERM; exit 1 ;;\n"
   "    *)\n"
   "      if [ -n \"$bound_identity\" ]; then\n"
   "        owner_alive || { cleanup; trap - HUP INT TERM; exit 1; }\n"
   "      fi\n"
   "      announce; sleep \"$interval\" ;;\n"
   "  esac\n"
   "done\n"))

(defun ghostel-viewport-editor--activation-content (helper)
  "Return shell code that installs HELPER."
  (concat
   "# Generated by ghostel-viewport-editor.el.\n"
   (mapconcat
    (lambda (name)
      (let ((original (ghostel-viewport-editor--original-variable name))
            (original-set
             (ghostel-viewport-editor--original-set-variable name))
            (installed (ghostel-viewport-editor--installed-variable name))
            (command
             (format "%s --kind %s" (shell-quote-argument helper) name)))
        (concat
         "if [ \"${" name "-}\" != \"${" installed "-}\" ]; then\n"
         "  if [ \"${" name "+x}\" = x ]; then\n"
         "    " original "=$" name "\n"
         "    " original-set "=1\n"
         "  else\n"
         "    unset " original "\n"
         "    " original-set "=0\n"
         "  fi\n"
         "fi\n"
         name "=" (ghostel-viewport-editor--shell-quote command) "\n"
         installed "=$" name "\n"
         "export " name " " installed " " original-set "\n"
         "[ \"$" original-set "\" = 0 ] || export " original "\n")))
    ghostel-viewport-editor--editor-variables
    "")
   "GHOSTEL_VIEWPORT_EDITOR_TOKEN="
   (ghostel-viewport-editor--shell-quote
    (ghostel-viewport-editor--token)) "\n"
   "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY="
   (ghostel-viewport-editor--shell-quote
    ghostel-viewport-editor--response-directory) "\n"
   "GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL="
   (number-to-string ghostel-viewport-editor--reannounce-interval) "\n"
   "export GHOSTEL_VIEWPORT_EDITOR_TOKEN "
   "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY "
   "GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL\n"
   "if [ -n \"${ZSH_VERSION-}\" ] && "
   "[ -z \"${GHOSTEL_VIEWPORT_EDITOR_ZLE_WRAPPED-}\" ] && "
   "zle -l edit-command-line >/dev/null 2>&1; then\n"
   "  zle -A edit-command-line "
   "__ghostel_viewport_editor_original_edit_command_line\n"
   "  __ghostel_viewport_editor_edit_command_line () {\n"
   "    zle __ghostel_viewport_editor_original_edit_command_line\n"
   "    zle reset-prompt\n"
   "  }\n"
   "  zle -N edit-command-line "
   "__ghostel_viewport_editor_edit_command_line\n"
   "  GHOSTEL_VIEWPORT_EDITOR_ZLE_WRAPPED=1\n"
   "fi\n"))

(defun ghostel-viewport-editor--ghostel-zsh-bootstrap ()
  "Return Ghostel's active zsh bootstrap file, or nil."
  (when-let* ((root (getenv "EMACS_GHOSTEL_PATH"))
              (current-directory (getenv "ZDOTDIR"))
              (expected-directory
               (expand-file-name "etc/shell/bootstrap/zsh/" root))
              ((file-directory-p expected-directory))
              ((file-equal-p current-directory expected-directory))
              (bootstrap (expand-file-name ".zshenv" expected-directory))
              ((file-readable-p bootstrap)))
    bootstrap))

(defun ghostel-viewport-editor--zsh-overlay-content
    (bootstrap activation)
  "Return a zsh startup overlay for BOOTSTRAP and ACTIVATION."
  (concat
   "# Generated by ghostel-viewport-editor.el.\n"
   "builtin source -- " (ghostel-viewport-editor--shell-quote bootstrap) "\n"
   "if [[ -o interactive ]]; then\n"
   "  function __ghostel_viewport_editor_install {\n"
   "    builtin typeset _ghostel_viewport_editor_status=$?\n"
   "    builtin emulate -L zsh -o no_aliases\n"
   "    builtin typeset -ga precmd_functions\n"
   "    precmd_functions=("
   "${precmd_functions:#__ghostel_viewport_editor_install})\n"
   "    builtin source -- "
   (ghostel-viewport-editor--shell-quote activation) "\n"
   "    builtin unfunction __ghostel_viewport_editor_install\n"
   "    return $_ghostel_viewport_editor_status\n"
   "  }\n"
   "  builtin typeset -ga precmd_functions\n"
   "  precmd_functions=(__ghostel_viewport_editor_install "
   "${precmd_functions:#__ghostel_viewport_editor_install})\n"
   "fi\n"))

(defun ghostel-viewport-editor--ensure-runtime ()
  "Create the state directory's stable generated support files."
  (let ((state-root (ghostel-viewport-editor--state-root)))
    (setq ghostel-viewport-editor--helper-file
          (expand-file-name "editor" state-root)
          ghostel-viewport-editor--activation-file
          (expand-file-name "activate.sh" state-root)
          ghostel-viewport-editor--response-directory
          (file-name-as-directory (expand-file-name "responses" state-root)))
    (ghostel-viewport-editor--ensure-private-directory state-root)
    (ghostel-viewport-editor--ensure-private-directory
     ghostel-viewport-editor--response-directory)
    (ghostel-viewport-editor--ensure-file
     ghostel-viewport-editor--helper-file
     (ghostel-viewport-editor--helper-content)
     #o700)
    (ghostel-viewport-editor--ensure-file
     ghostel-viewport-editor--activation-file
     (ghostel-viewport-editor--activation-content
      ghostel-viewport-editor--helper-file)
     #o600)
    state-root))

;;;; Environment injection and dtach activation

(defun ghostel-viewport-editor--supported-host-p ()
  "Return non-nil when this host can run the generated helper."
  (and (file-executable-p "/bin/sh")
       (file-exists-p "/dev/tty")
       (executable-find "base64")
       (executable-find "mktemp")
       (ghostel-viewport-editor--process-start-signature (emacs-pid))))

(defun ghostel-viewport-editor--register-callback ()
  "Register the package command in Ghostel's OSC whitelist."
  (setq ghostel-eval-cmds
        (cons
         (list ghostel-viewport-editor--command
               #'ghostel-viewport-editor--handle-osc)
         (cl-remove-if
          (lambda (entry)
            (equal (car-safe entry) ghostel-viewport-editor--command))
          ghostel-eval-cmds))))

(defun ghostel-viewport-editor--ensure-supported ()
  "Signal a user-facing error unless Ghostel integration is available."
  (unless (or (featurep 'ghostel) (require 'ghostel nil t))
    (user-error "The package ghostel is not installed"))
  (unless (and (boundp 'ghostel-pre-spawn-hook)
               (boundp 'ghostel-eval-cmds)
               (fboundp 'ghostel-send-string))
    (user-error "This Ghostel version lacks viewport-editor integration"))
  (unless (ghostel-viewport-editor--supported-host-p)
    (user-error "This host cannot run the Ghostel viewport editor helper"))
  (ghostel-viewport-editor--register-callback)
  (ghostel-viewport-editor--ensure-runtime))

(defun ghostel-viewport-editor--capture-original-environment (name command)
  "Record NAME's current value unless it is the installed COMMAND."
  (let ((original (ghostel-viewport-editor--original-variable name))
        (original-set (ghostel-viewport-editor--original-set-variable name))
        (installed (ghostel-viewport-editor--installed-variable name))
        (current (getenv name)))
    (unless (equal current (getenv installed))
      (if current
          (progn
            (setenv original current)
            (setenv original-set "1"))
        (setenv original nil)
        (setenv original-set "0")))
    (setenv name command)
    (setenv installed command)))

(defun ghostel-viewport-editor--install-zsh-overlay ()
  "Arrange to reapply editor routing after user zsh startup files."
  (when-let* ((bootstrap
               (ghostel-viewport-editor--ghostel-zsh-bootstrap))
              (directory
               (expand-file-name "zsh/"
                                 (expand-file-name
                                  "shell/"
                                  (ghostel-viewport-editor--state-root))))
              (overlay (expand-file-name ".zshenv" directory)))
    (ghostel-viewport-editor--ensure-file
     overlay
     (ghostel-viewport-editor--zsh-overlay-content
      bootstrap ghostel-viewport-editor--activation-file)
     #o600)
    (setenv "ZDOTDIR" directory)))

(defun ghostel-viewport-editor--inject-environment (&optional force)
  "Inject routing into the dynamically bound child environment.

With FORCE, inject even when the global mode is disabled."
  (when (and (or force ghostel-viewport-editor-global-mode)
             (not (file-remote-p default-directory)))
    (ghostel-viewport-editor--ensure-runtime)
    (dolist (name ghostel-viewport-editor--editor-variables)
      (let ((command
             (format "%s --kind %s"
                     (shell-quote-argument
                      ghostel-viewport-editor--helper-file)
                     name)))
        (ghostel-viewport-editor--capture-original-environment name command)))
    (setenv "GHOSTEL_VIEWPORT_EDITOR_TOKEN"
            (ghostel-viewport-editor--token))
    (setenv "GHOSTEL_VIEWPORT_EDITOR_RESPONSE_DIRECTORY"
            ghostel-viewport-editor--response-directory)
    (setenv "GHOSTEL_VIEWPORT_EDITOR_REANNOUNCE_INTERVAL"
            (number-to-string
             ghostel-viewport-editor--reannounce-interval))
    (ghostel-viewport-editor--install-zsh-overlay)))

;;;###autoload
(defun ghostel-viewport-editor-environment
    (environment &optional directory)
  "Return a copy of ENVIRONMENT containing viewport editor routing.

DIRECTORY defaults to `default-directory'.  A remote environment is returned
unchanged.  This is useful when a caller starts a persistent dtach server
outside Ghostel."
  (ghostel-viewport-editor--ensure-supported)
  (let ((process-environment (copy-sequence environment))
        (default-directory (or directory default-directory)))
    (ghostel-viewport-editor--inject-environment t)
    process-environment))

;;;###autoload
(defun ghostel-viewport-editor-enable-current-shell ()
  "Enable routing in the current local Ghostel shell explicitly.

Invoke this only at an idle POSIX-compatible shell prompt.  The command
sources a generated activation file; it does not infer shell state or inject
an `eval' command."
  (interactive)
  (unless ghostel-viewport-editor-global-mode
    (user-error "Ghostel viewport editor global mode is disabled"))
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "This command must run in a Ghostel buffer"))
  (when (file-remote-p default-directory)
    (user-error "Remote shells are not supported"))
  (ghostel-viewport-editor--ensure-supported)
  (when (yes-or-no-p
         "Source viewport editor activation at this idle shell prompt? ")
    (ghostel-send-string
     (concat ". "
             (ghostel-viewport-editor--shell-quote
              ghostel-viewport-editor--activation-file)
             "\r"))))

;;;; Request transport

(defun ghostel-viewport-editor--split-nul-fields (string)
  "Split NUL-delimited STRING while preserving empty fields."
  (let ((start 0)
        fields
        end)
    (while (setq end (string-match "\0" string start))
      (push (substring string start end) fields)
      (setq start (1+ end)))
    (when (< start (length string))
      (push (substring string start) fields))
    (nreverse fields)))

(defun ghostel-viewport-editor--decode-payload (encoded)
  "Decode one bounded helper payload ENCODED, or return nil."
  (when (and (stringp encoded)
             (<= (length encoded) ghostel-viewport-editor--max-payload-size))
    (condition-case nil
        (let* ((decoded (base64-decode-string encoded))
               (fields (ghostel-viewport-editor--split-nul-fields decoded))
               (version (nth 0 fields))
               (response (nth 1 fields))
               (directory (nth 2 fields))
               (kind (nth 3 fields))
               (count-text (nth 4 fields))
               (arguments (nthcdr 5 fields)))
          (when (and (equal version ghostel-viewport-editor--protocol-version)
                     response directory
                     (member kind ghostel-viewport-editor--editor-variables)
                     (string-match-p "\\`[0-9]+\\'" (or count-text ""))
                     (= (string-to-number count-text) (length arguments)))
            (list :response-file response
                  :directory directory
                  :kind kind
                  :arguments arguments)))
      (error nil))))

(defun ghostel-viewport-editor--handle-osc (token encoded)
  "Handle authenticated TOKEN and ENCODED helper request from Ghostel."
  (when (and (stringp token)
             (string= token (ghostel-viewport-editor--token)))
    (when-let* ((payload
                 (ghostel-viewport-editor--decode-payload encoded))
                (source (current-buffer)))
      (run-at-time 0 nil #'ghostel-viewport-editor--receive-request
                   source payload))))

(defun ghostel-viewport-editor--response-file-valid-p (file)
  "Return non-nil when FILE is this state directory's private response file."
  (and (stringp file)
       (not (file-remote-p file))
       ghostel-viewport-editor--response-directory
       (let* ((expanded (expand-file-name file))
              (directory (file-name-directory expanded))
              (attributes (file-attributes expanded 'integer)))
         (and (equal directory
                     (file-name-as-directory
                      (expand-file-name
                       ghostel-viewport-editor--response-directory)))
              attributes
              (null (file-attribute-type attributes))
              (equal (file-attribute-user-id attributes) (user-uid))
              (zerop (logand (or (file-modes expanded) 0) #o077))))))

(defun ghostel-viewport-editor--owner-identity ()
  "Return this Emacs process's PID and instance nonce."
  (ghostel-viewport-editor--process-identity
   (emacs-pid) (ghostel-viewport-editor--instance-id)))

(defun ghostel-viewport-editor--write-response (file outcome)
  "Atomically write OUTCOME to validated response FILE."
  (unless (memq outcome '(accepted done fallback error))
    (error "Invalid viewport editor outcome: %S" outcome))
  (unless (ghostel-viewport-editor--response-file-valid-p file)
    (error "Invalid viewport editor response file"))
  (let ((temporary
         (make-temp-file
          (expand-file-name ".response-"
                            ghostel-viewport-editor--response-directory))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert (if (eq outcome 'accepted)
                        (format "accepted:%s"
                                (ghostel-viewport-editor--owner-identity))
                      (symbol-name outcome))
                    "\n"))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun ghostel-viewport-editor--claim-response (file)
  "Claim FILE for this Emacs process and return non-nil on success."
  (let ((claim (concat file ".claim"))
        (owner (concat (ghostel-viewport-editor--owner-identity) "\n")))
    (condition-case nil
        (progn
          (write-region owner nil claim nil 'silent nil 'excl)
          (set-file-modes claim #o600)
          t)
      (file-already-exists
       (and (ghostel-viewport-editor--response-file-valid-p file)
            (let ((attributes (file-attributes claim 'integer)))
              (and attributes
                   (null (file-attribute-type attributes))
                   (equal (file-attribute-user-id attributes) (user-uid))
                   (zerop (logand (or (file-modes claim) 0) #o077))
                   (with-temp-buffer
                     (insert-file-contents claim)
                     (equal (buffer-string) owner)))))))))

(defun ghostel-viewport-editor--parse-position (argument)
  "Parse +LINE or +LINE:COLUMN ARGUMENT and return a cons."
  (when (and (stringp argument)
             (string-match
              "\\`+\\([1-9][0-9]*\\)\\(?::\\([1-9][0-9]*\\)\\)?\\'"
              argument))
    (cons (string-to-number (match-string 1 argument))
          (when (match-string 2 argument)
            (string-to-number (match-string 2 argument))))))

(defun ghostel-viewport-editor--parse-file-arguments
    (directory arguments)
  "Return (FILE LINE COLUMN) for supported ARGUMENTS in DIRECTORY."
  (when (and (stringp directory)
             (not (file-remote-p directory))
             (file-directory-p directory))
    (let ((args arguments)
          line
          column
          protected)
      (when-let* ((position
                   (ghostel-viewport-editor--parse-position (car args))))
        (setq line (car position)
              column (cdr position)
              args (cdr args)))
      (when (equal (car args) "--")
        (setq protected t
              args (cdr args)))
      (when (= (length args) 1)
        (let* ((name (car args))
               (file (expand-file-name name directory)))
          (when (and (or protected
                         (not (string-match-p "\\`[-+]" name)))
                     (not (file-remote-p file))
                     (not (file-directory-p file))
                     (or (not (file-exists-p file))
                         (and (file-regular-p file)
                              (file-readable-p file)))
                     (or (file-exists-p file)
                         (file-writable-p (file-name-directory file))))
            (list file line column)))))))

(defun ghostel-viewport-editor--request-plist (request)
  "Return REQUEST's public request plist."
  (list :source-buffer
        (ghostel-viewport-editor--request-source-buffer request)
        :directory (ghostel-viewport-editor--request-directory request)
        :arguments (copy-sequence
                    (ghostel-viewport-editor--request-arguments request))
        :file (ghostel-viewport-editor--request-file request)
        :line (ghostel-viewport-editor--request-line request)
        :column (ghostel-viewport-editor--request-column request)
        :kind (ghostel-viewport-editor--request-kind request)
        :request-id (ghostel-viewport-editor--request-id request)))

(defun ghostel-viewport-editor--accept-p (request)
  "Return non-nil when REQUEST should use a viewport."
  (or (null ghostel-viewport-editor-accept-function)
      (condition-case error-data
          (funcall ghostel-viewport-editor-accept-function
                   (ghostel-viewport-editor--request-plist request))
        (error
         (message "ghostel-viewport-editor: accept function failed: %s"
                  (error-message-string error-data))
         nil))))

(defun ghostel-viewport-editor--request-anchor-window (source)
  "Return a suitable anchor window for SOURCE."
  (or (and (eq (window-buffer (selected-window)) source)
           (selected-window))
      (get-buffer-window source (selected-frame))
      (get-buffer-window source t)
      (selected-window)))

(defun ghostel-viewport-editor--receive-request (source payload)
  "Validate and present PAYLOAD received from Ghostel SOURCE."
  (let ((response (plist-get payload :response-file))
        request)
    (when (ghostel-viewport-editor--response-file-valid-p response)
      (condition-case error-data
          (cond
           ((gethash response ghostel-viewport-editor--requests)
            (setq request
                  (gethash response ghostel-viewport-editor--requests))
            (pcase (ghostel-viewport-editor--request-phase request)
              ('accepted
               (ghostel-viewport-editor--write-response response 'accepted))
              ('complete
               (ghostel-viewport-editor--write-response
                response
                (ghostel-viewport-editor--request-outcome request)))))
           ((not (ghostel-viewport-editor--claim-response response)) nil)
           ((not (buffer-live-p source))
            (ghostel-viewport-editor--write-response response 'error))
           (t
            (pcase-let*
                ((`(,file ,line ,column)
                  (or (ghostel-viewport-editor--parse-file-arguments
                       (plist-get payload :directory)
                       (plist-get payload :arguments))
                      '(nil nil nil))))
              (setq request
                    (ghostel-viewport-editor--make-request
                     :id (file-name-nondirectory response)
                     :source-buffer source
                     :directory (file-name-as-directory
                                 (expand-file-name
                                  (plist-get payload :directory)))
                     :arguments (plist-get payload :arguments)
                     :file file
                     :line line
                     :column column
                     :kind (plist-get payload :kind)
                     :response-file response
                     :phase 'waiting))
              (if (or (null file)
                      (buffer-local-value
                       'ghostel-viewport-editor--active-request source)
                      (not (ghostel-viewport-editor--accept-p request)))
                  (ghostel-viewport-editor--write-response response 'fallback)
                (ghostel-viewport-editor--open-request request)
                (ghostel-viewport-editor--write-response response 'accepted)))))
        (error
         (message "ghostel-viewport-editor: %s"
                  (error-message-string error-data))
         ;; Once a viewport exists, keep it and let a repeated helper
         ;; announcement retry the acknowledgement.
         (unless (and request
                      (eq (ghostel-viewport-editor--request-phase request)
                          'accepted))
           (condition-case nil
               (ghostel-viewport-editor--write-response response 'fallback)
             (error nil))))))))

;;;; Viewport buffers and display

(defconst ghostel-viewport-editor--header-line-format
  '(" Ghostel edit   "
    (:propertize "C-c C-c" face mode-line-emphasis)
    " finish   "
    (:propertize "C-c C-k" face mode-line-emphasis)
    " cancel")
  "Header line shown in viewport editor buffers.")

(defun ghostel-viewport-editor-backward-delete (count)
  "Delete the active region or COUNT characters backward."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (backward-delete-char-untabify count)))

(defvar ghostel-viewport-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ghostel-viewport-editor-finish)
    (define-key map (kbd "C-c C-k") #'ghostel-viewport-editor-cancel)
    (define-key map (kbd "DEL")
                #'ghostel-viewport-editor-backward-delete)
    (define-key map [backspace]
                #'ghostel-viewport-editor-backward-delete)
    (define-key map (kbd "C-x #") #'ghostel-viewport-editor-finish)
    (define-key map [remap save-buffer] #'ghostel-viewport-editor-finish)
    map)
  "Keymap active in viewport editor buffers.")

(defun ghostel-viewport-editor--configure-viewport-buffer ()
  "Restore the editing invariants of the current viewport buffer."
  (when (bound-and-true-p view-mode)
    (view-mode -1))
  (setq buffer-read-only nil)
  (add-hook 'kill-buffer-query-functions
            #'ghostel-viewport-editor--kill-query nil t)
  (add-hook 'kill-buffer-hook
            #'ghostel-viewport-editor--viewport-killed nil t)
  (add-hook 'after-change-major-mode-hook
            #'ghostel-viewport-editor--after-major-mode-change nil t)
  (setq-local header-line-format
              ghostel-viewport-editor--header-line-format))

(defun ghostel-viewport-editor--after-major-mode-change ()
  "Restore viewport state after changing the current major mode."
  (when ghostel-viewport-editor-mode
    (ghostel-viewport-editor--configure-viewport-buffer)))

(put 'ghostel-viewport-editor--after-major-mode-change
     'permanent-local-hook t)

(define-minor-mode ghostel-viewport-editor-mode
  "Minor mode for a file being edited on behalf of a Ghostel process."
  :init-value nil
  :lighter " Viewport"
  :keymap ghostel-viewport-editor-mode-map
  (if ghostel-viewport-editor-mode
      (ghostel-viewport-editor--configure-viewport-buffer)
    (when (and ghostel-viewport-editor--request
               (eq (ghostel-viewport-editor--request-phase
                    ghostel-viewport-editor--request)
                   'accepted))
      (setq ghostel-viewport-editor-mode t)
      (user-error "Finish or cancel the active Ghostel viewport first"))
    (remove-hook 'kill-buffer-query-functions
                 #'ghostel-viewport-editor--kill-query t)
    (remove-hook 'kill-buffer-hook
                 #'ghostel-viewport-editor--viewport-killed t)
    (remove-hook 'after-change-major-mode-hook
                 #'ghostel-viewport-editor--after-major-mode-change t)))

(put 'ghostel-viewport-editor-mode 'permanent-local t)
(put 'ghostel-viewport-editor--request 'permanent-local t)
(put 'ghostel-viewport-editor--suppress-kill-query 'permanent-local t)

(defun ghostel-viewport-editor-display-same-window (buffer anchor)
  "Display BUFFER in ANCHOR and return its window."
  (with-selected-window (if (window-live-p anchor)
                            anchor
                          (selected-window))
    (display-buffer buffer '(display-buffer-same-window))))

(defun ghostel-viewport-editor--display-in-direction
    (buffer anchor direction)
  "Try to display BUFFER from ANCHOR in DIRECTION."
  (let ((size
         (if (memq direction '(left right))
             (cons 'window-width
                   (max window-min-width
                        (/ (window-total-width anchor) 2)))
           (cons 'window-height
                 (max window-min-height
                      (/ (window-total-height anchor) 2))))))
    (condition-case nil
        (with-selected-window anchor
          (display-buffer
           buffer
           `((display-buffer-in-direction)
             (direction . ,direction)
             (window . ,anchor)
             ,size)))
      (error nil))))

(defun ghostel-viewport-editor-display-adaptively (buffer anchor)
  "Display BUFFER by splitting ANCHOR along its longest dimension."
  (setq anchor (if (window-live-p anchor) anchor (selected-window)))
  (let* ((wide (>= (window-pixel-width anchor)
                   (window-pixel-height anchor)))
         (first (if wide 'right 'below))
         (second (if wide 'below 'right))
         (window
          (or (ghostel-viewport-editor--display-in-direction
               buffer anchor first)
              (ghostel-viewport-editor--display-in-direction
               buffer anchor second)
              (ghostel-viewport-editor-display-same-window buffer anchor))))
    (when (window-live-p window)
      (select-window window))
    window))

(defun ghostel-viewport-editor--live-viewport-buffers ()
  "Return a list of all live viewport editor buffers."
  (let (buffers)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-local-value 'ghostel-viewport-editor-mode buffer))
        (push buffer buffers)))
    buffers))

(defun ghostel-viewport-editor--other-viewport-window-p (frame current-buffer)
  "Return non-nil if FRAME displays a viewport other than CURRENT-BUFFER."
  (let ((found nil))
    (walk-windows
     (lambda (window)
       (let ((buf (window-buffer window)))
         (when (and (not (eq buf current-buffer))
                    (buffer-live-p buf)
                    (buffer-local-value 'ghostel-viewport-editor-mode buf))
           (setq found t))))
     nil frame)
    found))

(defun ghostel-viewport-editor--discard-base (request)
  "Kill REQUEST's unchanged package-created backing buffer."
  (when-let* (((ghostel-viewport-editor--request-base-created-p request))
              (base (ghostel-viewport-editor--request-base-buffer request))
              ((buffer-live-p base)))
    (with-current-buffer base
      (when (not (buffer-modified-p))
        (kill-buffer base)))))

(defun ghostel-viewport-editor--buffer-text ()
  "Return the entire buffer text without properties."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ghostel-viewport-editor--file-identifier (file)
  "Return FILE's followed filesystem identity, or nil when it is absent."
  (when-let* ((attributes
               (file-attributes (file-truename file) 'integer)))
    (cons (file-attribute-device-number attributes)
          (file-attribute-inode-number attributes))))

(defun ghostel-viewport-editor--file-digest (file)
  "Return a SHA-256 digest of local FILE bytes, or nil when absent."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun ghostel-viewport-editor--target-current-p (request base)
  "Return non-nil when REQUEST and BASE still name the accepted target."
  (condition-case nil
      (let* ((file (ghostel-viewport-editor--request-file request))
             (base-file (buffer-local-value 'buffer-file-name base))
             (target (ghostel-viewport-editor--request-target request))
             (identifier
              (ghostel-viewport-editor--request-target-id request))
             (file-id (ghostel-viewport-editor--file-identifier file))
             (base-id
              (ghostel-viewport-editor--file-identifier base-file)))
        (and (equal (file-truename file) target)
             (equal (file-truename base-file) target)
             (if identifier
                 (and (equal file-id identifier)
                      (equal base-id identifier))
               (and (null file-id) (null base-id)))))
    (error nil)))

(defun ghostel-viewport-editor--open-request (request)
  "Open and display an isolated buffer for supported REQUEST."
  (let* ((source (ghostel-viewport-editor--request-source-buffer request))
         (file (ghostel-viewport-editor--request-file request))
         (target (file-truename file))
         (target-id (ghostel-viewport-editor--file-identifier file))
         (file-digest (ghostel-viewport-editor--file-digest file))
         (anchor (ghostel-viewport-editor--request-anchor-window source))
         (existing (find-buffer-visiting file))
         (base (or existing (find-file-noselect file)))
         (buffer (generate-new-buffer
                  (format "*Ghostel edit: %s*"
                          (file-name-nondirectory file))))
         committed)
    (unwind-protect
        (progn
          (with-current-buffer base
            (when (or (buffer-modified-p)
                      (not (verify-visited-file-modtime base)))
              (error "File has unsaved or external changes in %s"
                     (buffer-name)))
            (setf (ghostel-viewport-editor--request-base-buffer request) base
                  (ghostel-viewport-editor--request-target request) target
                  (ghostel-viewport-editor--request-target-id request) target-id
                  (ghostel-viewport-editor--request-base-created-p request)
                  (null existing)
                  (ghostel-viewport-editor--request-base-tick request)
                  (buffer-chars-modified-tick)
                  (ghostel-viewport-editor--request-base-text request)
                  (ghostel-viewport-editor--buffer-text)))
          (unless (ghostel-viewport-editor--target-current-p request base)
            (error "Requested file does not match its backing buffer"))
          (unless (equal file-digest
                         (ghostel-viewport-editor--file-digest file))
            (error "Requested file changed while opening"))
          (setf (ghostel-viewport-editor--request-base-file-digest request)
                file-digest)
          (setf (ghostel-viewport-editor--request-viewport-buffer request)
                buffer)
          (with-current-buffer buffer
            (setq default-directory
                  (ghostel-viewport-editor--request-directory request))
            (insert (ghostel-viewport-editor--request-base-text request))
            (let ((buffer-file-name file))
              (set-auto-mode))
            ;; Major-mode setup clears ordinary buffer-local variables.
            (setq ghostel-viewport-editor--request request)
            (ghostel-viewport-editor-mode 1)
            (set-buffer-modified-p nil)
            (if-let* ((line
                       (ghostel-viewport-editor--request-line request)))
                (progn
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (when-let* ((column
                               (ghostel-viewport-editor--request-column
                                request)))
                    (move-to-column (1- column))))
              (goto-char (point-max))))
            (setf (ghostel-viewport-editor--request-live-viewports request)
                  (ghostel-viewport-editor--live-viewport-buffers)
                  (ghostel-viewport-editor--request-window-configuration request)
                  (with-selected-window anchor
                    (current-window-configuration)))
            (let ((window
                   (funcall ghostel-viewport-editor-display-function
                            buffer anchor)))
            (unless (and (window-live-p window)
                         (eq (window-buffer window) buffer))
              (error "Display function did not return the viewport window")))
          (setf (ghostel-viewport-editor--request-phase request) 'accepted)
          (puthash (ghostel-viewport-editor--request-response-file request)
                   request ghostel-viewport-editor--requests)
          (with-current-buffer source
            (setq ghostel-viewport-editor--active-request request)
            (add-hook 'kill-buffer-hook
                      #'ghostel-viewport-editor--source-killed nil t)
            (ghostel-viewport-editor--lock-source request))
          (setq committed t)
          request)
      (unless committed
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq ghostel-viewport-editor--suppress-kill-query t)
            (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (ghostel-viewport-editor--discard-base request)))))

(defun ghostel-viewport-editor--commit-request (request)
  "Commit REQUEST's isolated viewport to its unchanged backing buffer."
  (let ((base (ghostel-viewport-editor--request-base-buffer request))
        (viewport (ghostel-viewport-editor--request-viewport-buffer request)))
    (unless (and (buffer-live-p base) (buffer-live-p viewport))
      (user-error "The viewport backing buffer is no longer available"))
    (let ((text (with-current-buffer viewport
                  (ghostel-viewport-editor--buffer-text))))
      (with-current-buffer base
        (unless (and (ghostel-viewport-editor--target-current-p request base)
                     (equal
                      (ghostel-viewport-editor--file-digest
                       (ghostel-viewport-editor--request-file request))
                      (ghostel-viewport-editor--request-base-file-digest
                       request))
                     (not (buffer-modified-p))
                     (= (buffer-chars-modified-tick)
                        (ghostel-viewport-editor--request-base-tick request))
                     (equal (ghostel-viewport-editor--buffer-text)
                            (ghostel-viewport-editor--request-base-text request))
                     (verify-visited-file-modtime base))
          (user-error "The backing file changed; the viewport was kept"))
        (let (saved-text saved-digest saved-id post-save-error)
          (condition-case error-data
              (progn
                (atomic-change-group
                  (let ((inhibit-read-only t))
                    (save-restriction
                      (widen)
                      (replace-region-contents
                       (point-min) (point-max) (lambda () text))))
                  (let ((after-save-hook
                         (cons
                          (lambda ()
                            (setq saved-text
                                  (ghostel-viewport-editor--buffer-text)
                                  saved-digest
                                  (ghostel-viewport-editor--file-digest
                                   buffer-file-name)
                                  saved-id
                                  (ghostel-viewport-editor--file-identifier
                                   buffer-file-name)))
                          after-save-hook)))
                    (condition-case save-error
                        (save-buffer)
                      (error
                       (if (and saved-text
                                (equal saved-digest
                                       (ghostel-viewport-editor--file-digest
                                        buffer-file-name))
                                (equal saved-id
                                       (ghostel-viewport-editor--file-identifier
                                        buffer-file-name)))
                           (let ((inhibit-read-only t)
                                 (inhibit-modification-hooks t))
                             (save-restriction
                               (widen)
                               (replace-region-contents
                                (point-min) (point-max)
                                (lambda () saved-text)))
                             (set-buffer-modified-p nil)
                             (setq post-save-error save-error))
                         (signal (car save-error) (cdr save-error))))))
                  ;; An unchanged replacement does not run save hooks because
                  ;; `save-buffer' has nothing to write.  Record the already
                  ;; verified file as the successful result in that case.
                  (unless saved-text
                    (setq saved-text (ghostel-viewport-editor--buffer-text)
                          saved-digest
                          (ghostel-viewport-editor--file-digest
                           buffer-file-name)
                          saved-id
                          (ghostel-viewport-editor--file-identifier
                           buffer-file-name)))
                  (unless
                      (and saved-digest
                           (file-exists-p
                            (ghostel-viewport-editor--request-file request))
                           (file-exists-p buffer-file-name)
                           (file-equal-p
                            (ghostel-viewport-editor--request-file request)
                            buffer-file-name)
                           (equal saved-id
                                  (ghostel-viewport-editor--file-identifier
                                   buffer-file-name))
                           (equal saved-digest
                                  (ghostel-viewport-editor--file-digest
                                   buffer-file-name)))
                    (error "Backing target changed while saving")))
                (when post-save-error
                  (message
                   "ghostel-viewport-editor: saved despite post-save error: %s"
                   (error-message-string post-save-error)))
                (setf (ghostel-viewport-editor--request-target request)
                      (file-truename
                       (ghostel-viewport-editor--request-file request))
                      (ghostel-viewport-editor--request-target-id request)
                      (ghostel-viewport-editor--file-identifier
                       (ghostel-viewport-editor--request-file request))
                      (ghostel-viewport-editor--request-base-tick request)
                      (buffer-chars-modified-tick)
                      (ghostel-viewport-editor--request-base-text request)
                      (ghostel-viewport-editor--buffer-text)
                      (ghostel-viewport-editor--request-base-file-digest request)
                      (ghostel-viewport-editor--file-digest
                       (ghostel-viewport-editor--request-file request))))
          (error
           ;; An atomic change rollback can advance the tick.  Refresh it only
           ;; when the original unmodified text was restored.
           (when (and (not (buffer-modified-p))
                      (equal (ghostel-viewport-editor--buffer-text)
                             (ghostel-viewport-editor--request-base-text
                              request)))
             (setf (ghostel-viewport-editor--request-base-tick request)
                   (buffer-chars-modified-tick)))
             (signal (car error-data) (cdr error-data)))))))))

(defun ghostel-viewport-editor--cancel-needs-confirmation-p ()
  "Return non-nil when canceling the current viewport needs confirmation."
  (or (buffer-modified-p)
      (> (buffer-size) 0)))

(defun ghostel-viewport-editor--cancel-confirmed-p ()
  "Return non-nil when the current viewport may be canceled."
  (or (not (ghostel-viewport-editor--cancel-needs-confirmation-p))
      (yes-or-no-p "Discard this viewport edit? ")))

(defun ghostel-viewport-editor--source-killed ()
  "Fail the accepted request owned by the source buffer being killed."
  (when (and ghostel-viewport-editor--active-request
             (not (eq (ghostel-viewport-editor--request-phase
                       ghostel-viewport-editor--active-request)
                      'complete)))
    (condition-case error-data
        (ghostel-viewport-editor--clear-request
         ghostel-viewport-editor--active-request 'error)
      (error
       (message "ghostel-viewport-editor: could not fail helper: %s"
                (error-message-string error-data))))))

(defun ghostel-viewport-editor--clear-request (request outcome)
  "Release REQUEST with OUTCOME and clear its in-memory ownership."
  (unless (eq (ghostel-viewport-editor--request-phase request) 'complete)
    (let ((response
           (ghostel-viewport-editor--request-response-file request)))
      ;; Preserve the viewport for a live but invalid response channel.  A
      ;; missing file means the waiting helper has already gone away.
      (condition-case error-data
          (ghostel-viewport-editor--write-response response outcome)
        (error
         (when (file-exists-p response)
           (signal (car error-data) (cdr error-data))))))
    (setf (ghostel-viewport-editor--request-phase request) 'complete
          (ghostel-viewport-editor--request-outcome request) outcome)
    ;; Keep a brief tombstone for announcements already queued by dtach.
    (run-at-time
     1 nil
     (lambda (response expected)
       (when (eq (gethash response ghostel-viewport-editor--requests)
                 expected)
         (remhash response ghostel-viewport-editor--requests)))
     (ghostel-viewport-editor--request-response-file request) request)
    (when-let* ((source
                 (ghostel-viewport-editor--request-source-buffer request))
                ((buffer-live-p source)))
      (with-current-buffer source
        (when (eq ghostel-viewport-editor--active-request request)
          (ghostel-viewport-editor--unlock-source request)
          (setq ghostel-viewport-editor--active-request nil)
          (remove-hook 'kill-buffer-hook
                       #'ghostel-viewport-editor--source-killed t))))))

(defun ghostel-viewport-editor--refresh-source (request)
  "Render REQUEST's source at its live cursor and synchronously repaint it."
  (when-let* ((source
               (ghostel-viewport-editor--request-source-buffer request))
              ((buffer-live-p source))
              (windows (get-buffer-window-list source nil t)))
    (let (target)
      (with-current-buffer source
        (when (and (derived-mode-p 'ghostel-mode)
                   (fboundp 'ghostel-force-redraw))
          (ghostel-force-redraw))
        (setq target
              (and (derived-mode-p 'ghostel-mode)
                   (fboundp 'ghostel-cursor-point)
                   (ghostel-cursor-point)))
        (unless (and (integer-or-marker-p target)
                     (<= (point-min) target)
                     (<= target (point-max)))
          (setq target (point-max)))
        (goto-char target))
      (dolist (window windows)
        (when (window-live-p window)
          (set-window-point window target)
          (force-window-update window)))
      (redisplay t))))

(defun ghostel-viewport-editor--close-viewport (request)
  "Close REQUEST's viewport and any package-created backing buffer."
  (when-let* ((buffer
               (ghostel-viewport-editor--request-viewport-buffer request))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq ghostel-viewport-editor--suppress-kill-query t)
      (set-buffer-modified-p nil))
    (let* ((wc (ghostel-viewport-editor--request-window-configuration request))
           (live-viewports
            (ghostel-viewport-editor--request-live-viewports request))
           (frame (and wc (window-configuration-p wc)
                       (frame-live-p (window-configuration-frame wc))
                       (window-configuration-frame wc)))
           (window (get-buffer-window buffer (or frame t))))
      (if (and wc frame
               (cl-every #'buffer-live-p live-viewports)
               (not (ghostel-viewport-editor--other-viewport-window-p
                     frame buffer)))
          (progn
            (set-window-configuration wc)
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
        (if (and window (window-live-p window))
            (progn
              (condition-case nil
                  (delete-window window)
                (error (quit-window nil window)))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))))
  (ghostel-viewport-editor--discard-base request)
  (ghostel-viewport-editor--refresh-source request))

(defun ghostel-viewport-editor--run-after-finish (request)
  "Run the optional finish callback for REQUEST."
  (when-let* ((function ghostel-viewport-editor-after-finish-function)
              (source
               (ghostel-viewport-editor--request-source-buffer request))
              ((buffer-live-p source)))
    (with-current-buffer source
      (condition-case error-data
          (funcall function
                   (ghostel-viewport-editor--request-plist request))
        (error
         (message "ghostel-viewport-editor: finish callback failed: %s"
                  (error-message-string error-data)))))))

;;;###autoload
(defun ghostel-viewport-editor-finish ()
  "Save the current viewport and release its waiting editor process."
  (interactive)
  (unless (and ghostel-viewport-editor-mode
               ghostel-viewport-editor--request)
    (user-error "This is not a Ghostel viewport editor buffer"))
  (let ((request ghostel-viewport-editor--request))
    (ghostel-viewport-editor--commit-request request)
    (ghostel-viewport-editor--clear-request request 'done)
    (ghostel-viewport-editor--close-viewport request)
    (ghostel-viewport-editor--run-after-finish request)))

;;;###autoload
(defun ghostel-viewport-editor-cancel ()
  "Cancel the current viewport without changing its backing file."
  (interactive)
  (unless (and ghostel-viewport-editor-mode
               ghostel-viewport-editor--request)
    (user-error "This is not a Ghostel viewport editor buffer"))
  (when (ghostel-viewport-editor--cancel-confirmed-p)
    (let ((request ghostel-viewport-editor--request))
      (ghostel-viewport-editor--clear-request request 'done)
      (ghostel-viewport-editor--close-viewport request))))

(defun ghostel-viewport-editor--kill-query ()
  "Release the helper when the viewport is killed through another command."
  (or ghostel-viewport-editor--suppress-kill-query
      (when (ghostel-viewport-editor--cancel-confirmed-p)
        (when ghostel-viewport-editor--request
          (ghostel-viewport-editor--clear-request
           ghostel-viewport-editor--request 'done)
          (ghostel-viewport-editor--discard-base
           ghostel-viewport-editor--request))
        (set-buffer-modified-p nil)
        t)))

(defun ghostel-viewport-editor--viewport-killed ()
  "Fail an accepted request if its viewport disappears unexpectedly."
  (when (and ghostel-viewport-editor--request
             (not ghostel-viewport-editor--suppress-kill-query)
             (not (eq (ghostel-viewport-editor--request-phase
                       ghostel-viewport-editor--request)
                      'complete)))
    (condition-case error-data
        (progn
          (ghostel-viewport-editor--clear-request
           ghostel-viewport-editor--request 'error)
          (ghostel-viewport-editor--discard-base
           ghostel-viewport-editor--request))
      (error
       (message "ghostel-viewport-editor: could not fail helper: %s"
                (error-message-string error-data))))))

;;;; Ghostel integration

(defvar ghostel-viewport-editor-source-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'ghostel-viewport-editor-trigger)
    map)
  "Keymap active in Ghostel source buffers.")

(defvar-local ghostel-viewport-editor--source-input-locked nil)

(defvar ghostel-viewport-editor--source-lock-map
  (let ((map (make-keymap)))
    ;; A full keymap catches every character, including prefix keys, before
    ;; Ghostel's emulation maps can forward it to the terminal.
    (set-char-table-range
     (nth 1 map) t #'ghostel-viewport-editor--reject-source-input)
    (dolist (event '(down-mouse-1 mouse-1 drag-mouse-1
                     down-mouse-2 mouse-2 drag-mouse-2
                     down-mouse-3 mouse-3 drag-mouse-3
                     mouse-4 mouse-5 wheel-up wheel-down wheel-left wheel-right
                     drag-n-drop xterm-paste touch-end))
      (define-key map (vector event)
                  #'ghostel-viewport-editor--reject-source-input))
    map)
  "Keymap that rejects input while a source is waiting for its editor.")

(defun ghostel-viewport-editor--lock-source-window (request window)
  "Make REQUEST's source WINDOW ineligible for normal selection."
  (when (and (window-live-p window)
             (eq (window-buffer window)
                 (ghostel-viewport-editor--request-source-buffer request)))
    (unless (assq window
                  (ghostel-viewport-editor--request-source-window-parameters
                   request))
      (push
       (cons window (window-parameter window 'no-other-window))
       (ghostel-viewport-editor--request-source-window-parameters request)))
    (set-window-parameter window 'no-other-window t)))

(defun ghostel-viewport-editor--source-window-state-changed (window)
  "Keep an active source WINDOW locked and redirect selection from it."
  (when-let* ((request ghostel-viewport-editor--active-request)
              ((eq (ghostel-viewport-editor--request-phase request)
                   'accepted)))
    (ghostel-viewport-editor--lock-source-window request window)
    (when (eq window (selected-window))
      (ghostel-viewport-editor--reveal-request request))))

(defun ghostel-viewport-editor--source-post-command ()
  "Leave a locked source window after any command that selected it."
  (when (and ghostel-viewport-editor--source-input-locked
             ghostel-viewport-editor--active-request
             (eq (window-buffer (selected-window)) (current-buffer)))
    (ghostel-viewport-editor--reveal-request
     ghostel-viewport-editor--active-request)))

(defun ghostel-viewport-editor--reject-source-input ()
  "Reject input in a locked source and reveal its active viewport."
  (interactive)
  (when-let* ((request ghostel-viewport-editor--active-request))
    (ghostel-viewport-editor--reveal-request request))
  (message "Shell input is locked while its Ghostel viewport is active"))

(defun ghostel-viewport-editor--lock-source (request)
  "Prevent focus and input in REQUEST's source until it completes."
  (when-let* ((source
               (ghostel-viewport-editor--request-source-buffer request))
              ((buffer-live-p source)))
    (with-current-buffer source
      (unless ghostel-viewport-editor--source-input-locked
        (setf
         (ghostel-viewport-editor--request-source-overriding-local-map
          request)
         overriding-local-map
         (ghostel-viewport-editor--request-source-overriding-local-map-local-p
          request)
         (local-variable-p 'overriding-local-map))
        (setq ghostel-viewport-editor--source-input-locked t)
        (setq-local overriding-local-map
                    ghostel-viewport-editor--source-lock-map)
        (add-hook 'post-command-hook
                  #'ghostel-viewport-editor--source-post-command nil t)
        (add-hook 'window-state-change-functions
                  #'ghostel-viewport-editor--source-window-state-changed nil t))
      (dolist (window (get-buffer-window-list source nil t))
        (ghostel-viewport-editor--lock-source-window request window)))))

(defun ghostel-viewport-editor--unlock-source (request)
  "Restore focus and input state saved while locking REQUEST's source."
  (dolist (entry
           (ghostel-viewport-editor--request-source-window-parameters request))
    (when (window-live-p (car entry))
      (set-window-parameter (car entry) 'no-other-window (cdr entry))))
  (setf (ghostel-viewport-editor--request-source-window-parameters request)
        nil)
  (when-let* ((source
               (ghostel-viewport-editor--request-source-buffer request))
              ((buffer-live-p source)))
    (with-current-buffer source
      (setq ghostel-viewport-editor--source-input-locked nil)
      (remove-hook 'post-command-hook
                   #'ghostel-viewport-editor--source-post-command t)
      (remove-hook 'window-state-change-functions
                   #'ghostel-viewport-editor--source-window-state-changed t)
      (if (ghostel-viewport-editor--request-source-overriding-local-map-local-p
           request)
          (setq-local
           overriding-local-map
           (ghostel-viewport-editor--request-source-overriding-local-map
            request))
        (kill-local-variable 'overriding-local-map)))))

(define-minor-mode ghostel-viewport-editor-source-mode
  "Expose viewport editor commands in one Ghostel buffer."
  :init-value nil
  :lighter nil
  :keymap ghostel-viewport-editor-source-mode-map)

(defun ghostel-viewport-editor--reveal-request (request)
  "Display and select REQUEST's live viewport, returning its window."
  (when-let* ((viewport
               (ghostel-viewport-editor--request-viewport-buffer request))
              ((buffer-live-p viewport)))
    (let ((window
           (or (get-buffer-window viewport (selected-frame))
               (funcall
                ghostel-viewport-editor-display-function
                viewport
                (ghostel-viewport-editor--request-anchor-window
                 (ghostel-viewport-editor--request-source-buffer request))))))
      (when (and (window-live-p window)
                 (eq (window-buffer window) viewport)
                 (frame-visible-p (window-frame window)))
        (select-frame-set-input-focus (window-frame window))
        (select-window window))
      (and (window-live-p window)
           (eq (window-buffer window) viewport)
           (frame-visible-p (window-frame window))
           window))))

;;;###autoload
(defun ghostel-viewport-editor-trigger ()
  "Reveal an active viewport or request one from the foreground program."
  (interactive)
  (unless ghostel-viewport-editor-global-mode
    (user-error "Ghostel viewport editor global mode is disabled"))
  (let ((request ghostel-viewport-editor--active-request))
    (if (and request
             (eq (ghostel-viewport-editor--request-phase request) 'accepted)
             (buffer-live-p
              (ghostel-viewport-editor--request-viewport-buffer request)))
        (if (ghostel-viewport-editor--reveal-request request)
            (message "Revealed the active Ghostel viewport")
          (user-error "Could not display the active Ghostel viewport"))
      (ghostel-send-string "\C-x\C-e"))))

(defun ghostel-viewport-editor--setup-source-buffer ()
  "Enable source commands in a Ghostel buffer."
  (when ghostel-viewport-editor-global-mode
    (ghostel-viewport-editor-source-mode 1)))

(defun ghostel-viewport-editor--install ()
  "Install the package's public Ghostel adapters."
  (ghostel-viewport-editor--ensure-supported)
  (add-hook 'ghostel-pre-spawn-hook
            #'ghostel-viewport-editor--inject-environment)
  (add-hook 'ghostel-mode-hook
            #'ghostel-viewport-editor--setup-source-buffer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'ghostel-mode)
        (ghostel-viewport-editor-source-mode 1)))))

(defun ghostel-viewport-editor--uninstall ()
  "Stop injecting routing into future Ghostel processes."
  (remove-hook 'ghostel-pre-spawn-hook
               #'ghostel-viewport-editor--inject-environment)
  (remove-hook 'ghostel-mode-hook
               #'ghostel-viewport-editor--setup-source-buffer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when ghostel-viewport-editor-source-mode
        (ghostel-viewport-editor-source-mode -1)))))

;;;###autoload
(define-minor-mode ghostel-viewport-editor-global-mode
  "Route external-editor calls from new local Ghostel processes into Emacs."
  :global t
  :group 'ghostel-viewport-editor
  (if ghostel-viewport-editor-global-mode
      (condition-case error-data
          (ghostel-viewport-editor--install)
        (error
         (setq ghostel-viewport-editor-global-mode nil)
         (ignore-errors (ghostel-viewport-editor--uninstall))
         (signal (car error-data) (cdr error-data))))
    (ghostel-viewport-editor--uninstall)))

;; A development `load-file' preserves the global minor-mode variable.  Re-run
;; the idempotent installer so the new definitions take effect without adding
;; duplicate hooks or OSC commands.
(when ghostel-viewport-editor-global-mode
  (ghostel-viewport-editor--install))

(provide 'ghostel-viewport-editor)
;;; ghostel-viewport-editor.el ends here
