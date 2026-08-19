;;; package-smoke.el --- Clean package installation smoke test -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Install the single-file package into an isolated package-user-dir and
;; verify that its public autoloads are generated without loading the package.

;;; Code:

(require 'package)

(let* ((root (make-temp-file "ghostel-viewport-package-" t))
       (package-user-dir (expand-file-name "elpa" root))
       (ghostel-directory
        (expand-file-name "ghostel-0.35.0" package-user-dir))
       (source
        (expand-file-name "ghostel-viewport-editor.el" default-directory))
       (package-alist nil)
       (package-activated-list nil)
       (package-archives nil))
  (unwind-protect
      (progn
        (make-directory ghostel-directory t)
        (with-temp-file (expand-file-name "ghostel-pkg.el" ghostel-directory)
          (insert "(define-package \"ghostel\" \"0.35.0\" \"Test stub\")\n"))
        (with-temp-file
            (expand-file-name "ghostel-autoloads.el" ghostel-directory)
          (insert ";;; ghostel-autoloads.el -*- lexical-binding: t; -*-\n"
                  "(provide 'ghostel-autoloads)\n"))
        (package-initialize)
        (package-install-file source)
        (unless (package-installed-p 'ghostel-viewport-editor '(0 1 0))
          (error "ghostel-viewport-editor 0.1.0 was not installed"))
        (dolist (function '(ghostel-viewport-editor-environment
                            ghostel-viewport-editor-enable-current-shell
                            ghostel-viewport-editor-finish
                            ghostel-viewport-editor-cancel
                            ghostel-viewport-editor-trigger
                            ghostel-viewport-editor-global-mode))
          (unless (autoloadp (symbol-function function))
            (error "%S has no installed autoload" function))))
    (delete-directory root t)))

;;; package-smoke.el ends here
