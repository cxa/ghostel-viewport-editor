;;; lint-setup.el --- Package metadata for release linting -*- lexical-binding: t; -*-

;;; Commentary:

;; Teach package-lint about the declared minimum Ghostel dependency without
;; installing Ghostel's native terminal module in the unit-test job.

;;; Code:

(setq package-user-dir
      (expand-file-name
       "fixtures/packages/"
       (file-name-directory (or load-file-name buffer-file-name))))
(require 'package)

;;; lint-setup.el ends here
