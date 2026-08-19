;;; checkdoc-batch.el --- Portable batch Checkdoc runner  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run Checkdoc in batch mode on Emacs versions that predate
;; `checkdoc-batch'.

;;; Code:

(require 'checkdoc)

(defun ghostel-viewport-editor-checkdoc-batch (file)
  "Run Checkdoc on FILE, exiting unsuccessfully for any diagnostics."
  (let ((checkdoc-diagnostic-buffer
         "*ghostel-viewport-editor-checkdoc*")
        diagnostics)
    (when (get-buffer checkdoc-diagnostic-buffer)
      (kill-buffer checkdoc-diagnostic-buffer))
    (with-current-buffer (find-file-noselect file)
      (checkdoc-current-buffer t))
    (with-current-buffer checkdoc-diagnostic-buffer
      (goto-char (point-min))
      (when (re-search-forward "^.+\\.el:[0-9]+: " nil t)
        (setq diagnostics (buffer-string))))
    (when diagnostics
      (princ diagnostics)
      (kill-emacs 1))))

(ghostel-viewport-editor-checkdoc-batch
 (or (getenv "CHECKDOC_FILE") "ghostel-viewport-editor.el"))

(provide 'ghostel-viewport-editor-checkdoc-batch)
;;; checkdoc-batch.el ends here
