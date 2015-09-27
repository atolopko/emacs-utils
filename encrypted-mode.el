(define-derived-mode encrypted-mode text-mode "Encrypted"
  "Major mode used for editing gpg-encrypted files.
\\{encrypted-mode-map}"
  (auto-save-mode nil)
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)
  (setq encrypt-buffer-file-name buffer-file-name)
  (setq buffer-file-name nil)
  (setq buffer-file-truename nil)
  (encrypt-decrypt-buffer))

(define-key encrypted-mode-map [(control x)(control s)] 'encrypt-save-file)

(defvar encrypt-buffer-file-name nil "The file name associated with
the buffer, taken from buffer-file-name before it is set to nil" )
(make-variable-buffer-local 'encrypt-buffer-file-name)

(defun encrypt-decrypt-buffer ()
  (pgg-decrypt-region (point-min) (point-max))
  (pgg-display-output-buffer (point-min) (point-max) t))

(defun encrypt-save-file ()
  (interactive)
  ;; note: (save-excursion) doesn't work, so we save & restore point manually
  (let ((current-point (point)))
    (call-interactively 'pgg-encrypt)
    (write-region (point-min) (point-max) encrypt-buffer-file-name)
    (encrypt-decrypt-buffer)
    (goto-char current-point)))

(provide 'encrypted-mode)