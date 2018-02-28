; get the appropriate map working

(define-key mew-summary-mode-map "bi" 'critic-mew-flag-as)

(defun critic-mew-get-unique-identifier-for-email-summary-at-point (arg)
 ""
 (interactive "P")
 (when (mew-sumsyn-match mew-regex-sumsyn-long)
  (let* ((fld (mew-sumsyn-folder-name))
	 (msg (mew-sumsyn-message-number))
	 (size (mew-sumsyn-message-size))
	 (uid (mew-sumsyn-message-uid))
	 (file (concat (file-name-as-directory fld) msg))
	 (path (mew-expand-folder file)))
   (setq path (mew-msg-get-filename path))
   (setq file (file-name-nondirectory path))
   file)))

(defun critic-mew-flag-as (flag)
 ""
 (interactive)
 (let ((critic-mew-message-id (critic-mew-get-unique-identifier-for-email-summary-at-point)))
  (if (numberp (read critic-mew-message-id))
   (freekbs-assert
    (list "mew-message-classify" critic-mew-message-id flag)))))