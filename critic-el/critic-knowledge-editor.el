(setq critic-knowledge-editor-mode-map nil)

(defvar critic-knowledge-editor-mode-map nil
 "Keymap for Critic mode.")

(let ((map (make-keymap)))
 (define-key map "q" 'critic-knowledge-editor-quit)
 (setq critic-knowledge-editor-mode-map map))

(defun critic-knowledge-editor-quit ()
 ""
 (interactive)
 )

(defun critic-knowledge-editor-mode (item)
 ""
 (interactive)
 (let ((buffer (get-buffer-create (concat "all-asserted: " item))))
  (set-buffer buffer)
  (insert (shell-command-to-string
	   (concat 
	    "/var/lib/myfrdcsa/codebases/releases/freekbs-0.2/freekbs-0.2/scripts/all-asserted-knowledge-2.pl "
	    item)))
  (beginning-of-buffer)
  (switch-to-buffer buffer)
  )
 )
