;;; critic-mode.el

;; (load "/var/lib/myfrdcsa/codebases/internal/critic/critic-knowledge-editor.el")
(global-set-key "\C-cCk" 'critic-knowledge-editor)
(global-set-key "\C-cCv" 'critic-unilang-view-recent-entries)
(global-set-key "\C-cCg" 'critic-unilang-view-recent-goals)
(global-set-key "\C-cCc" 'critic-critique-entries)
(global-set-key "\C-cCKe" 'critic-ke-edit)
(global-set-key "\C-cCKu" 'critic-ke-unassert)
(global-set-key "\C-cCt" 'critic-classify)

(setq auto-mode-alist
 (cons '("\\.critic\\'" . critic-mode) auto-mode-alist))

(defvar critic-mode-map nil
 "Keymap for Critic mode.
 Many other modes, such as Mail mode, Outline mode and Indented Critic mode,
 inherit all the commands defined in this map.")

(let ((map (make-keymap)))
 (define-key map "pc" 'pse-completed)
 (define-key map "pC" 'pse-query-completed)
 (define-key map "pi" 'pse-incomplete)
 (define-key map "po" 'pse-comment)
 (define-key map "ps" 'pse-solution)
 (define-key map "pb" 'pse-belongs-to)
 (define-key map "sa" 'freekbs-assert-relation)
 (define-key map "su" 'freekbs-assert-relation)
 (define-key map "s." 'freekbs-push-entry-at-point-onto-stack)
 (define-key map "sc" 'freekbs-clear-stack)
 (define-key map "t" 'critic-classify)
 (define-key map "k" 'critic-knowledge-editor)
 (setq critic-mode-map map))

(defvar critic-mode-syntax-table nil
 "Syntax table used while in text mode.")
     
(defun critic-mode ()
 "Major mode for editing text intended for humans to read...
      Special commands: \\{critic-mode-map}
     Turning on critic-mode runs the hook `critic-mode-hook'."
 (interactive)
 (kill-all-local-variables)
 (use-local-map critic-mode-map)
 ; (setq local-abbrev-table critic-mode-abbrev-table)
					; (set-syntax-table critic-mode-syntax-table)
 (setq mode-name "Critic")
 (setq major-mode 'critic-mode)
 (run-hooks 'critic-mode-hook))	   ; Finally, this permits the user to

(defun critic-next ()
 "Jump to next item in list"
 (interactive)
 (next-line))

(defun critic-previous ()
 "Jump to previous item in list"
 (interactive)
 (previous-line))

(defun critic-get-entry ()
 ""
 (pse-get-item))

(defun critic-search ()
 "search for matching items")

(defun critic-rate ()
 "")


(defun critic-knowledge-editor ()
 ""
 (interactive)
 ;; open up a new window which shows the classifications
 (let* ((item (critic-get-entry))
	(buffer (get-buffer-create (concat "all-asserted: " item))))
  (pop-to-buffer buffer)
  (erase-buffer)
  (insert (shell-command-to-string
	   (concat 
	    "/var/lib/myfrdcsa/codebases/releases/freekbs-0.2/freekbs-0.2/scripts/all-asserted-knowledge-2.pl "
	    item)))
  (beginning-of-buffer)
  )
 )

(defun critic-ke-assert ()
 ""
 (interactive)
 )

(defun critic-ke-edit ()
 ""
 (interactive)
 ;; first get the entry ID, then bring it up, then edit it
 (let ((id (freekbs-get-id-of-assertion-at-point)))
  (if (numberp (read id))
   ()
   )
  )
 )

(defun critic-ke-unassert ()
 ""
 (interactive)
 ;; first get the entry ID, then bring it up, then edit it
 (let ((id (freekbs-get-id-of-assertion-at-point)))
  (if (numberp (read id))
   (if (y-or-n-p (concat "Unassert assertion id: " id " "))
    (freekbs-send (concat "unassert-by-id " id))
    )
   )
  )
 )

(defun critic-relate ()
 "")

(defun critic-compare ()
 "")

(defun critic-reload ()
 "")

(defun critic-resort ()
 "")

;; selection algebra

(defun critic-select ()
 "")

(defun critic-deselect ()
 "")

(defun critic-select-all ()
 "")

(defun critic-deselect-all ()
 "")

(defun critic-select-region ()
 "")

(defun critic-deselect-region ()
 "")

(defun critic-push-up ()
 "")

(defun critic-push-down ()
 "")

(defun critic-view ()
 "")

(defun critic-classify ()
 ""
 (interactive)
 (let ((critic-unilang-class-list (uea-query-agent "query-cyclike (\"critic-unilang-class\" ?X)" "KBS")))
  (freekbs-assert-relation
   (list 
    "critic-unilang-classification"
    (critic-get-entry)
    (completing-read "Class: " (mapcar 'cdr (mapcar 'car critic-unilang-class-list)))))
  ))

(defun non-nil (arg)
 (if (symbolp arg)
  (and (boundp arg)
   (not (equal arg nil)))
  t))

(defun non-nil-2 (arg)
 (not (not arg)))

(defun critic-unilang-view-recent-entries (&optional depth search)
 ""
 (interactive "p")
 (if (= depth 1)
  (setq depth 100))
 (pop-to-buffer (get-buffer-create "Critic-Unilang"))
 (erase-buffer)
 (insert
  ;; (shell-command-to-string 
   (concat 
    "corpus -s "
    (if (non-nil search)
     (shell-quote-argument search)
     ".")
    " -d " 
    (number-to-string depth)
    " --k2 "
    freekbs2-context
    ))
 (critic-mode))

(defun critic-unilang-view-recent-goals ()
 ""
 (interactive)
 (pop-to-buffer (get-buffer-create (concat "Critic-Unilang-Goals " freekbs-context)))
 (erase-buffer)
 (insert
  (shell-command-to-string 
   (concat 
    "corpus -s "
    "."
    " -k "
    freekbs-context
    " -g "
    )))
 (critic-mode))

(defun critic-critique-entries ()
 ""
 (interactive)
 ;; what this is going to do is to allow the user to search some
 ;; existing set of entries.  first, depending on how you ask it, it
 ;; removes the entries that you've already seen (or it just displays
 ;; them under a "seen" heading or with some markup that they have
 ;; been seen.
 ;; 
 ;; Then it allows you to make assertions, and reload if you wish the
 ;; contents, finally you can mark which sections you have reviewed.
 ;; note that you can mark it as reviewed wrt a specific purpose.

 ;; this same review system should actually apply to knowledge as well
 (if (= depth 1)
  (setq depth 100))
 (pop-to-buffer (get-buffer-create "Critic-Unilang"))
 (erase-buffer)
 (insert
  (shell-command-to-string 
   (concat 
    "corpus -s "
    (if (non-nil search)
     (shell-quote-argument search)
     ".")
    " -d " 
    (number-to-string depth)
    " -k"
    )))
 (critic-mode))
