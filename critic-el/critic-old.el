(tags (list
	     '("annecdote" . "a")
	     '("capability-request" . "cr")
	     '("complex-statement" . "cs")
	     '("dangling-clause" . "dc")
	     '("deleted" . "de")
	     '("dream" . "dr")
	     '("event" . "e")
	     '("goal" . "g")
	     '("icodebase-capability-request" . "ic")
	     '("icodebase-input-data" . "ii")
	     '("icodebase-resource" . "ir")
	     '("icodebase-solution-to-extant-problem" . "is")
	     '("icodebase-task" . "it")
	     '("inspiring-annecdote" . "ia" )
	     '("intersystem-relation" . "ie")
	     '("not-a-unilang-client-entry" . "n")
	     '("observation" . "o")
	     '("policy" . "pol")
	     '("poem" . "poe")
	     '("political-action-item" . "pa")
	     '("priority-shift" . "ps")
	     '("propaganda" . "pr")
	     '("quote" . "q")
	     '("rant" . "r")
	     '("shopping-list-item" . "sl")
	     '("solution-to-extant-problem" . "se")
	     '("SOP" . "so")
	     '("suspicion" . "su")
	     '("system-request" . "sy" )
	     '("unclassifiable" . "uc")
	     '("unilang-client-outgoing-message" . "um")
	     '("verber-task-definition" . "v")
	     ))

 (mapcar (lambda (item)
	  (eval
	   (read
	   (concat 
	    "(progn
	    (defun critic-unilang-classification-" (car item) " ()
	    \"" (car item) "\"
	    (interactive)
	    (freekbs-assert-relation
	     (list
	      \"unilang-message-classify\"
	      (critic-get-entry) \""
	      (car item)
	    "\")))
	   (define-key map \""
	    (concat "t" (cdr item))
	    "\" 'critic-unilang-classification-" (car item)
	    "))"))))
  tags)