(require 'ext-scel)
(defvar scel-unit-tests '())

(push
 (let ((name (concat (make-temp-name "sclang-doc") ".sc")))
   (list '_documentOpen
	 (concat "~testdoc = Document.open(\"" name "\")")
	 (concat "a ScelDocument(***" name "***)")))
 scel-unit-tests)

(defun scel-run-unit-tests ()
  (interactive)
  (let ((unit-test (pop scel-unit-tests))
	 handler command result-re)
    (if unit-test
	(progn
	  (setq handler (car unit-test)
		command (cadr unit-test)
		result-re (caddr unit-test))
	  (sclang-eval-string-with-hook
	   command
	   (lambda (result)
	     (if (string-match result-re result)
		 (progn (message "Unit test for %S passed." handler)
			(sclang-run-unit-tests))
	       (error
		"%S handler failed to match %S with %S in test code: %S"
		handler result-re  result command)))))
      (message "Unit tests done, all passed."))))

(provide 'scel-unit-tests)