(require 'cl)
(require 'ext-scel)
(defvar scel-unit-tests '())
(defvar scel-all-unit-tests)

(push
 (list "_documentOpen new with 0 args"
       (lambda () (concat "~testdoc = Document.open(\""
			  (make-temp-name "sclang-doc")
			  ".sc\")"))
       (lambda (res)
	 (and (string-match
	       "a ScelDocument(\\*\\*\\*\\(sclang-doc.*\\.sc\\)\\*\\*\\*)"
	       res)
	      (get-buffer (substring res (nth 2 (match-data))
				     (nth 3 (match-data))))
	      "Done\n")))
 scel-unit-tests)

(push
 (list "_documentOpen new with 1 arg"
       (lambda () (concat "~testdoc = Document.open(\""
			  (make-temp-name "sclang-doc")
			  ".sc\", 100)"))
       (lambda (res)
	 (and (string-match
	       "a ScelDocument(\\*\\*\\*\\(sclang-doc.*\\.sc\\)\\*\\*\\*)"
	       res)
	      (get-buffer (substring res (nth 2 (match-data))
				     (nth 3 (match-data))))
	      "Done\n")))
 scel-unit-tests)

(push
 (list "_documentOpen new with 2 args"
       (lambda () (concat "~testdoc = Document.open(\""
			  (make-temp-name "sclang-doc")
			  ".sc\", 100, 10)"))
       (lambda (res)
	 (and (string-match
	       "a ScelDocument(\\*\\*\\*\\(sclang-doc.*\\.sc\\)\\*\\*\\*)"
	       res)
	      (get-buffer (substring res (nth 2 (match-data))
				     (nth 3 (match-data))))
	      "Done\n")))
 scel-unit-tests)


(defun scel-run-all-unit-tests ()
  (interactive)
  (setq scel-all-unit-tests (nreverse (copy-list scel-unit-tests)))
  (scel-run-unit-tests))

(defun scel-run-unit-tests ()
  (let ((unit-test (pop scel-all-unit-tests))
	 handler command command-str test)
    (if unit-test
	(progn
	  (message "Starting unit test for %S" (car unit-test))
	  (setq handler (car unit-test)
		command (cadr unit-test)
		command-str (funcall command)
		test (caddr unit-test))
	  (sclang-eval-string-with-hook
	   command-str
	   `(lambda (result)
	      (if (funcall ,test result)
		  (progn (message "Unit test for %S passed." ',handler)
			 (scel-run-unit-tests))
		(error "%S handler test failed on %S with %S"
		       ',handler ,command-str result)))))
      (message "Unit tests done, all passed."))))

(provide 'scel-unit-tests)