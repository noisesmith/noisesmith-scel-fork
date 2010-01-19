(require 'cl)
(require 'ext-scel)
(defvar scel-unit-tests '())
(defvar scel-all-unit-tests)
(defvar scel-unit-test-file)

(defvar scel-unit-test-file-result
  "a ScelDocument(\\*\\*\\*\\(scel-unit-test.*\\.sc\\)\\*\\*\\*)")

(push
 (list "_documentOpen new with 0 args"
       (lambda ()
	 (concat "Document.open(\"" (make-temp-name "scel-unit-test") ".sc\")"))
       (lambda (res)
	 (and (string-match scel-unit-test-file-result res)
	      (get-buffer (substring res (nth 2 (match-data))
				     (nth 3 (match-data)))))))
 scel-unit-tests)

(push
 (list "_documentOpen new with 1 arg"
       (lambda ()
	 (concat "Document.open(\""
		 (make-temp-name "scel-unit-test") ".sc\", 100)"))
       (lambda (res)
	 (and (string-match scel-unit-test-file-result res)
	      (get-buffer (substring res (nth 2 (match-data))
				     (nth 3 (match-data)))))))
 scel-unit-tests)

(push
 (list "_documentOpen new with 2 args"
       (lambda ()
	 (concat "Document.open(\""
		 (make-temp-name "scel-unit-test") ".sc\", 100, 10)"))
       (lambda (res)
	 (and (string-match scel-unit-test-file-result res)
	      (setq scel-unit-test-file (substring res (nth 2 (match-data))
					   (nth 3 (match-data))))
	      (get-buffer scel-unit-test-file))))
 scel-unit-tests)

(push
 (list "_documentOpen existing with no args"
       (lambda ()
	 (with-current-buffer scel-unit-test-file
	   (insert "0123456789")
	   (save-buffer)
	   (kill-buffer))
	 (concat "Document.open(\"" scel-unit-test-file "\")"))
       (lambda (res)
	 (with-current-buffer scel-unit-test-file
	   (and (string-match scel-unit-test-file-result res)
		(equal "0123456789"
		       (buffer-substring-no-properties 1 (point-max)))
		(kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentOpen existing with 1 arg"
       (lambda ()
	(concat "Document.open(\"" scel-unit-test-file "\", 5)"))
       (lambda (res)
	 (with-current-buffer scel-unit-test-file
	   (and (string-match scel-unit-test-file-result res)
		(equal "5"
		       (buffer-substring-no-properties (point) (+ 1 (point))))
		(kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentOpen existing with 2 args"
       (lambda ()
	 (concat "Document.open(\"" scel-unit-test-file "\", 5, 4)"))
       (lambda (res)
	 (with-current-buffer scel-unit-test-file
	   (and (string-match scel-unit-test-file-result res)
		(equal "5678"
		       (buffer-substring-no-properties (point) (mark)))
		(kill-buffer)))))
 scel-unit-tests)

(push
 (list "cleanup"
       (lambda ())
       (lambda (res)
	 (delete-file scel-unit-test-file)
	 t))
 scel-unit-tests)

(defun scel-run-all-unit-tests ()
  (interactive)
  (setq scel-all-unit-tests (nreverse (copy-list scel-unit-tests)))
  (scel-run-unit-tests))

(defun scel-run-unit-tests ()
  (let ((unit-test (pop scel-all-unit-tests))
	 handler command command-str test)
    (if (not unit-test)
	(message "Unit tests done, all passed.")
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
		   ',handler ,command-str result))))))
  "")

(provide 'scel-unit-tests)