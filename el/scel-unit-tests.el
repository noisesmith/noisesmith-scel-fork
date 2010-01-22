(require 'cl)
(require 'ext-scel)
(defvar scel-unit-tests '())
(defvar scel-all-unit-tests)
(defvar scel-unit-test-file)
(defvar scel-failed-tests '())

(defvar scel-unit-test-file-result
  "a ScelDocument(\\*\\*\\*\\(scel-unit-test.*\\.sc\\)\\*\\*\\*)")

;;; these next two functions are the main engine of the testing system
(defun scel-run-all-unit-tests ()
  (interactive)
  ;; this is so that the pops in scel-run-unit-tests do not destroy the original
  (setq scel-all-unit-tests scel-unit-tests)
  (scel-run-unit-tests))

(defun scel-run-unit-tests ()
;; run the next test in the list, and when it's callback is finalized,
;; tail call this function again.
  (let ((unit-test (pop scel-all-unit-tests))
	 handler command command-str test)
    (if (not unit-test)
	(if (not scel-failed-tests)
	    (message "Unit tests done, all passed.")
	  (dolist (test (nreverse scel-failed-tests))
	    (lwarn '(sclang) :warning "failed unit test %s" test)))
      (message "Starting unit test for %S" (car unit-test))
      (setq handler (car unit-test)
	    command (cadr unit-test)
	    command-str (funcall command)
	    test (caddr unit-test))
      (sclang-eval-string-with-hook
       command-str
       `(lambda (result)
	  (let ((test-results (funcall ,test result)))
	    (if (not (apply 'every (list 'identity test-results)))
		(push (format "%S\n%S\n%S\n%S\n" ,handler ,command-str result
			      test-results)
		      scel-failed-tests)
	      (message "Unit test for %S passed." ',handler))
	    (scel-run-unit-tests))))))
    "")

;;; _documentOpen

(push
 (list "_documentOpen new with 0 args"
       (lambda ()
	 (concat "Document.open(\"" (make-temp-name "scel-unit-test") ".sc\")"))
       (lambda (res)
	 (list
	  "return value is correct"
	  (string-match scel-unit-test-file-result res)
	  "buffer exists"
	  (and (get-buffer (substring res (nth 2 (match-data))
				      (nth 3 (match-data))))
	       (with-current-buffer (substring res (nth 2 (match-data))
					       (nth 3 (match-data)))
		 (kill-buffer))))))
 scel-unit-tests)

(push
 (list "_documentOpen new with 1 arg"
       (lambda ()
	 (concat "Document.open(\""
		 (make-temp-name "scel-unit-test") ".sc\", 100)"))
       (lambda (res)
	 (list
	  "return value is correct"
	  (string-match scel-unit-test-file-result res)
	  "buffer exists"
	  (and (get-buffer (substring res (nth 2 (match-data))
				      (nth 3 (match-data))))
	       (with-current-buffer (substring res (nth 2 (match-data))
					       (nth 3 (match-data)))
		 (kill-buffer))))))
 scel-unit-tests)

(push
 (list "_documentOpen new with 2 args"
       (lambda ()
	 (concat "Document.open(\""
		 (make-temp-name "scel-unit-test") ".sc\", 100, 10)"))
       (lambda (res)
	 (list
	  "return value is correct"
	  (string-match scel-unit-test-file-result res)
	  "buffer exists"
	  (setq scel-unit-test-file (substring res (nth 2 (match-data))
					       (nth 3 (match-data))))
	  "buffer assigned"
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
	   (list
	    "return value is correct"
	    (string-match scel-unit-test-file-result res)
	    "correct file contents"
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
	   (list
	    "return value is correct"
	    (string-match scel-unit-test-file-result res)
	    "position set properly"
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
	   (list
	    "return value is correct"
	    (string-match scel-unit-test-file-result res)
	    "region activated properly"
	    (equal "5678"
		   (buffer-substring-no-properties (point) (mark)))
	    (kill-buffer)))))
 scel-unit-tests)

;;; _documentNew

(push
 (list "_documentNew no args"
       (lambda () "Document.new()")
       (lambda (res)
	 (with-current-buffer "Untitled"
	   (list
	    "return value is correct"
	    (string-match "a ScelDocument(\\*\\*\\*Untitled\\*\\*\\*)" res)
	    "postbuffer has not been redefined"
	    (not (equal (sclang-get-post-buffer) (get-buffer "Untitled")))
	    "buffer has been made visible"
	    ;; this sit-for is neccessary -- because?
	    (progn (sit-for 0.2) (> buffer-display-count 0))
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentNew all args"
       (lambda ()
	 "Document.new(\"Also Untitled\", \"0123456789\", true, false)")
       (lambda (res)
	 (with-current-buffer "Also Untitled"
	   (list
	    "buffer name"
	    (string-match "a ScelDocument(\\*\\*\\*Also Untitled\\*\\*\\*"
			  res)
	    "buffer contents"
	    (equal "0123456789"
		   (buffer-substring-no-properties 1 (point-max)))
	    ;; reassign post buffer is failing
	    "set as postbuffer"
	    (equal (sclang-get-post-buffer) (get-buffer "Also Untitled"))
	    "not visible"
	    (= buffer-display-count 0)
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentClose"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.prclose"))
       (lambda (res)
	 (list
	  "buffer is closed"
	  (not (get-buffer "testdoc.sc")))))
 scel-unit-tests)

(push
 (list "_documentRename"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.title_(Document.current.title++\"renamed.sc\");"))
       (lambda (res)
	 (list
	  "buffer is renamed"
	  (and (get-buffer "testdoc.screnamed.sc")
	       (kill-buffer "testdoc.screnamed.sc")))))
 scel-unit-tests)

(push
 (list "_documentSetEditable"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	 "Document.current.editable_(false);"))
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list
	    "buffer is read only"
	    (eq buffer-read-only t)
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentSwitchTo"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t))
	 (switch-to-buffer "*scratch*")
	 (delete-other-windows)
	 "Document.current.front;")
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list
	    "buffer is visible"
	    (> buffer-display-count 0)
	    (kill-buffer)))))
 scel-unit-tests)

;;; _documentPutString

(push
 (list "_documentPutString one arg"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (erase-buffer)
	   (insert "old")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.string_(\"new\");"))
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list ; failing during test, but succeeds if run on its own
	    "newld"
	    (buffer-substring-no-properties 1 (point-max))
	    "contents of buffer correct"
	    (equal "newld" (buffer-substring-no-properties 1 (point-max)))
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentPutString two args"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (erase-buffer)
	   (insert "old")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.string_(\"new\", 1);"))
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list
	    "onewd"
	    (buffer-substring-no-properties 1 (point-max))
	    "contents of buffer correct"
	    (equal "onewd" (buffer-substring-no-properties 1 (point-max)))
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentPutString three args"
       (lambda ()
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (erase-buffer)
	   (insert "old")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.string_(\"new\", 0, 3);"))
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list
	    "new"
	    (buffer-substring-no-properties 1 (point-max))
	    "contents of buffer correct"
	    (equal "new" (buffer-substring-no-properties 1 (point-max)))
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "_documentPopTo"
       (lambda ()
	 (delete-other-frames)
	 (delete-other-windows)
	 (display-buffer "*scratch*")
	 (with-current-buffer (get-buffer-create "testdoc.sc")
	   (sclang-mode)
	   (sclang-set-current-document (get-buffer "testdoc.sc") t)
	   "Document.current.unfocusedFront"))
       (lambda (res)
	 (with-current-buffer "testdoc.sc"
	   (list
	    "buffer visible"
	    (> buffer-display-count 0)
	    (kill-buffer)))))
 scel-unit-tests)

(push
 (list "cleanup"
       (lambda ())
       (lambda (res)
	 (delete-file scel-unit-test-file)
	 '(t)))
 scel-unit-tests)

(setq scel-unit-tests (nreverse scel-unit-tests))


(provide 'scel-unit-tests)