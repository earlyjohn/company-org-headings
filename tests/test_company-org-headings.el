(defvar /path/to/mock-org-file1
  "* Heading 1
** Subheading 1
*** Subsubheading 1
* Heading 2
** Subheading 2
")
(defvar /path/to/mock-org-file2
  "* Heading 3
** Subheading 3
*** Subsubheading 3
**** Subsubsubheading 3
***** Subsubsubsubheading 3
")

(defmacro with-mock-alist (&rest body)
  `(cl-letf (((symbol-function #'directory-files)
	      (lambda (&rest args) '("/path/to/mock-org-file1"
				"/path/to/mock-org-file2")))
	     ((symbol-function #'file-exists-p) (lambda (f) t))
	     ((symbol-function #'insert-file-contents)
	      (lambda (&rest args) (insert (symbol-value (read file))))))
     (let* ((company-org-headings/search-directory ".")
	    (company-org-headings/alist nil))
       (progn
	 (company-org-headings/collect-data)
	 ;; mock recursion; should append the elements of file2
	 (company-org-headings/collect-data))
       ,@body)))

;; (macroexpand (with-mock-alist (message "%s" company-org-headings/alist)))

(defmacro with-mock-timer (&rest body)
  `(cl-letf (((symbol-function #'cancel-timer) (lambda (f) t)))
     ,@body
     ;; remove all created idle timers
     (delete company-org-headings/idle-timer timer-idle-list)))

(ert-deftest test-aggregate-headings ()
  "Check length of mock alist."
  (with-mock-timer
   (with-mock-alist
    (should (= 10 (length company-org-headings/alist))))))

(ert-deftest test-append ()
  "Check whether the alist adds one unique heading."
  (with-mock-timer
   (with-mock-alist
    (let ((before (length company-org-headings/alist))
	  (appendone
	   (lambda ()
	     (with-temp-buffer
	       (cl-letf
		   (((symbol-function
		      #'company-org-headings/in-search-dir-and-org-p)
		     (lambda (f) t))
		    ((symbol-function #'insert-file-contents)
		     (lambda (f) (insert "* Append Heading\n"))))
		 (company-org-headings/append-current-file-headings))))))
      (funcall appendone)
      (should (= (1+ before) (length company-org-headings/alist)))
      (funcall appendone)
      ;; should still have the length of 11 since the heading already
      ;; exists
      (should (= (1+ before) (length company-org-headings/alist)))))))

(ert-deftest test-length-parse-msg ()
  "Message length shouldn't exceed 72 characters."
  (let ((company-org-headings/parse-with-idle-time 300))
    (should
     (<= (string-width (company-org-headings/parse-message
			;; assuming an exaggerated parsing time of 30
			;; secs
			(+ company-org-headings/parse-with-idle-time 60)))
	 72))
    ;; msg length should stay the same no matter how long we parse
    (should
     (= (string-width
	 (company-org-headings/parse-message
	  (+ company-org-headings/parse-with-idle-time 1)))
	(string-width
	 (company-org-headings/parse-message
	  (+ company-org-headings/parse-with-idle-time 20)))))))
