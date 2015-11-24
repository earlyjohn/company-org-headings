(defvar /path/to/mock-org-file1
  "* Heading 1
** Subheading 1
*** Subsubheading 1
* Heading 1
** Subheading 1
")
(defvar /path/to/mock-org-file2
  "* Heading 2
** Subheading 2
*** Subsubheading 2
**** Subsubsubheading 2
***** Subsubsubsubheading 2
")

(defmacro mock-alist (&rest body)
  `(flet ((file-directory-p (f) t)
	  (directory-files (&rest args) '("/path/to/mock-org-file1"
					  "/path/to/mock-org-file2"))
	  (file-exists-p (f) t)
	  (insert-file-contents
	   (&rest args)
	   (insert (symbol-value (read files)))))
     (let ((company-org-headings/alist
	    (company-org-headings/aggregate-headings-in-dir ".")))
       ,@body)))

;; (macroexpand (mock-alist (message "%s" company-org-headings/alist)))

(ert-deftest test-aggregate-headings ()
  "Check length of mock alist."
  (mock-alist
   (should (= 10 (length company-org-headings/alist)))))

(ert-deftest test-append ()
  "Check whether the alist adds one unique heading."
  (mock-alist
   (let ((before (length company-org-headings/alist))
	 (appendone
	  (lambda ()
	    (with-temp-buffer
	      (flet ((company-org-headings/in-search-dir-and-org-p (file) t)
		     (insert-file-contents (file) (insert "* Append Heading\n")))
		(company-org-headings/append-current-buffer-file-headings))))))
     (funcall appendone)
     (should (= (1+ before) (length company-org-headings/alist)))
     (funcall appendone)
     ;; should still have the length of 11 since the heading already exists
     (should (= (1+ before) (length company-org-headings/alist))))))
