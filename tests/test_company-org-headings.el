(require 'projectile)

(ert-deftest test-aggregate-headings ()
  (let ((company-org-headings/alist
	 (company-org-headings/aggregate-headings
	  (concat (projectile-project-root)
		  "/tests"))))
    (should (= 5 (length company-org-headings/alist)))))
