;;; company-org-headings --- completion backend for company.el


;; Copyright (c) 2015- Bernhard Pröll

;; Author: Bernhard Pröll
;; Maintainer: Bernhard Pröll
;; URL: https://github.com/mutbuerger/company-org-headings
;; Created: 2015-07-25
;; Version: 0.2.0
;; Keywords: company abbrev convenience matching
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This backend for `company' provides completion for Org-mode
;; headings of org files in a specified directory. This package is
;; directed towards Org-mode users that keep their notes in multiple
;; files and try to interconnect them with extensive linking. The idea
;; behind this is making it as easy as possible to link new notes to
;; the concurrent knowledge base in the process of writing them. This
;; may be of great use when trying to create a knowledge base system
;; similar to Niklas Luhmann's Zettelkasten that evolves to something
;; like a "second brain" only by linking the bits of information
;; together. Certainly there may be other ways to use this library.

;;; Code:


(require 'org)
(require 'org-capture)
(require 'cl-lib)
(require 'dash)
(require 'company)

(defconst company-org-headings/stopwords-german
  (list "aber" "alle" "allem" "allen" "aller" "alles" "als"
	"also" "am" "an" "ander" "andere" "anderem" "anderen"
	"anderer" "anderes" "anderm" "andern" "anderr" "anders"
	"auch" "auf" "aus" "bei" "bin" "bis" "bist" "da" "damit"
	"dann" "der" "den" "des" "dem" "die" "das" "daß" "derselbe"
	"derselben" "denselben" "desselben" "demselben" "dieselbe"
	"dieselben" "dasselbe" "dazu" "dein" "deine" "deinem" "deinen"
	"deiner" "deines" "denn" "derer" "dessen" "dich" "dir"
	"du" "dies" "diese" "diesem" "diesen" "dieser" "dieses"
	"doch" "dort" "durch" "ein" "eine" "einem" "einen" "einer"
	"eines" "einig" "einige" "einigem" "einigen" "einiger"
	"einiges" "einmal" "er" "ihn" "ihm" "es" "etwas" "euer"
	"eure" "eurem" "euren" "eurer" "eures" "für" "gegen"
	"gewesen" "hab" "habe" "haben" "hat" "hatte" "hatten"
	"hier" "hin" "hinter" "ich" "mich" "mir" "ihr" "ihre"
	"ihrem" "ihren" "ihrer" "ihres" "euch" "im" "in" "indem"
	"ins" "ist" "jede" "jedem" "jeden" "jeder" "jedes" "jene"
	"jenem" "jenen" "jener" "jenes" "jetzt" "kann" "kein"
	"keine" "keinem" "keinen" "keiner" "keines" "können" "könnte"
	"machen" "man" "manche" "manchem" "manchen" "mancher" "manches"
	"mein" "meine" "meinem" "meinen" "meiner" "meines" "mit"
	"muss" "musste" "nach" "nicht" "nichts" "noch" "nun" "nur"
	"ob" "oder" "ohne" "sehr" "sein" "seine" "seinem" "seinen"
	"seiner" "seines" "selbst" "sich" "sie" "ihnen" "sind"
	"so" "solche" "solchem" "solchen" "solcher" "solches" "soll"
	"sollte" "sondern" "sonst" "über" "um" "und" "uns" "unse"
	"unsem" "unsen" "unser" "unses" "unter" "viel" "vom" "von"
	"vor" "während" "war" "waren" "warst" "was" "weg" "weil"
	"weiter" "welche" "welchem" "welchen" "welcher" "welches"
	"wenn" "werde" "werden" "wie" "wieder" "will" "wir" "wird"
	"wirst" "wo" "wollen" "wollte" "würde" "würden" "zu"
	"zum" "zur" "zwar" "zwischen")
  "The standard set of german stopwords provided by the `tm' package for R.")

(defconst company-org-headings/stopwords-english
  (list "i" "me" "my" "myself" "we" "our" "ours" "ourselves"
	"you" "your" "yours" "yourself" "yourselves" "he" "him"
	"his" "himself" "she" "her" "hers" "herself" "it" "its"
	"itself" "they" "them" "their" "theirs" "themselves" "what"
	"which" "who" "whom" "this" "that" "these" "those" "am"
	"is" "are" "was" "were" "be" "been" "being" "have" "has"
	"had" "having" "do" "does" "did" "doing" "would" "should"
	"could" "ought" "i'm" "you're" "he's" "she's" "it's" "we're"
	"they're" "i've" "you've" "we've" "they've" "i'd" "you'd"
	"he'd" "she'd" "we'd" "they'd" "i'll" "you'll" "he'll"
	"she'll" "we'll" "they'll" "isn't" "aren't" "wasn't" "weren't"
	"hasn't" "haven't" "hadn't" "doesn't" "don't" "didn't"
	"won't" "wouldn't" "shan't" "shouldn't" "can't" "cannot"
	"couldn't" "mustn't" "let's" "that's" "who's" "what's"
	"here's" "there's" "when's" "where's" "why's" "how's" "a"
	"an" "the" "and" "but" "if" "or" "because" "as" "until"
	"while" "of" "at" "by" "for" "with" "about" "against"
	"between" "into" "through" "during" "before" "after" "above"
	"below" "to" "from" "up" "down" "in" "out" "on" "off"
	"over" "under" "again" "further" "then" "once" "here"
	"there" "when" "where" "why" "how" "all" "any" "both"
	"each" "few" "more" "most" "other" "some" "such" "no"
	"nor" "not" "only" "own" "same" "so" "than" "too" "very"
	)
  "The standard set of english stopwords provided by the `tm' package for R.")

;; customize
(defgroup company-org-headings nil
  "Customization group for company-org-headings."
  :group 'company
  :prefix "company-org-headings/")

(defcustom company-org-headings/search-directory nil
  "Search for Org-mode headings in this directory."
  :type 'string
  :group 'company-org-headings)

(defcustom company-org-headings/restricted-to-directory t
  "Complete in `company-org-headings/search-directory' only."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/use-in-capture-mode t
  "Also complete in `org-capture-mode'."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/stopwords
  (append company-org-headings/stopwords-english
	  company-org-headings/stopwords-german)
  "Collection of stopwords to be removed from the candidates."
  :type 'sexp
  :group 'company-org-headings)

(defcustom company-org-headings/annotations-separator "▷"
  "String to separate the annotations from the candidates."
  :type 'string
  :group 'company-org-headings)

(defcustom company-org-headings/no-cache '(equal arg "")
  "Save candidates in cache."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/case-sensitive t
  "Nil for case-insensitive matching."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/show-headings-context 'path
  "Show the meta information in the echo area.
Revealing the higher level heading(s) to show the context of the
candidate at point.

The default is set to \"path\" to avoid the constant resizing of
the minibuffer resp. the echo area when you are switching through
several completion candidates. It displays the path as the
`org-display-outline-path' command does.

The default setting of this variable is the most time-consuming
choice when it comes to building the
`company-org-headings/alist'.

Consider rebuilding the `company-org-headings/alist' with the
`company-org-headings/create-alist' command when changing your
choice."
  :type '(choice
	  (const :tag "As path" path)
	  (const :tag "As outline" outline)
	  (other :tag "No" nil))
  :group 'company-org-headings)

(defcustom company-org-headings/ignore-stopwords t
  "Set to non-nil to inhibit completion on stopwords.
In fact with this variable set to `t' the backend will only offer
candidates whenever the symbol at point is NOT a prefix of any
stopword."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/point-after-completion 'after
  "Specify where the point after the link insertion should be located.
Set this variable to one of the following:

inside: Point will be located at the end of the link description.
From there, you may change the description to your liking.

after: Point will be located right after the link."
  :type '(choice
	  (const :tag "Inside the org-link description" inside)
	  (const :tag "After the org-link" after))
  :group 'company-org-headings)

(defcustom company-org-headings/link-description 'full
  "Specify the description part of an org link.
There are two options to complete the link's description:

full: The link description is the complete heading text to link
refers to.

word: The link description is the word in the candidate that
matches the string at point."
  :type '(choice
	  (const :tag "The whole heading text" full)
	  (const :tag "The word that matches"  word))
  :group 'company-org-headings)

(defcustom company-org-headings/idle-time-before-parsing 15
  "Seconds of idle-time before the parser starts collecting data."
  :type 'float
  :group 'company-org-headings)

(defcustom company-org-headings/rebuild-alist-on-idle-p 'rebuild
  "Renew the idle timer whenever retrieving candidates.
There are two choices to keep the alist up to date:

append: Heading elements in the currently visited file are
appended to the alist if they are not there yet.

rebuild: Parse the whole directory again and append new heading
elements.

A value of nil will inhibit the recurring parsing, leaving you
with the alist built at the very beginning."
  :type '(choice
	  (const :tag "Append headings in currently visited file" append)
	  (const :tag "Rebuild the alist as a whole" rebuild)
	  (const :tag "Build the alist only once" nil))
  :group 'company-org-headings)

;; hook(s)
(defcustom company-org-headings/collect-data-post-hook nil
  "Hook run after `company-org-headings/alist' has been successfully built."
  :type 'hook
  :group 'company-org-headings)


(defvar company-org-headings/alist nil
  "Variable to hold the collected data from org files.")

(defvar company-org-headings/candidates nil
  "Candidates the `company-org-headings/backend' will use.")

(defvar company-org-headings/parser-state nil
  "Ensure something like a state by saving the currently parsed file.")

(defvar company-org-headings/idle-timer nil
  "Variable to hold the timer.")


(defun company-org-headings/string-repeat (str n)
  (let (res)
    (dotimes (i n)
      (setq res (concat res str)))
    res))

;; ###########################  parsing  ###########################

(defun company-org-headings/collect-components (file)
  (let ((heading (nth 4 (org-heading-components)))
	(parent
	 (save-excursion
	   (org-up-heading-safe)
	   (cons (nth 1 (org-heading-components))
		 (nth 4 (org-heading-components))))))
    `(,heading
      ,file
      ,(cond
	((equal company-org-headings/show-headings-context 'path)
	 (org-get-outline-path))
	((equal company-org-headings/show-headings-context 'outline)
	 parent)
	(t nil)))))

(defun company-org-headings/parse-buffer-file (file)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       while (re-search-forward org-complex-heading-regexp nil t)
       collect
       (company-org-headings/collect-components file)))))

(defun company-org-headings/parse-message (secs)
  (let* ((idl  (- secs company-org-headings/idle-time-before-parsing))
	 (dots (% idl 15)))
    (message
     (concat
      (propertize "company-org-headings"
		  'face (car	org-level-faces))
      " is collecting…    "
      (company-org-headings/string-repeat " " dots)
      (propertize "ᗣ"	'face (nth 5	org-level-faces))
      (propertize "ᗣ"	'face (nth 4	org-level-faces))
      (propertize "ᗣ"	'face (nth 3	org-level-faces))
      (propertize " ᗧ"	'face (cadr	org-level-faces))
      (company-org-headings/string-repeat
       "·" (- 15 dots)) "··"))))

(defun company-org-headings/file-to-parse ()
  (let ((files
	 ;; remove temporary files
	 (-filter
	  'file-exists-p
	  (directory-files company-org-headings/search-directory t "\\.org$"))))
    (car-safe
     (cond
      (company-org-headings/parser-state
       ;; subset the tail of the list; beginning from where we stopped
       ;; parsing, incrementally reducing the files to parse and make
       ;; progress towards the base case, that is the empty list
       (cdr-safe
	(--drop-while
	 (not (equal it company-org-headings/parser-state)) files)))
      ;; if only headings in the currently visited file should be
      ;; appended to the alist, check whether the alist has been
      ;; built already
      ((and (eq company-org-headings/rebuild-alist-on-idle-p 'append)
	    (cdr-safe files)
	    (cdr-safe company-org-headings/alist)
	    (member (buffer-file-name) files))
       (list (buffer-file-name)))
      ;; with 'rebuild option provide all directory-files
      ((or (eq company-org-headings/rebuild-alist-on-idle-p 'rebuild)
	   ;; else only return files if alist doesn't exist yet
	   (not company-org-headings/alist))
       files)))))

(defun company-org-headings/collect-data ()
  "Collect headings from org files."
  (let ((file (company-org-headings/file-to-parse))
	(secs (car (decode-time (current-idle-time)))))
    ;; break condition: cancel timer when there are no files left;
    ;; this is the base case
    (if (not file)
	(progn
	  ;; clear state
	  (setq company-org-headings/parser-state nil)
	  ;; and clear echo area
	  (run-with-timer 1 nil 'message nil)
	  (run-hooks 'company-org-headings/collect-data-post-hook)
	  (cancel-timer company-org-headings/idle-timer))
      ;; invoke message only once per second while parsing
      (unless (and
	       timer-list
	       (equal (aref (car timer-list) 5)
		      'company-org-headings/parse-message))
	(run-with-timer 1 nil 'company-org-headings/parse-message secs))
      (mapc
       (lambda (x)
	 ;; push to the alist only if it's not already there
	 ;; default :test is `eql' that won't help in this case
	 (cl-pushnew x company-org-headings/alist :test 'equal))
       (company-org-headings/parse-buffer-file file))
      (setq company-org-headings/parser-state file)
      (setq company-org-headings/candidates
	    (mapcar 'car company-org-headings/alist))
      ;; recursively set timer that calls the collect defun
      ;; immediately because SECS is <= `current-idle-time'. this
      ;; allows for interrupting after every recursion and avoids
      ;; using up the stack granted by `max-lisp-eval-depth'
      (company-org-headings/set-idle-timer))))

;;;###autoload
(defun company-org-headings/set-idle-timer (&optional secs)
  "Set timer to collect data in idle time.
Save the timer to a variable. This allows us to cancel the timer
whenever necessary."
  (interactive)
  (setq
   company-org-headings/idle-timer
   (run-with-idle-timer
    (or secs company-org-headings/idle-time-before-parsing) nil
    'company-org-headings/collect-data)))

(defun company-org-headings/in-search-dir-and-org-p (file)
  ;; in case the `buffer-file-name' returns nil (e.g. in a capture
  ;; buffer) this saves me from a stringp nil error
  (when file
      (if (file-exists-p file)
	  (and (string= (file-name-extension file) "org")
	       (member (file-name-nondirectory file)
		       (directory-files company-org-headings/search-directory)))
	(user-error
	 "There is no corresponding org file for the current buffer."))))

;;;###autoload
(defun company-org-headings/append-current-file-headings ()
  "Collect heading elements in the currently visited file immediately.
Append the elements to the `company-org-headings/alist' if not
already there.

This command is useful when you want to add newly created
headings to the completion candidates immediately after creating
them."
  (interactive)
  (when (company-org-headings/in-search-dir-and-org-p (buffer-file-name))
    (mapc
     (lambda (x)
       ;; default :test is `eql' that won't help in this case
       (cl-pushnew x company-org-headings/alist :test 'equal))
     (company-org-headings/parse-buffer-file (buffer-file-name)))
    (setq
     company-org-headings/candidates
     (mapcar 'car company-org-headings/alist))))

;; #########################  completion  #########################

(defun company-org-headings/annotation (s)
  (format
   (concat " " company-org-headings/annotations-separator " %s")
   (file-name-base
    (cadr (assoc s company-org-headings/alist)))))

(defun company-org-headings/matching-candidates (prefix)
  (let ((case-fold-search (not company-org-headings/case-sensitive))
	(fun (lambda () (cl-remove-if-not
		    (lambda (x) (string-match-p (regexp-quote prefix) x))
		    company-org-headings/candidates))))
    (if company-org-headings/ignore-stopwords
	(unless
	    (-any? (lambda (x) (string-prefix-p prefix x))
		   company-org-headings/stopwords)
	  (funcall fun))
      (funcall fun))))

(defun company-org-headings/meta ()
  "Show contextual information in the echo area."
  (if (equal company-org-headings/show-headings-context 'path)
      (org-format-outline-path
       (cl-caddr (assoc arg company-org-headings/alist)))
    (let* ((parent (cl-caddr (assoc arg company-org-headings/alist)))
	   (par (car parent))
	   (head (1+ (car parent)))
	   (child (if (not (string-equal (cdr parent) arg))
		      (concat
		       "\n"
		       (company-org-headings/string-repeat
			"*" head) " " arg)
		    "")))
      (concat
       (propertize
	(concat
	 (company-org-headings/string-repeat
	  "*" par) " " (cdr parent))
	'face (nth par org-level-faces))
       (propertize
	child
	'face (nth head org-level-faces))))))

(defun company-org-headings/insert-link (c)
  "Transform the completion to an org link.
The description of the Org-mode link will be determined by
`string-match-p' with all words in the completion string.
Occasionally there may be multiple possible completions for the
description, this function will take the first match."
  (let* ((case-fold-search (not company-org-headings/case-sensitive))
	 (alist (assoc c company-org-headings/alist))
	 (file (cadr alist))
	 (s (if (eq company-org-headings/link-description 'full) c
	      (let (res)
		(car
		 (remq
		  nil
		  (mapcar
		   (lambda (x) (when (string-match-p
				 (regexp-quote company-prefix) x)
			    (append res x)))
		   (split-string
		    (car alist)
		    split-string-default-separators))))))))
    (delete-char (- 0 (string-width c)))
    (org-insert-link
     t (concat file "::*" c) s)
    (when (eq
	   company-org-headings/point-after-completion
	   'inside)
      (re-search-backward "]]"))))

(defun company-org-headings/candidates ()
  ;; conditions when to set the idle-timer
  (when (or (not company-org-headings/alist)
	    (when company-org-headings/rebuild-alist-on-idle-p
	      (not (member company-org-headings/idle-timer
			   timer-idle-list))))
    ;; create the `company-org-headings/alist' on next idle time
    ;; if it doesn't yet exist
    (company-org-headings/set-idle-timer))
  (company-org-headings/matching-candidates arg))

(defun company-org-headings/prefix-p ()
  (and
   (derived-mode-p 'org-mode)
   (or
    (when company-org-headings/use-in-capture-mode
      (and org-capture-mode
	   (buffer-base-buffer (current-buffer))))
    (buffer-file-name))
   company-org-headings/search-directory
   ;; when the restriction is desired, test if file is in the
   ;; directory
   (if company-org-headings/restricted-to-directory
       (or
	(when company-org-headings/use-in-capture-mode
	  (and org-capture-mode
	       (buffer-base-buffer (current-buffer))))
	(company-org-headings/in-search-dir-and-org-p
	 (buffer-file-name)))
     ;; else return t
     t)))

;;;###autoload
(defun company-org-headings (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `org-mode' headings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-headings))
    (prefix (when (company-org-headings/prefix-p) (company-grab-symbol)))
    (candidates (company-org-headings/candidates))
    (meta
     (when company-org-headings/show-headings-context
       (company-org-headings/meta)))
    (post-completion (company-org-headings/insert-link arg))
    ;; (ignore-case 'keep-prefix)
    (sorted t)
    (require-match 'never)
    (annotation (company-org-headings/annotation arg))
    (duplicates nil)
    (no-cache company-org-headings/no-cache)))

(provide 'company-org-headings)
;;; company-org-headings ends here
