;;; company-org-headings --- completion backend for company.el


;; Copyright (c) 2015- Bernhard Pröll

;; Author: Bernhard Pröll
;; Maintainer: Bernhard Pröll
;; URL: https://github.com/mutbuerger/company-org-headings
;; Created: 2015-07-25
;; Version: 0.0.1
;; Keywords: completion, org-headings, abbrev, convenience
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
(require 'cl-lib)
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
  :group 'matching
  :group 'convenience
  :prefix "company-org-headings/")

(defcustom company-org-headings/search-directory nil
  "Search for Org-mode headings in this directory."
  :type 'string
  :group 'company-org-headings)

(defcustom company-org-headings/restricted-to-directory t
  "Complete in `company-org-headings/search-directory' only."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/stopwords
  (append company-org-headings/stopwords-english
	  company-org-headings/stopwords-german)
  "Collection of stopwords to be removed from the candidates."
  :type 'list
  :group 'company-org-headings)

(defcustom company-org-headings/annotations-separator "▶"
  "String to separate the annotations from the candidates."
  :type 'string
  :group 'company-org-headings)

(defcustom company-org-headings/no-cache '(equal arg "")
  "Avoid caching to match the prefix with every word of a candidate.
Slows down candidates retrieval significantly."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/case-sensitive t
  "Nil to ignore case when collecting completion candidates.
Setting this variable to nil will impair the speed of candidates
retrieval."
  :type 'boolean
  :group 'company-org-headings)

(defcustom company-org-headings/show-headings-context t
  "Show the meta information in the minibuffer.
Revealing the higher level heading to show the context of the
candidate at point."
  :type 'boolean
  :group 'company-org-headings)

;; hooks
(defcustom company-org-headings/create-alist-post-hook nil
  "Hook run after company successfully completes."
  :type 'hook
  :group 'company-org-headings)

(defvar company-org-headings/alist nil
  "Variable to hold the org headings with according filename.")

(defvar company-org-headings/candidates nil
  "Candidates the `company-org-headings/backend' will use.")

(defun company-org-headings/string-repeat (str n)
  (let ((res ))
    (dotimes (i n)
      (setq res (concat res str)))
    res))

;; ~~~~~~~~~~~~~~~~{  aggregrate headings function  }~~~~~~~~~~~~~~~~
(defun company-org-headings/aggregate-headings (dir)
  "Aggregate headings from the org files in DIR."
  (if (not company-org-headings/search-directory)
      (error "Specify a directory to collect headings from."))
  (cl-loop
   for files in (directory-files dir t "\\.org$")
   nconc
   (when (file-exists-p files)
     (with-temp-buffer
       (org-mode)
       (insert-file-contents files)
       (save-excursion
	 (goto-char (point-min))
	 (cl-loop
	  while (re-search-forward org-complex-heading-regexp nil t)
	  collect
	  (let ((heading (nth 4 (org-heading-components)))
		(parent (save-excursion
			  (org-up-heading-safe)
			  (cons (nth 1 (org-heading-components))
				(nth 4 (org-heading-components))))))
	    `(,heading
	      ,(cl-remove-if
		;; remove stopwords to allow for a more reliable candidates retrieval
		(lambda (s) (car (member s company-org-headings/stopwords)))
		(mapcar
		 (lambda (s)
		   ;; remove the occasional parentheses and quotes
		   (replace-regexp-in-string "[\]\[\\(\\)\'\\\"\{\}]+"
					     "" s))
		 (split-string heading split-string-default-separators)))
	      ,files
	      ,(when company-org-headings/show-headings-context
		 parent)))))))))

;; ~~~~~~~~~~~~~~~~~~~~~~{  backend functions  }~~~~~~~~~~~~~~~~~~~~~~
(defun company-org-headings/annotation (s)
  (format
   (concat " " company-org-headings/annotations-separator  " %s")
   (file-name-base
    (caddr (assoc s company-org-headings/alist)))))

(defun company-org-headings/word-match (prefix candidate)
  (memq t (mapcar
	   (if company-org-headings/case-sensitive
	       (lambda (s) (string-prefix-p prefix s))
	     ;; FIXME there may be a more elegant solution to this:
	     (lambda (s) (or (string-prefix-p (downcase prefix) s)
			(string-prefix-p (capitalize prefix) s)
			(string-prefix-p (upcase prefix) s))))
	   (cadr (assoc candidate company-org-headings/alist)))))

(defun company-org-headings/meta ()
  "Show contextual information in the minibuffer."
  (let* ((parent (cadddr (assoc arg company-org-headings/alist)))
	 (Npar (car parent))
	 (Nhead (1+ (car parent)))
	 (child (if (not (string-equal (cdr parent) arg))
		    (concat
		     "\n"
		     (company-org-headings/string-repeat
		      "*" Nhead) " " arg)
		  "")))
    (concat
     (propertize
      (concat
       (company-org-headings/string-repeat
	"*" Npar) " " (cdr parent))
      'face (nth Npar org-level-faces))
     (propertize
      child
      'face (nth Nhead org-level-faces)))))

(defun company-org-headings/insert-link (c)
  "Transform the completion with `org-store-link'.
The description of the Org-mode link will be determined by
`string-prefix-p' with all words in the completion string.
Occasionally there may be multiple possible completions for the
description, this function will take the first match."
  (let* ((alist (assoc c company-org-headings/alist))
	 (file (caddr alist))
	 (s (let ((res ))
	      (car
	       (remq
		nil
		(mapcar (lambda (x) (if (string-prefix-p company-prefix x)
				   (append res x)))
			(cadr alist)))))))
    (delete-backward-char (string-width c))
    (org-insert-link
     t (concat file "::*" c) s)))

;;;###autoload
(defun company-org-headings/create-alist ()
  "(Re-)create `company-org-headings/alist'.
The buffers related to the
`company-org-headings/search-directory' will be saved beforehand.

If you for example want to alter the candidates
`company-org-headings' will provide, make use of the
`company-org-headings/create-alist-post-hook'."
  (interactive)
  (message "Creating a `company-org-headings/alist'...")
  (setq company-org-headings/alist (company-org-headings/aggregate-headings
  				    company-org-headings/search-directory))
  (setq company-org-headings/candidates
	(mapcar 'car company-org-headings/alist))
  (message "Creating a `company-org-headings/alist'... done.")
  (run-hooks 'company-org-headings/create-alist-post-hook))

;;;###autoload
(defun company-org-headings (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Org-mode headings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-headings))
    (prefix (and (eq major-mode 'org-mode)
		 (when (and
			company-org-headings/search-directory
			company-org-headings/restricted-to-directory
			(buffer-file-name))
		   (file-in-directory-p (buffer-file-name)
					company-org-headings/search-directory))
		 (company-grab-symbol)))
    (candidates
     (progn
       ;; create a `company-org-headings/alist' if it doesn't yet exist
       (when (not company-org-headings/alist)
	 (company-org-headings/create-alist))
       (cl-remove-if-not
	(lambda (c) (company-org-headings/word-match arg c))
	company-org-headings/candidates
	)))
    (meta
     (when company-org-headings/show-headings-context
       (company-org-headings/meta)))
    (post-completion (company-org-headings/insert-link arg))
    ;; (ignore-case 'keep-prefix)
    (sorted t)
    (require-match 'never)
    (annotation (company-org-headings/annotation arg))
    (duplicates nil)
    ;; need to avoid caching for `company-org-headings/word-match' to
    ;; work properly - slows down candidates retrieval significantly
    (no-cache company-org-headings/no-cache)))

(provide 'company-org-headings)
;;; company-org-headings ends here
