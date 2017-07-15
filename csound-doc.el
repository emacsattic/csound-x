;;; -*- auto-recompile: t -*-

;;; csound-doc.el --- utilities for Csound documentation

;; Keywords: csound, convenience, documentation

;; This file is not part of GNU Emacs.
;; 
;; csound-doc.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-doc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;; Commentary:  Csound on-line help within GNU Emacs 
;;              (this package requires Emacs 21.1)


;; installation
;;-------------

;;================================================================
;; this should be installed automatically by the csound-x package
;;================================================================
;; however, for a stand-alone installation:
;; put this file in your load-path and add the following to your .emacs: 
;;   (require 'csound-doc)


;; this package requires either the "canonical" csound 5 reference manual
;; which is part of the csound 5 distribution,
;;
;; or the "official" (David Boothe's) HTML docs
;; which you can get there: 
;;
;;    http://www.lakewoodsound.com/csound/hypertext/manual.htm
;;
;; or the "alternative" manual from Kevin Conder, which you can get there:
;;
;;    http://www.kevindumpscore.com/download.html (take the HTML one !)


;; go to the 'csound-doc' customization group (a sub-group of the 'convenience'
;; group) and define your settings as follow:
;;
;;    set 'csdoc-html-directory' and 'csdoc-html-entry-point' as needed to fit 
;;    your installation
;;
;;    tell Emacs which HTML manual you're using by setting 'csdoc-which-manual to
;;    either its default, or Boothe's or Conder's (deprecated csound 5 manuals).
;;
;;    if you want to use the emacs w3 browser instead of the default external
;;    one, toggle 'csdoc-use-w3' on 
;;    (note that w3 is an independant emacs package; it is not installed in
;;    the default configuration)
;;    then you may want the doc window to pop up in a specific frame:
;;    (add-to-list 'special-display-buffer-names "*Csound Documentation*")


;; usage
;;------

;; the following interactive commands are equivalent to the menu items
;; provided in the "Documentation" submenu of "Orc+":
;;
;;    M-x csdoc-html-document-opcode           document the opcode at point
;;    M-x csdoc-insert-opcode-html-template    insert a template
;;    M-x csdoc-browse-html                    browse the HTML documentation
;;
;; If you just updated the documentation, you may also find the following useful:
;;
;;    M-x csdoc-refresh-html            refresh the hash table for html docs


;; feel free to comment or request/provide features, feedback is always welcome
;;
;;                         Stef        

;; --------
;; last modified May 25, 2010
;; (in sync with csound 5.12)

;; code:

(defgroup csound-doc nil
  "Utilities for on-line Csound documentation"
  :group 'csound-x
  :prefix "csdoc-")

(defcustom csdoc-html-manual 'csound5-canonical
  "Which HTML manual are you using ?"
  :type '(radio
          (const :tag "Csound 5 canonical" csound5-canonical)
          (const :tag "David Boothe's" boothe)
          (const :tag "Kevin Conder's" conder))
  :group 'csound-doc)

(defcustom csdoc-html-directory 
  ;; fragile heuristic below
  (or (getenv "CSDOCDIR")
      ;; windows
      (let ((csound-path (csound-find-directory)))
	(when csound-path (expand-file-name "doc/manual/" csound-path)))
      ;;linux
      (let ((somepage (first 
		 (split-string 
		  (shell-command-to-string 
		   "locate /usr*csound*manual/diskin.html")))))
	(when somepage (file-name-directory somepage)))
      ;; useless default value
      "~")
  "Directory containing the HTML docs"
  :type 'directory
  :set (lambda (option val)
	 (set option val)
	 (when (fboundp 'csdoc-refresh-html)
	   (csdoc-refresh-html)))
  :group 'csound-x-applications-paths
  :group 'csound-doc)

(defun csdoc-html-directory ()
  (substitute-in-file-name csdoc-html-directory))

(defcustom csdoc-html-entry-point 
  (expand-file-name "indexframes.html" csdoc-html-directory)
  "The starting page of your choice for browsing the HTML docs"
  :type 'file
  :group 'csound-x-applications-paths
  :group 'csound-doc)

(defun csdoc-html-entry-point ()
  (let ((file (substitute-in-file-name csdoc-html-entry-point)))
    (if (file-exists-p file) file
      (expand-file-name "index.html" (csdoc-html-directory)))))

(defcustom csdoc-examples-with-new-frame nil
   "If t, `csdoc-fetch-opcode-example' does create a new frame,
else it reuses the current window"
   :type 'boolean
   :group 'csound-doc)

(defcustom csdoc-use-w3 nil
  "Weither w3 should be invoked to browse the HTML docs.
If nil, the default external browser is used."
  :type 'boolean
  :group 'csound-doc)

(defcustom csdoc-winhelp-file "c:/csound/csound.hlp"
  "Location of winhelp file for csound \(that's for Windows)"
  :type 'file
  :group 'csound-x-applications-paths
  :group 'csound-doc)

(defun csdoc-winhelp-file ()
  (substitute-in-file-name csdoc-winhelp-file))

(defcustom csdoc-csoundAV-manual-directory "c:/csound/ManualCsoundAV"
  "Directory containing CsoundAV HTML manual"
  :type 'file
  :group 'csound-x-applications-paths
  :group 'csound-doc)


(defvar csdoc-special-opcodes '(("&amp;" "&")
                                ("&amp;&amp;" "&&")
                                ("&gt;" ">")
                                ("&gt;=" ">=")
                                ("&lt;" "<")
                                ("&lt;=" "<=")
                                ("&lt;&lt;" "<<")
                                ("&gt;&gt;" ">>")
				("¬" "~")) 
  "opcodes coming with htmlized names, and their correct names")

(defun csound-doc-available-p ()
  (file-exists-p (csdoc-html-entry-point)))

(defun csound-winhelp-available-p ()
  (file-exists-p (csdoc-winhelp-file)))

(defun csound-doc-submenu (&optional global)
  "Submenu specification for documentation.
When GLOBAL is t some context-specific items are removed"
  (delete-if 'null
	     `("Documentation"
	       ["Browse HTML Documentation"  csdoc-browse-html (csound-doc-available-p)]
	       ["Document Opcode"  csdoc-html-document-opcode (csound-doc-available-p)]
	       ,(unless global ["Insert Opcode Template" csdoc-insert-opcode-html-template (csound-doc-available-p)])
	       ["Fetch Opcode Example" csdoc-fetch-opcode-example
		(and (csound-doc-available-p) (eq csdoc-html-manual 'csound5-canonical))]
	       ,@(when (and (member system-type '(windows-nt cygwin))
			    (csound-winhelp-available-p))
		   '("--"
		     ["Open Winhelp" csdoc-open-winhelp t]
		     ["Winhelp Document Opcode" csdoc-winhelp-document-opcode t]
		     "--"
		     ["Browse CsoundAV Manual" csdoc-browse-csoundav-man t]
		     ))
	       "--"
	       ["Csound -z" csound-z]
	       "--"
	       ["Documentation settings" (customize-group 'csound-doc) t]
	       ["Scan HTML docs again (refresh)" csdoc-refresh-html (csound-doc-available-p)])))
  
(defun csdoc-open-winhelp ()
  (interactive)
  (call-process "winhlp32" nil 0 nil (csdoc-winhelp-file)))

(defun csdoc-winhelp-document-opcode (opcode)
  "Pop-up the Winhelp documentation related to opcode at point"
  (interactive (list (read-from-minibuffer "Opcode: " (thing-at-point 'word))))
  (call-process "winhlp32" nil 0 nil (concat "-k" opcode) (csdoc-winhelp-file)))

(defun csdoc-browse-csoundav-man ()
  "Browse the HTML documentation for CsoundAV"
  (interactive )
  (browse-url-of-file (expand-file-name "index.htm" csdoc-csoundAV-manual-directory)))

(defun csdoc-browse-html ()
  "Browse the HTML documentation
\(starting with the file pointed at by csdoc-html-entry-point)"
  (interactive)
  (if csdoc-use-w3
      (w3-open-local (csdoc-html-entry-point))
    (browse-url-of-file (csdoc-html-entry-point))))

(defun csdoc-scan-html-files ()
  (cond 
    ((eq csdoc-html-manual 'csound5-canonical) (csdoc-scan-manual-html-files))
    ((eq csdoc-html-manual 'boothe) (csdoc-scan-dbmanual-html-files))    
    ((eq csdoc-html-manual 'conder) (csdoc-scan-kdmanual-html-files))))  

;; Csound 5 canonical reference manual, as of version 5.11
(defun csdoc-scan-manual-html-files ()
  (let ((table (makehash 'equal)) ;(make-hash-table :test 'equal))
	(index-file (expand-file-name "index.html" (csdoc-html-directory)))
	end)
    (when (file-exists-p index-file)
      (with-temp-buffer
	(insert-file-contents index-file)
	(goto-char (point-min))
	(when (search-forward
	       "Orchestra Opcodes and Operators"
	       (setq end
		     (save-excursion
		       (or (progn
			     (search-forward 
			      "Score Statements and GEN Routines" nil t) 
			     (search-backward "<a"))
			   (point-max)))) 
	       t)
	  (message "scanning opcodes section in manual index")
	  (while (re-search-forward "href=\"\\([^\"]+\\)\"" end t)
	    (let* ((html-file (match-string 1))
		   (opcode (progn (re-search-forward ">\\(.+\\)</a")
				  (match-string 1))))
	      ;; we do not consider some so-called "opcodes"
	      (unless (or
		       (and (re-search-forward "^") 
			    ;; do something about them ?
			    (looking-at "[ \t]*Deprecated")) 
		       (equal ?# (elt opcode 0))
		       (equal ?< (elt opcode 0)))
		(setq opcode (or (cadr (assoc opcode csdoc-special-opcodes)) opcode))
		(puthash opcode (expand-file-name html-file (csdoc-html-directory)) table)))))))
    table))

;; David Boothe's manual (for csound 4)
(defun csdoc-scan-dbmanual-html-files ()
  (let ((table (makehash 'equal)))
    (if (file-exists-p (csdoc-html-directory))
	(dolist (sub-dir (directory-files (csdoc-html-directory) t))
	  (if (and (file-directory-p sub-dir)
		   (null (or (equal (substring sub-dir -2) "/.")
			     (equal (substring sub-dir -3) "/.."))))
	      (dolist (html-file (directory-files sub-dir t "\\.htm"))
		(with-temp-buffer
		  (insert-file-contents html-file)
		  (goto-char (point-min))
		  (if (re-search-forward "^<h2>" () t)
		      (dolist (opcode (split-string (thing-at-point 'line) "[ ,\t\n/<>]" t))
			(if (null (or (equal opcode "")
				      (equal opcode "h2")))
			    (puthash opcode html-file table)))))))))
    table))

;; Kevin Conder's manual (for csound 4)
(defun csdoc-scan-kdmanual-html-files ()
  (let ((table (makehash 'equal))
	(index-file (expand-file-name "index.html" (csdoc-html-directory))))
    (when (file-exists-p index-file)
      (with-temp-buffer
	(insert-file-contents index-file)
	(goto-char (point-min))
	(when (search-forward "Orchestra Opcodes and Operators"
			      (save-excursion
				(or (search-forward "Score Statements and GEN Routines" nil t)
			      (point-max))) 
			      t)
	    (while (re-search-forward "^HREF=\"\\(.+\\)\"$" nil t)
	      (let ((html-file (match-string 1))
		    (opcode (progn (re-search-forward ">\\(.+\\)</a")
				   (match-string 1))))
		(puthash opcode (expand-file-name html-file (csdoc-html-directory)) table))))))
    table))

(defvar csdoc-html-opcodes (csdoc-scan-html-files)
  "Hash table associating each documented opcode to its file 
in the HTML manual")

(defun csdoc-refresh-html ()
  (interactive)
  (setq csdoc-html-opcodes (csdoc-scan-html-files)))

;;;-----------------------------------------------------------------------------------
;;;                  opcode commands
;;;-----------------------------------------------------------------------------------

(defmacro defun-opcode-command (name docstring prompt &rest body)
  "Define the function property of NAME as a command of argument OPCODE
variable DOCFILE is defined within the scope of BODY and refers to the html
file documenting OPCODE"
  (declare (indent 1))
  `(defun ,name (opcode)
     ,docstring
     (interactive (list (completing-read
		    ,prompt
                    (mapcar 'list (csdoc-get-all-opcodes))
                    nil
                    t
		    (thing-at-point 'word))))
     (let ((docfile (gethash opcode csdoc-html-opcodes)))
       (when (and docfile (file-readable-p docfile))
         ,@body))))

(defun-opcode-command csdoc-html-document-opcode
  "Pop-up the HTML documentation related to opcode at point"
  "Document opcode: "
  (if csdoc-use-w3 (w3-open-local docfile) (browse-url-of-file docfile)))

(defalias 'csound-opcode-doc 'csdoc-html-document-opcode)

(defun-opcode-command csdoc-insert-opcode-html-template 
  "Insert the documentation template related to opcode at point
WARNING: this replaces the line at point !"
  "Insert template for opcode: "
  (let ((opcode-template "")
        (bleurp ""))
    (with-temp-buffer
      (insert-file-contents docfile)
      (goto-char (point-min))
      (cond 
       ;; canonical
       ((eq csdoc-html-manual 'csound5-canonical)
        (setq opcode-template (csdoc-read-manual-section "Syntax")))
       ;; Boothe's
       ((eq csdoc-html-manual 'boothe)
        (if (search-forward (concat "<strong>" opcode "</strong>") () t)
            (setq opcode-template
                  (replace-regexp-in-string "</*strong>" ""
                                            (thing-at-point 'line)))))
       ;; Conder's
       ((eq csdoc-html-manual 'conder)
        (search-forward ">Syntax</h2")
        (while (and (re-search-forward ">\\(.+\\)<\\(.*\\)$" nil t)
                    (not (member (match-string 1) '("Initialization" "Performance"))))
          (setq opcode-template (concat opcode-template (match-string 1)))
          (if (string-match "/p" (match-string 2))
              (setq opcode-template (concat opcode-template "\n")))))))
    (beginning-of-line)
    (kill-line)
    (insert opcode-template)))

(defun-opcode-command csdoc-fetch-opcode-example
  "Bring up a CSD from the example section in the doc page for OPCODE"
  "Fetch example for opcode: "
  (let (opcode-example-orc opcode-example-sco opcode-example-csd)
    (when (eq csdoc-html-manual 'csound5-canonical) ;; only csound 5 is supported
      (with-temp-buffer
        (insert-file-contents docfile)
        (goto-char (point-min))
	(if (re-search-forward "href=\"\\(.+\\.csd\\)\"" nil t)
	    (setq opcode-example-csd (expand-file-name (match-string 1) (csdoc-html-directory)))
	  (when (search-forward "class=\"example\"" nil t)
	    (dolist (part '(opcode-example-orc opcode-example-sco))
	      (when (search-forward "class=\"programlisting\"" nil t)
		(search-forward "class=\"inlinemediaobject\"" nil t)
		(set part (buffer-substring 
			   (search-forward ">")
			   (progn
			     (search-forward "</")
			     (- (point) 2)))))))))

    (if (and (not opcode-example-csd)
	     (not (and opcode-example-orc opcode-example-sco)))
        (message "No example found for %s !" opcode)
      (when csdoc-examples-with-new-frame (select-frame (make-frame)))
      (if opcode-example-csd
	  (find-file opcode-example-csd)
	(switch-to-buffer (get-buffer-create (generate-new-buffer-name " *temp csd*")))
	(insert opcode-example-orc opcode-example-sco)
	(cscsd-auto-import (expand-file-name (format "%s example.csd" opcode) (cscsd-default-csd-directory))))))))

(defun csdoc-get-all-opcodes () 
  "Return a list of all opcodes" 
  (let (opcodes)
    (maphash (lambda (opcode file) 
               (add-to-list 'opcodes opcode)) 
             csdoc-html-opcodes)
    opcodes))

;;;-----------------------------------------------------------------------------------
;;;                  opcode database & opcodes classification
;;;-----------------------------------------------------------------------------------

(defvar csdoc-opcode-database nil
  "Store specifications for all documented opcodes.
Use function `csdoc-opcode-database' for accessing it")

(defvar csdoc-irregular-opcodes ()
  "opcodes whose specification could not be read.
consequently they are not present in `csdoc-opcode-database'
although they do appear in `csdoc-html-opcodes'")

(defvar csdoc-deprecated-opcodes ()
  "")

(defun csdoc-populate-opcode-database ()
  "Scan the canonical Csound 5 HTML manual and build a database covering
all documented opcodes and functions, stored in variable `csdoc-opcode-database'"
  (when (eq csdoc-html-manual 'csound5-canonical) ;; only csound 5 is supported
    (setq csdoc-opcode-database (makehash 'equal)
          csdoc-irregular-opcodes nil)
    (dolist (opcode (csdoc-get-all-opcodes))
      (let (spec opcode-template opcode-doc opcode-deprecated
                 (html-doc (gethash opcode csdoc-html-opcodes)))                ;;; ?? test
        (when (and html-doc (file-exists-p html-doc))
          (with-temp-buffer
            (insert-file-contents html-doc)
            (setq opcode-doc (csdoc-read-manual-section "Description" t)
		  opcode-template (csdoc-read-template opcode)
		  opcode-deprecated (csdoc-read-deprecated opcode)))
	  (if (or (not opcode-template)
		  (string= "" opcode-template))
	      (progn
		(unless opcode-deprecated
		  (message "error parsing template for opcode: %s in file %S" 
			   opcode html-doc))
		(add-to-list 'csdoc-irregular-opcodes opcode))
            ;; synthetic spec
            ;; 1
            (let (spec-list)
              (dolist (line (split-string opcode-template "\n" t))
                (when (string-match "\".*\"" line)
                  (setq line (replace-match "\"\"" nil nil line)))
                (setq line (mapconcat 'identity (split-string line "\\[" t) " - ")
                      line (mapcar '(lambda (mot)
                                      (if (string= mot opcode)
                                          (concat " " opcode " ")
                                        (substring mot 0 1)))
                                   (split-string line "[ ,]" t)))
                (when (or (null spec-list)
                          (= (length line) (length (car spec-list))))
                  (add-to-list 'spec-list line 't)))
              ;; 2
              (condition-case nil
                  (progn
                    (setq spec "")
                    (while (caar spec-list)
                      (let ((s1 ""))
                        (when (string= (caar spec-list) "-")
                          (setq spec (concat spec " -"))
                          (setq spec-list (mapcar 'cdr spec-list)))
                        (if (> (length (caar spec-list)) 1)
                            (setq spec (concat spec (caar spec-list)))
                          (progn
                            (dolist (l spec-list)
                              (setq s1 (concat s1 (car l))))
                            (setq spec (concat spec s1 " "))))
                        (setq spec-list (mapcar 'cdr spec-list)))))
                (progn
                  (message "could not figure out spec-list:\n%s\nspec so far: %s" spec-list spec)
                  (setq spec "error"))))
            ;; 3
            (let ((ok t))
              (if (string-match "(.*)" (car (split-string opcode-template "\n" t)))
                  (let ((opcode-name (if (assoc opcode csdoc-special-opcodes)
                                         (cadr (assoc opcode csdoc-special-opcodes))
                                       opcode)))
                    (puthash opcode-name
                             `(:function ,opcode-name
                               ;;; :doc ,opcode-doc
                               :html ,(file-name-nondirectory html-doc))
                             csdoc-opcode-database)
                    (setq ok nil))
                (setq spec
                      (mapconcat 
                       (lambda (s)
                         (if (string= opcode s)
                             (concat " " s " ")
                           (if (string-match "^[]\"aikxfwlcSTg.\\-]+$" s)
                               (cond
                                ((string-match "^[-a]+$" s) "a")
                                ((string-match "^[-k]+$" s) "k")
                                ((string-match "^[-i]+$" s) "i")
                                ((string-match "^[-f]+$" s) "f")
                                ((string-match "^[-w]+$" s) "w")
                                ((string-match "^[-S]+$" s) "S")
                                ((string-match "^[-T]+$" s) "T") ;; string or number (for "connect" only ?)
                                ((string-match "^[-\"]+$" s) "\"")
                                ((string-match "^[-l]+$" s) "l") ;; label
                                ((string-match "^[-\"ilc]+$" s) "X") ;; i, condition, filename
                                ((string-match "^[].\\-]+$" s) "") ;; dots are ignored, ] are begnin parsing errors
                                (t "x"))
                             (when ok
                               (add-to-list 'csdoc-irregular-opcodes opcode)
                               (message "failure for opcode: %s \nspec: %s\nat chunk:_%s_" opcode spec s))
                             (setq ok nil))))
                       (split-string spec " " t) "")))
              (when ok
                (puthash opcode 
                         `(:opcode ,opcode
                           :spec ,spec
                           :out ,(let ((out (car (split-string spec " " t))))
                                   (unless (string= out opcode) out))
                           :in ,(let ((in (car (last (split-string spec " " t)))))
                                  (unless (string= in opcode) in))
                           :template ,(when opcode-template
					(mapconcat 'identity 
						   (split-string opcode-template " \\\\")
						   ""))
                           :doc ,opcode-doc
                           :html ,(file-name-nondirectory html-doc))
                         csdoc-opcode-database)))))))))

(defun csdoc-read-deprecated (opcode)   
  (save-excursion
    (goto-char (point-min))
    (when (or (re-search-forward "Same as the .* href=\"\\(.*\\)\.html\" .* opcode\." nil t)
	      (and (search-forward "deprecated" nil t)
		   (re-search-forward "href=\"\\(.*\\)\.html\"" nil t)))
      (add-to-list 'csdoc-deprecated-opcodes 
		   (list opcode (match-string-no-properties 1))))))

(defun csdoc-read-template (opcode)   
  (let ((template (csdoc-read-manual-section "Syntax" nil t)))
    (when template
      (setq template (with-temp-buffer
                       (insert template)
                       (goto-char (point-min))
                       (keep-lines (format ".*%s.*" (downcase opcode)))
                       (buffer-string)))
      (if (string-match " *\n$" template) 
          (replace-match "" nil nil template)
        template))))

(defun csdoc-read-manual-section (label &optional stop-at-dot reject-div)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (format ">%s</h2" label) nil t)
      (car (split-string (csdoc-clean-html
                          (buffer-substring 
                           (search-forward ">") 
                           (save-excursion (re-search-forward (if reject-div "</?div" "</div"))
					   (re-search-backward (if reject-div "</?div" "</div")))))
                         (if stop-at-dot "\\." "\n\r") t)))))

; (csdoc-manual-opcodes-under '("Signal Input and Output"))
; (csdoc-manual-opcodes-under '("Instrument Control"))
(defun csdoc-manual-opcodes-under (categories)
  (let (opcodes)
    (loop for docfile in (directory-files (csdoc-html-directory) t "\\.html$")
          do (with-temp-buffer
               (insert-file-contents docfile)
               (goto-char (point-min))
               (dolist (category categories)
                 (when (search-forward 
			(format "align=\"center\">%s</th>"
				category) 
			nil t)
                   (while (re-search-forward 
			   "<a class=\"link\" href=\"\\([a-z0-9]+\\).html\" title=\"\\1\">" nil t)
                     (add-to-list 'opcodes (match-string 1)))))))
    opcodes))


(defun csdoc-clean-html (s)
  (setq s (delete ?' s))
  (setq s (delete ?\n s))
  (setq s (delete ?\r s))
  (while (string-match "</p>" s) (setq s (replace-match "\n" nil nil s)))
  (while (string-match "</pre>" s) (setq s (replace-match "\n" nil nil s)))
  (while (string-match "<[^<>]+>" s) (setq s (replace-match "" nil nil s)))
  (while (string-match "\t+" s) (setq s (replace-match " " nil nil s)))
  (while (string-match "  +" s) (setq s (replace-match " " nil nil s)))
  (when (string-match " *\n *$" s) (setq s (replace-match "" nil nil s)))
  (when (string-match "^ +" s) (setq s (replace-match "" nil nil s)))
  s)

;; --- queries: 

(defun csdoc-opcode (key opcode)
  (plist-get (gethash opcode (csdoc-opcode-database)) key))

(defun csdoc-opcodes ()
  "return all documented opcodes, including functions"
  (let (opcodes)
    (maphash (lambda (opcode spec) 
               (add-to-list 'opcodes opcode))
             (csdoc-opcode-database))
    opcodes))

(defvar csdoc-opcodes nil "All documented opcodes and functions")

(defun csdoc-functions ()
  "return all documented functions"
  (let (functions)
    (maphash (lambda (opcode spec) 
               (when (csdoc-opcode :function opcode)
                 (add-to-list 'functions opcode)))
             (csdoc-opcode-database))
    functions))

(defun csdoc-0-opcodes ()
  "return all documented opcodes with no output values"
  (let (opcodes)
    (maphash (lambda (opcode spec) 
               (unless (or (csdoc-opcode :function opcode)
                           (csdoc-opcode :out opcode))
                 (add-to-list 'opcodes opcode)))
               (csdoc-opcode-database))
    opcodes))

(defun csdoc-regexp-for-words (list)
  (autoload 'make-regexp "make-regexp")
  (let ((max-specpdl-size 1500))
    (format "\\<%s\\>" (make-regexp (copy-sequence list) t))))

(defun csdoc-regexp-for-words-to-eol (list)
  (autoload 'make-regexp "make-regexp")
  (let ((max-specpdl-size 1500))
    (format "\\<%s\\>[^;/\n\r\"]*" (make-regexp (copy-sequence list) t))))

;; ----------------------------------- classification:

(defvar csound-declaration-opcodes nil "Orchestra declaration opcodes")
(defvar csound-regexp-declaration nil "Regexp matching all orchestra declaration opcodes")
(defvar csdoc-general-opcodes () "Regular opcodes, with inputs and output\(s), no I/O")
(defvar csound-regexp-general-opcodes () "Regexp matching `csdoc-general-opcodes'")
(defvar csdoc-functions nil "Orchestra functions")
(defvar csound-regexp-functions nil 
  "Regexp matching all documented orchestra functions
\(used for indenting only)")
(defvar csdoc-0-opcodes () "All documented opcodes with no output value")
(defvar csound-regexp-0-opcodes nil)
(defvar csound-IO-opcodes nil "list of I/O opcodes.")
(defvar csound-regexp-IO nil "")
(defvar csound-control-flow-opcodes nil "Control flow opcodes")
(defvar csound-regexp-control-flow nil)
(defvar csdoc-undocumented-opcodes '() "keywords not documented that we want to be recognized as opcodes")
(defvar csound-special-opcodes nil "opcodes that should have specific indentation and/or fontification")
(defvar csdoc-opcode-typology 
  '((csound-declaration-opcodes . '("instr" "endin" "opcode" "endop"))
    (csound-regexp-declaration . (csdoc-regexp-for-words-to-eol 
                                  csound-declaration-opcodes))

    (csdoc-functions . (csdoc-functions))
    (csound-regexp-functions . (concat (csdoc-regexp-for-words csdoc-functions) 
                                       "[ \t]*("))

    (csdoc-0-opcodes . (append (csdoc-0-opcodes) 
			       '("FLpanel_end" "FLpack_end"
				 "connect" "alwayson")))
    (csound-regexp-0-opcodes . (csdoc-regexp-for-words csdoc-0-opcodes))

    (csdoc-undocumented-opcodes . '("printf" "printf_i" "0dbfs" "hrtfer" "tab" 
                                    "tabw" "tabw_i" "subinstrinit" "fluidControl"))

    (csound-control-flow-opcodes . (append 
				    '("setksmps")
				    (csdoc-manual-opcodes-under 
				     '("Program Flow Control"
				       "Initialization and Reinitialization"))))
    (csound-regexp-control-flow . (csdoc-regexp-for-words
                                   csound-control-flow-opcodes))

    (csound-IO-opcodes . (append '("xin" "xout"
				   "outleta" "outletk" "outletf"
				   "inleta" "inletk" "inletf")
                                 (csdoc-manual-opcodes-under 
				  '("Signal Input and Output"
				    ;; MIDI I/O :
				    "MIDI input"
				    "MIDI Message Output"
				    "Generic Input and Output"
				    "Note-on/Note-off Output"
				    "MIDI Message Output"
				    "System Realtime Messages"
				    "Slider Banks"))))
    (csound-regexp-IO . (csdoc-regexp-for-words csound-IO-opcodes))

    (csound-special-opcodes . (append csound-declaration-opcodes
                                      csound-control-flow-opcodes
                                      csound-IO-opcodes
                                      csdoc-0-opcodes
                                      csdoc-functions
                                      '("kr" "sr" "ksmps" "nchnls")))

    (csdoc-opcodes . (csdoc-opcodes))

    (csdoc-general-opcodes . (set-difference 
                              (union csdoc-opcodes
                                     csdoc-undocumented-opcodes)
                              csound-special-opcodes
                              :test 'string=))
    (csound-regexp-general-opcodes . (csdoc-regexp-for-words
                                      csdoc-general-opcodes)))
  "global variables used to classify opcodes, and how to build them
out of the opcode database")

;; -------------------------------------------------------------------- 

(defun csdoc-build-typology ()
  (interactive)
  "redefine the global variables used to classify opcodes; requires
that variable `csdoc-opcode-database' is not nil"
  (unless csdoc-opcode-database
    (error "No database. Call `csdoc-opcode-database'"))
  (dolist (var-spec csdoc-opcode-typology)
    (let ((var (car var-spec))
	  (val (eval (cdr var-spec))))
      (set var (if (listp val)
		   (sort val 'string<)
		 val))))
  ;; refresh font-lock setting for csound-orc-mode
  (when (featurep 'csound-orc)
    (setq csound-font-lock-keywords (csound-font-lock-keywords))))

(defun csdoc-opcode-database ()
  "Return variable `csdoc-opcode-database', initialize if nil"
  (unless csdoc-opcode-database
    (csdoc-populate-opcode-database)
    (csdoc-save-opcode-typology)
    (csdoc-save-opcode-database))
  csdoc-opcode-database)

(defun csdoc-refresh-opcode-database ()
  (interactive)
  (setq csdoc-opcode-database nil)
  (csdoc-opcode-database))

(defun csdoc-save-opcode-database ()
  (interactive)
  (when csdoc-opcode-database
    (let ((ofile (expand-file-name 
		  "csound-x-opcodes.el"
		  (file-name-directory (locate-library "csound-x")))))
      (with-temp-file ofile
	(insert (format "%S\n" '(setq csdoc-opcode-database (makehash 'equal))))
	(maphash
	 (lambda (opcode plist) 
	   (insert (format "%S\n"
			   `(puthash ,opcode ',plist csdoc-opcode-database))))
	 csdoc-opcode-database)
	(insert (format "%S" '(provide 'csound-x-opcodes))))
      (byte-compile-file ofile))))

(defun csdoc-save-opcode-typology ()
  (interactive)
  (csdoc-build-typology) ;; for safety
  (let ((ot-file (expand-file-name
		  "csound-opcode-typology.el"
		  (file-name-directory (locate-library "csound-x")))))
    (with-temp-file ot-file
      ;; typology + csdoc-irregular-opcodes
      (dolist (var (append (mapcar 'car csdoc-opcode-typology) 
			   '(csdoc-irregular-opcodes csdoc-deprecated-opcodes)))
	(insert (format "%S\n"
			`(setq ,var ',(eval var)))))
      ;; provide 'csound-x-opcodes
      (insert (format "%S" '(provide 'csound-opcode-typology))))
    (byte-compile-file ot-file)))

;; initialization from cache, if any:
(require 'csound-x-opcodes nil t)
(require 'csound-opcode-typology nil t)

;; safety check:
;;
;; ... for cases when csound-x-opcodes.el is not synchronized with csdoc-opcode-typology
;; (new defvars were not saved yet via (csdoc-save-opcode-database))
;; this should only happen while developping csound-x; users should not see this
(when (loop for var-spec in csdoc-opcode-typology
            thereis (null (eval (car var-spec))))
  (if csdoc-opcode-database
      (csdoc-build-typology)
    (when (yes-or-no-p "Missing opcode database. Build it ?")
      (csdoc-refresh-opcode-database))))

;;;-----------------------------------------------------------------------------------
;;;                  csound -z
;;;-----------------------------------------------------------------------------------

(defvar csdoc-opcodes-buffer-name "*csound -z*")

(add-to-list 'special-display-buffer-names csdoc-opcodes-buffer-name)

(defun csound-z (&optional function)
  (interactive)
  (set-buffer (get-buffer-create csdoc-opcodes-buffer-name))
  (erase-buffer)
  (shell-command (format "\"%s\" -z" (cscsd-csound-binary)) csdoc-opcodes-buffer-name)
  (when (and (csound-doc-available-p)
	     (require 'embedded-elisp-library nil t))     
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^[0-9]+ opcodes" nil t)
      (while (re-search-forward "[[:alnum:]_]+" nil t)
	(if (gethash (match-string 0) csdoc-html-opcodes)
	    (save-excursion
	      (beginning-of-line)
	      (eel-wake-up-buttons 
	       `((,(match-string 0) 
		  . 
		  '(lambda () 
		     (interactive)
		     (,(or function 'csdoc-html-document-opcode) 
		      ,(match-string 0))))))))))))


;;;-----------------------------------------------------------------------------------
;;;                  generating opcode library for Squeak Csound Blocks
;;;-----------------------------------------------------------------------------------

(defgroup muO-csound-doc nil
  "\(setting for µO deployement only - no public usage)"
  :group 'csound-doc
  :prefix "csdoc-")

(defcustom csdoc-squeak-opcode-library "csound opcodes.cs"
  "Name of the opcode library used by Csound Blocks for Squeak
\(used for deployement only - no public usage)"
  :type 'file
  :group 'spfa-paths
  :group 'muO-csound-doc)

(defun csdoc-squeak-opcode-library ()
  (expand-file-name (substitute-in-file-name csdoc-squeak-opcode-library)))

(defun csdoc-insert-doIt (code)
  "Insert a Smalltalk doIt as it appears in changeset files \(.cs) for Squeak"
  (while (string-match "!" code) 
    (setq code (replace-match "<--bouh-->" nil nil code)))
  (while (string-match "<--bouh-->" code) 
    (setq code (replace-match "!!" nil nil code)))
  (insert (format "\n''!%s!\n" code)))

(defun csdoc-write-opcode-library ()
  "Write the opcode library file used by Csound Blocks for Squeak
\(this is used for deployement only - no public usage)"
  (interactive)
  (with-temp-file (csdoc-squeak-opcode-library)
    (dolist (function csdoc-functions)
      (csdoc-insert-doIt (format "CsoundOpcode functions add: ('%s' -> '%s')" 
                                 function (csdoc-opcode :html function))))
    (dolist (opcode csdoc-opcodes)
      (csdoc-insert-doIt (format 
                          "CsoundOpcode registerOpcode: ((CsoundOpcode new)
name: '%s';
spec: '%s';
fullSpec: '%s';
doc: '%s';
html: '%s')" 
                          opcode 
                          (csdoc-opcode :spec opcode) 
                          (csdoc-opcode :template opcode)
                          (csdoc-opcode :doc opcode)
                          (csdoc-opcode :html opcode))))
    (csdoc-insert-doIt "CsoundOpcode HTMLpath: 'http://www.csounds.com/manual/html/'")
    (csdoc-insert-doIt "CsoundOpcode sortDatabase")))

;;;-----------------------------------------------------------------------------------
;;; this is it.

(provide 'csound-doc)

;; csound-doc.el ends here
