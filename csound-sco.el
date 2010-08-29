;;; -*- auto-recompile: t -*-

;;; csound-sco.el --- major mode for editing Csound scores

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

; ---------------
; initially based on the code by John Fitch (26 Oct 1996), version 1.05
; heavily hacked and adapted for Csound-X
; by Stef <hepta@zogotounga.net>

; last modification on October 27, 2009

(require 'csound-orc)
(require 'csound-mx)
(require 'tabify)

(defgroup csound-sco nil
  "csound-sco major mode settings"
  :group 'csound-x
  :prefix "csound-sco-")

(defcustom csound-sco-mode-indent-function 'csound-sco-indent
  "Function to be used for indenting lines in Csound scores."
  :type '(choice (const csound-sco-indent)
		 (const csound-sco-indent-by-block))
  :group 'csound-sco)

(defcustom csound-sco-mode-never-indent nil
  "When non-nil, automatic indentation via TAB is turned off."
  :type 'boolean
  :group 'csound-sco)

(defcustom csound-sco-indent-minimum-gap 2
  "Minimum number of spaces to be kept between p-fields when indenting.
A value of 0 means keep large gaps as they are, any other value implies an
initial compactification."
  :type 'integer
  :group 'csound-sco)

(defcustom csound-sco-indent-comment-gap 2
  "Minimum number of spaces to be kept between the last p-field and a
following comment when indenting."
  :type 'integer
  :group 'csound-sco)

(defcustom csound-sco-break-continuation-column 7
  "Column at which a broken line is to be continued."
  :type 'integer
  :group 'csound-sco)

(defcustom csound-sco-indent-do-break-line-p nil
  "When non-nil indenting a score line also force its breaking at column
`csound-x-break-column'."
  :type 'boolean
  :group 'csound-sco)

(defcustom csound-sco-mode-hook nil
  "Hook run when csound-sco-mode is started."
  :type 'hook
  :group 'csound-sco)

(defcustom csound-sco-mode-load-hook nil
  "Hook run when csound-sco-mode is loaded."
  :type 'hook
  :group 'csound-sco)


;;======================================================================
;                       csound-sco-mode
;;======================================================================

(defun csound-sco-mode ()
  "Mode for editing Csound Scores

\\{csound-sco-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map csound-sco-mode-map)
  (set-syntax-table csound-sco-mode-syntax-table)
  (setq mode-name "Csound Score")
  (setq major-mode 'csound-sco-mode)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (setq comment-start "; "
	comment-end ""
	comment-column 40
	indent-line-function csound-sco-mode-indent-function
	indent-region-function 'csound-sco-indent-region)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'csound-sco-mode-comment)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (setq csound-sco-inline-empty-comment-pattern "^.+;+ *$")
  (setq csound-sco-code-level-empty-comment-pattern "^[\t ]+;; *$")
  (setq csound-sco-flush-left-empty-comment-pattern "^;;; *$")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  ;; tool bar extension
  (csound-sco-define-tool-bar)
  ;; multiline comments:
  (add-to-list 'font-lock-extend-region-functions
	       'csound-scorc-extend-region-comment)
  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(csound-sco-font-lock-keywords
 			     t nil nil beginning-of-line))
  ;; menu
  (csound-install-sco-menu)
  (add-hook 'activate-menubar-hook 'cscsd-maybe-refresh-menu nil t)
  (run-hooks 'csound-sco-mode-hook)
;  (font-lock-mode 1)
  ;; matrices:
  (if (featurep 'csound-mx)
      (csound-mx-install)
      (scomx-highlight-all)))

(defvar csound-sco-mode-map (make-sparse-keymap) "Keymap for csound-sco-mode")

(defun csound-sco-define-tool-bar ()
  (when (boundp 'tool-bar-map)
    (defvar csound-sco-tool-bar-map (copy-sequence tool-bar-map))
    (let ((tool-bar-map csound-sco-tool-bar-map))
      (tool-bar-add-item-from-menu 
       'cscsd-process "csd-process" csound-sco-mode-map
       :enable (and (featurep 'csound-csd) (cscsd-indirect-buffer-p))))
    (set (make-local-variable 'tool-bar-map) csound-sco-tool-bar-map)))

(csound-sco-mode-populate-keymap)

(defvar csound-sco-mode-syntax-table nil "Syntax table")

(unless csound-sco-mode-syntax-table
  (setq csound-sco-mode-syntax-table
	(make-syntax-table text-mode-syntax-table)))


;;======================================================================
;                       indentation
;;======================================================================

(defun csound-sco-indent-command ()
  (interactive)
  (if csound-sco-mode-never-indent
      (insert ?\t)
    (funcall csound-sco-mode-indent-function)))

(defun csound-sco-indent-whole-block ()
  (interactive)
  (let ((csound-sco-mode-indent-function 'csound-sco-indent-by-block))
    (funcall csound-sco-mode-indent-function)))

(defun csound-sco-indent-all-blocks ()
  (interactive)
  (let ((csound-sco-mode-indent-function 'csound-sco-indent-by-block))
    (indent-region (csound-sco-point-min) (csound-sco-point-max))))

(defvar csound-sco-i-tab-stop-list
  (append '(2 4) (loop for tb upfrom 7 to (* 100 7) by 7 collect tb))
  "")

(defvar csound-sco-f-tab-stop-list
  (append '(2 4) (loop for tb upfrom 6 to (* 100 6) by 6 collect tb))
  "")

(defun csound-sco-indent ()
  "Default indentation function for scores."
  (save-excursion
    (goto-char (point-at-bol))
    (while (save-excursion
	     (forward-char -1)
	     (looking-back "\\\\.*" (point-at-bol)))
      (forward-line -1))
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (delete-horizontal-space)
      (cond 
       ((looking-at "[iftsarb0-9.+>~-]")  
	(let* ((tab-stop-list 
		(cond ((looking-at "i") 
		       csound-sco-i-tab-stop-list)
		      ((looking-at "f") 
		       csound-sco-f-tab-stop-list)
		      (t tab-stop-list))))
	  (cssco-i-tabulate)
	  (csound-tabulate-left-comment)))
       ;; comment line
       ((looking-at "[ \t]*;")
	(delete-horizontal-space))
       ;; end
       ((looking-at "[ \t]e") 
	(delete-horizontal-space)
	(csound-tabulate-left-comment))))
    (when csound-sco-indent-do-break-line-p
      (cscsd-break-line csound-sco-break-continuation-column))))

(defun csound-sco-indent-region (&optional start end)
  (interactive "r")
  (setq start (or start (point-min))
	end (or end (point-max)))
  (save-excursion
    (save-restriction
      (goto-char start)
      (goto-char (point-at-bol))
      (narrow-to-region (max 1 (1- (point))) 
			(save-excursion (goto-char end)
					(point-at-eol))) ;; no: line breaks !
      (while (zerop (forward-line 1))
	(funcall csound-sco-mode-indent-function)
	(when (eq csound-sco-mode-indent-function
		  'csound-sco-indent-by-block)
	  (while (and (looking-at "[iftsarb0-9.-;]")
		      (zerop (forward-line 1)))))))))

(defun csound-sco-indent-by-block ()
  "Indentation function for scores aligning all columns in a same block,
a block being a bunch of successive i-statements and possibly comment lines.
Blocks are separated by empty lines."
  (save-excursion
    (goto-char (point-at-bol))
    (delete-horizontal-space)
    (cond 
     ((looking-at "[iftsarb0-9.+>~-]")  
      (when (looking-at "[0-9.-]")
	(insert ? ))
      (csound-sco-indent-block))
     ;; non comment line
     ((not (looking-at ";"))
      (csound-tabulate-left-comment)))))

(defun cssco-tab-stop-list ()
  "Return a tab-stop-list for i-statement at point"
  (save-excursion
    (save-excursion
      (unless (or (re-search-forward "[ \t]$" (point-at-eol) t)
		  (search-forward ";" (point-at-eol) t))
	(goto-char (point-at-eol))
	(insert ? )))
    (let ((tsl (loop
		do (when (looking-at "\\[")
		     (search-forward "]" (point-at-eol) t))
		while (or
		       (re-search-forward "[ \t]+" (point-at-eol) t)
		       (re-search-forward "[ \t;]" (point-at-eol) t))
		until (looking-at ";")
		collect (current-column))))
      (if (> csound-sco-indent-minimum-gap 1)
	  (loop for i from 0 by (1- csound-sco-indent-minimum-gap)
		for j in tsl
		collect (+ i j))
	tsl))))

(defun cssco-align-tab-stop-lists (tab-stop-lists)
  (let (maxtab (modlists tab-stop-lists))
    (loop for i upto (apply 'max 
			    (mapcar 'length tab-stop-lists))
	  do (setq maxtab
		   (loop for tsl in modlists
			 maximize (or (nth i tsl) -1)))
	  do (setq modlists
		   (loop for tsl in modlists
			 collect (mapcar 
				  (lambda (n) 
				    (+ n (- maxtab 
					    (or (nth i tsl) 0))))
				  tsl)))
	  collect maxtab)))

(defmacro csound-sco-indent-iterating-over (loop-iterating)
  `(progn
     (cscsd-remove-breaks (csound-sco-point-min) 
			  (csound-sco-point-max))
     (when (,loop-iterating
	    thereis (re-search-forward "^[fi][ \t]+[^ \t]" (point-at-eol) t))
    ;; separate iN -> i N 
    (,loop-iterating
      when (re-search-forward "^[fi]\\([ \t]*\\)[^ \t]" (point-at-eol) t)
      do (replace-match " " nil nil nil 1)))
     (when (>= csound-sco-indent-minimum-gap 1) 
       ;; start with compact form
       (let ((tab-stop-list nil))
	 (,loop-iterating
	  do (cssco-i-tabulate))))
     (let ((tab-stop-list (cssco-align-tab-stop-lists 
			   (,loop-iterating
			    collect (cssco-tab-stop-list)))))
       (,loop-iterating
	;; apply tab stops
	do (cssco-i-tabulate)))
     (let ((comment-column 0))
       (,loop-iterating
	;; compute comment-column for the block
	do (when (search-forward ";" (point-at-eol) t)
	     (setq comment-column (max comment-column 
				       (1- (current-column))))))
       (,loop-iterating
	;; apply comment-column
	do (csound-tabulate-left-comment)))
     (when csound-sco-indent-do-break-line-p
       (,loop-iterating
	do (cscsd-break-line csound-sco-break-continuation-column)))))

(defmacro loop-for-i-statements-in-block (&rest body)
  `(save-excursion
     (goto-char (point-at-bol))
     (while (and (looking-at "[ \t]*[iftsarb0-9.+>~;-]")
		 (zerop (forward-line -1))))
     (goto-char (point-at-bol))
     (loop while (and (zerop (forward-line 1))
		      (looking-at "[ \t]*[iftsarb0-9.+>~;-]"))
	   if (looking-at "[ \t]*[if]")
	   ,@body)))

(defun csound-sco-indent-block ()
  (csound-sco-indent-iterating-over loop-for-i-statements-in-block))

(defun cssco-i-tabulate ()
  ""
  (cscsd-untabify-comment)
  (while (and (re-search-forward "[ \t]+" (point-at-eol) t)
	      (not (save-excursion (search-backward ";" (point-at-bol) t)))
	      (or (not (looking-at "$"))
		  (and (delete-horizontal-space) nil))
	      (or (not (looking-at "[;\]"))
		  (just-one-space csound-sco-indent-comment-gap)))
    (just-one-space) 
    (when (and tab-stop-list
	       (> (save-excursion 
		    (backward-char 1) (move-to-tab-stop) (point))
	       (point)))
      (tab-to-tab-stop))
    (when (looking-at "\\[")
      (search-forward "]" (point-at-eol) t)))
  (untabify (point-at-bol) (point-at-eol)))

;;=====================================================

(defun csound-sco-indent-by-instrument (&optional instruments)
  "Indentation function for scores aligning columns for a same instrument."
  (save-excursion
    (let (inum)
      (goto-char (point-at-bol))
      (delete-horizontal-space)
      (cond
       ((and (looking-at "i[ \t]*\\([^ \t]+\\)") 
	     (setq inum (match-string-no-properties 1))
	     (not (member inum 'instruments)))
	(add-to-list 'instruments inum)
	(csound-sco-indent-instrument inum))
       ((looking-at "[ftsarb0-9.-]")
	(when (looking-at "[0-9.-]")
	(insert ? ))
	(csound-sco-indent-block))))))

(defmacro loop-for-inum-statements (&rest body)
  `(save-excursion
     (goto-char (csound-sco-point-min))
     (loop while (zerop (forward-line 1))
	   if (and (looking-at "[ \t]*i[ \t]*\\([^ \t]+\\)[ \t]+")
		   (string= (match-string 1) inum))
	  ,@body)))

(defun csound-sco-indent-instrument (&optional inum)
  (interactive "sInstrument number or name: ")
  (csound-sco-indent-iterating-over loop-for-inum-statements))

(defun csound-sco-indent-all-instruments ()
  (interactive)
  (dolist (inum (csound-sco-instruments))
    (csound-sco-indent-instrument inum)))

(defun csound-sco-instruments ()
  (let (instruments)
    (save-excursion
      (goto-char (csound-sco-point-min))
      (loop while (zerop (forward-line 1))
	    if (looking-at "[ \t]*i[ \t]*\\([^ \t]+\\)[ \t]+")
	    do (add-to-list 'instruments (match-string-no-properties 1))))
    instruments))

;;=====================================================

(defun csound-sco-mode-comment ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ";")
	0
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun csound-sco-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments
  (cond ((looking-at ";")
	 (csound-sco-indent-command))
        ; otherwise goto end of line or sth else?
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (csound-sco-mode-comment))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
;;	   (beginning-of-line)
;;	   (insert "\n")
;;	   (forward-char -1)
	   )
	 (insert ";")
	 (insert-char '\; (- (calculate-csound-sco-indent) (current-column))))))

(defun csound-sco-find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (if (save-excursion
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point)) t))
      (let ((save-match-beginning (match-beginning 0))
	    (save-match-end (match-end 0)))
	(goto-char save-match-beginning)
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point)) t)
	(goto-char (match-end 0))
	t)
    nil))

(defun csound-sco-current-line-indentation ()
  "Indentation of current line
This is the column position of the first non-whitespace character"
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^;")
	   (goto-char (match-end 0))
	   (skip-chars-forward ";")))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun calculate-csound-sco-indent ()
  "Calculates the Csound indent column based on previous lines."
  (let (icol)
    (save-excursion
      (if (= (point) (csound-sco-point-min))
	  (setq icol 8)
	(save-excursion
	  (beginning-of-line)
	  (cond ((looking-at "^;")
		 (setq icol 0))
	  ;; Move past whitespace.
		(t (skip-chars-forward " \t")
		   (setq icol (current-column))))))
      (save-excursion
	(beginning-of-line)
	icol))))

;;======================================================================
;                       fontlock support
;;======================================================================

(dolist 
    (fspec
     '( ;; name  foreground  background  bold doc
       (csound-table-face "RoyalBlue" nil nil "f-tables")
       (csound-tableno-face "red" nil t "f-tables GEN type")
       (csound-i1-face "Sienna"	nil nil "i..1 statements")
       (csound-i2-face "DarkSlateBlue" "gray91" nil "i..1 statements")
       (csound-i3-face "SteelBlue" "gray92" nil "i..2 statements")
       (csound-i4-face "Brown" "gray93" nil "i..3 statements")
       (csound-i5-face "DarkOliveGreen" "gray94" nil "i..4 statements")
       (csound-i6-face "MediumSeaGreen" "gray95" nil "i..5 statements")
       (csound-i7-face "Maroon"	"gray96" nil "i..6 statements")
       (csound-i8-face "Plum" "gray97" nil "i..7 statements")
       (csound-i9-face "OrangeRed" "gray98" nil "i..8 statements")
       (csound-i0-face "Violet"	"gray99" nil "i..0 statements")
       (csound-section-face "Sienna" nil t "section statements")
       (csound-tempo-face "ForestGreen"	nil nil "tempo statements")))
(eval
 `(defface ,(nth 0 fspec) 
    '((t (:foreground ,(nth 1 fspec) 
          :background ,(nth 2 fspec)
          :bold ,(nth 3 fspec))))
    ,(format "Face for %s in csound scores" (nth 4 fspec))
    :group 'csound-faces)))

(defvar csound-sco-font-lock-keywords 
  (list
   ;; Comments
;   '(";.*$" . 'csound-comment-face)
   ;;
   ;; Fontify syntactically (assuming strings cannot be quoted or span lines).
   '("\"[^\"\n]*\"" . 'csound-string-face)
   ;;
   '("^[ \t]*[es]" . 'csound-section-face)
   ;; f-tables
   '("^[ \t]*\\(f[ \t]*[0-9]+[ \t]+[0-9.]+[ \t]+[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]+\\(.*\\)"
     (1 'csound-table-face)
     (2 'csound-tableno-face)
     (3 'csound-table-face keep t))
   ;;    
   '("^[ \t]*i[ \t]*[0-9]*1\\b.*" . 'csound-i1-face)
   '("^[ \t]*i[ \t]*[0-9]*2\\b.*" . 'csound-i2-face)
   '("^[ \t]*i[ \t]*[0-9]*3\\b.*" . 'csound-i3-face)
   '("^[ \t]*i[ \t]*[0-9]*4\\b.*" . 'csound-i4-face)
   '("^[ \t]*i[ \t]*[0-9]*5\\b.*" . 'csound-i5-face)
   '("^[ \t]*i[ \t]*[0-9]*6\\b.*" . 'csound-i6-face)
   '("^[ \t]*i[ \t]*[0-9]*7\\b.*" . 'csound-i7-face)
   '("^[ \t]*i[ \t]*[0-9]*8\\b.*" . 'csound-i8-face)
   '("^[ \t]*i[ \t]*[0-9]*9\\b.*" . 'csound-i9-face)
   '("^[ \t]*i[ \t]*[0-9]*0\\b.*" . 'csound-i0-face)
   '("^[ \t]*\\bs[0-9]*\\b.*" . 'csound-kvar-face)
   '("^[ \t]*\\b[btv][0-9]*\\b.*" . 'csound-tempo-face)
   ;;
;   '("\\b<\\|\\b>\\b\\|\\b{\\|\\b}\\b" . 'font-lock-string)
;   '("\\b+\\b" . 'font-lock-global)
;   '("\\b\\.\\b" . 'csound-tempo-face)
   ;;
   ;; #include & #define
   '("#" 0 'csound-define-face t)
   '("^[ \t]*#[ \t]*define\\([^(#]*\\)" 1 'csound-flow-face t)
   '("^[ \t]*#[ \t]*undef\\([^(#]*\\)" 1 'csound-flow-face t)
   '("^[ \t]*#[ \t]*include" 0 'csound-define-face t)
   '("^[ \t]*#[ \t]*define" 0 'csound-define-face t)
   '("^[ \t]*#" 0 'csound-define-face t)
   '("#[ \t]*$" 0 'csound-define-face t)
   '("^[ \t]*#[ \t]*undef" 0 'csound-define-face t)
   ;;
   ;; Macros
   '("\\$[a-zA-Z0-9_]+" 0 'csound-flow-face t)  
   ;;
   ;; Comments ; & /* */
   '("\\(/\\*[[:ascii:]]*?\\*/\\)[ \t]*$" 1 'csound-long-comment-face t)
   '("\\(;[^|\n\r].*$\\)" 1 'csound-comment-face t)
   ;;
   ;; Meta-labels
   '(";|.*$" 0 'csound-label-face t)
   '(";_.*_[be]\\.[0-9]+" 0 'csound-label-face t)
   )
  "Default expressions to highlight in Csound mode.")

;;======================================================================
;                       commenting/uncommenting
;;======================================================================

(defvar csound-sco-code-level-empty-comment-pattern nil
  "Pattern for entry line with comment")
(defvar csound-sco-flush-left-empty-comment-pattern nil
  "Pattern for left most comment")
(defvar csound-sco-inline-empty-comment-pattern nil
  "Patern for comment line inline")

(defun csound-sco-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun csound-sco-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) ?; )
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1)))

(defun csound-sco-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger csound-sco-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((csound-sco-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((csound-sco-line-matches "^[^;\n]+$")
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((csound-sco-line-matches csound-sco-flush-left-empty-comment-pattern)
    (insert ";"))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty. 
   ((csound-sco-line-matches csound-sco-code-level-empty-comment-pattern)
    (csound-sco-pop-comment-level)
    (insert ";;;"))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty. 
   ((csound-sco-line-matches csound-sco-inline-empty-comment-pattern)
    (csound-sco-pop-comment-level)
    (tab-to-tab-stop)
    (insert ";; "))

   ;; If all else fails, insert character
   (t
    (insert ";")))
  (end-of-line))

;; (defun uncomment-region ()
;;   "Call `\\[comment-region]' with an argument"
;;   (interactive)
;;   (comment-region (point) (mark) '(t)))

(defvar csound-sco-muter ";mute"
  "")

(defun csound-sco-comment-instrument (inum)
  "Comment or uncomment the score statements for instrument INUM"
  (interactive "nInstrument number: ")
  (save-excursion
    (goto-char (csound-sco-point-min))
    (let* ((muter (format "%s %s " csound-sco-muter inum))
           (commenting-out (not (search-forward muter (csound-sco-point-max) t))))
      (goto-char (csound-sco-point-min))
      (while (< (point) (csound-sco-point-max))
        (beginning-of-line)
        (when (and commenting-out 
		   (looking-at (format "i[ \t]*%d[ \t]" inum)))
          (insert muter))
        (when (and (not commenting-out)
                   (looking-at muter))
          (replace-match ""))
        (forward-line 1)))))

(defun csound-sco-uncomment-all-instruments ()
  (interactive)
  (save-excursion
    (goto-char (csound-sco-point-min))
    (while (re-search-forward (format "^%s [0-9.]+ " csound-sco-muter) 
			      (csound-sco-point-max) t)
      (replace-match ""))))


;;======================================================================
;                       CSD integration
;;======================================================================

(defun csound-sco-point-min ()
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-sco-beginning)
    (point-min)))

(defun csound-sco-point-max ()
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-sco-end)
    (point-max)))


;;======================================================================
;                       menu
;;======================================================================

(defun csound-install-sco-menu ()
  (easy-menu-define csound-sco-menu csound-sco-mode-map
    "Menu provided by `csound-sco-mode'"
    `("SCO"
      ["Comment Region" comment-region t]
      ["Uncomment Region" uncomment-region t]
      ["Power-of-2 Region" csound-e-power t]
      "--"
      ["Indent block as a whole" csound-sco-indent-whole-block t]
      ["Indent all blocks as whole" csound-sco-indent-all-blocks t]
      ["Indent instrument as a whole" csound-sco-indent-instrument t]
      ["Indent all instruments as whole" csound-sco-indent-all-instruments t]
      "--"
      ["Mute/unmute instrument" csound-sco-comment-instrument t]
      ["Unmute all instruments" csound-sco-uncomment-all-instruments t]
      "--"
      ["Wrap in CSD" cscsd-wrap-buffer
       :visible (and (cscsd-non-indirect-buffer-p)
		     (not (cscsd-buffer-is-a-csd-p)))]
      ["Send to muO"  muo-get-sco :visible (featurep 'surmulot) ]
      ["Visit associated orchestra" (find-file (funcall cscsd-associated-orc))
       :visible (and (not (cscsd-buffer-is-a-csd-p))
		     (funcall cscsd-associated-orc))]
;;       "--"
;;       ;; the repository submenu is only computed upon request:
;;       ,(or cscsd-repository-menu-cache (cscsd-make-repository-menu))
      "--"
      ["Display f-table"   csft-display-ftable t]
      ["Source of macro at point"        cscsd-source-macro 
       (thing-at-point-looking-at "\\$[a-zA-Z0-9_]+")]
      ["Visit included file at point"    cscsd-find-include  
       (thing-at-point-looking-at "^[ \t]*#include.*")]
      "--"
      ["Break lines when indenting"
       (setq csound-sco-indent-do-break-line-p 
	     (not csound-sco-indent-do-break-line-p))
       :style toggle
       :selected csound-sco-indent-do-break-line-p]
      ["Insert next line upon <RET>"
       (setq cscsd-sco-has-electric-return 
	     (not cscsd-sco-has-electric-return))
       :style toggle
       :selected cscsd-sco-has-electric-return]
;;       "--"
;;       ,(cscsd-processing-menu)
;;       ["Process source CSD"               (cscsd-process (buffer-base-buffer (current-buffer))) (cscsd-indirect-buffer-p)]
;;       ["Process via temp CSD" cscsd-process-scorc-via-temp-csd
;;        (and (cscsd-non-indirect-buffer-p) (not (cscsd-buffer-is-a-csd-p)))]
      "--"
      ["Compile score (to DAC)"          cscsd-play-score (null (cscsd-buffer-is-a-csd-p))]
      ["Compile score (to file)"          cscsd-compile-score (null (cscsd-buffer-is-a-csd-p))]
      ["Play file"           cscsd-play-audio (cscsd-audio-available-p)]
      ["Edit file"           cscsd-edit-audio (cscsd-audio-available-p)])))

(defvar cscsd-sco-has-electric-return nil
  "")
(make-local-variable 'cscsd-sco-has-electric-return)

(defun cscsd-sco-electric-return ()
  ""
  (interactive)
  (if (and cscsd-sco-has-electric-return
           (scomx-i-statement-p)
           (numberp (read (match-string 2)))
           (numberp (read (match-string 3))))
      (progn
        (goto-char (point-at-eol))
        (insert "\ni " (match-string 1) 
                (format " %s" (+ (read (match-string 2))
                                 (read (match-string 3))))
                " " (match-string 3))
        (when (match-string 4)
          (insert (match-string 4))))
    (insert "\n")))

;;======================================================================

(provide 'csound-sco)
(run-hooks 'csound-sco-mode-load-hook)

;;; csound-sco.el ends here
