;;; -*- auto-recompile: t -*-

;;; csound-mx.el --- defining and working on matrices in Csound scores

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-mx.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-mx.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;;; Commentary:
;;-------------

;;    full documentation here:
;;    http://www.zogotounga.net/comp/csoundx_doc_mx.html

;;
;; ==========================================================
;; this file should be installed through the csound-x package
;; ==========================================================
;;
;; but, if you want to install it independently,
;; add the following to your .emacs:
;;
;;   (require 'csound-mx)
;;   (add-hook 'csound-sco-mode-hook 'csound-mx-install)

;; last modified March 5, 2006

;;;=====================================================================
;;; Code:

(eval-when-compile       ;; ?
  (load "calc"))

;; automatic SES extensions: 
(if (require 'ses nil t)
    (require 'csound-ses))

(defgroup csound-mx nil
  "Matrices in Csound scores"
  :group 'csound-x
  :prefix "scomx-")

(defcustom scomx-calc-precision 8             
  "calc precision (the lower the faster)"
  :type 'integer
  :group 'csound-mx)

(defcustom scomx-propose-algebraic-input-first t                
  "if t, the main input format for formulas is algebraic"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-highlight-matrices t
  "weither matrices should be automatically highlighted"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-selection-color "yellow"
  "color or face used by default to highlight selected matrices"
  :type '(choice color plist)
  :group 'csound-mx)

(defcustom scomx-main-selection-color '(:weight bold :background "yellow")
  "color or face used to highlight the current selected matrice"
  :type '(choice color plist)
  :group 'csound-mx)

(defcustom scomx-colors-alist '(("m1" . "skyblue")
				("m2" . "lavender")
				("m3" . "lawngreen")
				("m4" . "khaki")
				("mbold" :slant italic :weight bold)
				("minv" :inverse-video t)
				("m5" :foreground "red" :background "yellow"))
  "colors or faces to be used for highlighting specifically named matrices"
  :type '(alist :key-type string :value-type (choice color plist))			  
  :group 'csound-mx)

(defcustom scomx-toggle-truncate-at-first t
  "weither we should toggle Truncate Lines when starting"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-no-i-macros t
  "if nil, a line starting with a macro is always considered as a i-statement"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-i-macros '("piano" "bass")
  "list of macros names interpreted as i-statements when beginning a score line"
  :type '(repeat string)
  :group 'csound-mx)


(defvar scomx-ALL "-ALL-"
  "keyword used (internaly) as a special SID for indicating all selections")

;;;-----------------------------------------------------------------------------------
;;; installation (this is called once when starting csound-sco mode)

(require 'calc-stef)

(defun csound-mx-install ()

  ;; initialisation de calc:
;  (load "calc-mat")
;  (load "calc-stef")

  ;; implementing operators in 'csound-score calc-language:
  (defun calcFunc-potwo (x) (let ((pw 2))
		       (while (< pw x) 
			 (setq pw (* 2 pw)))
		       pw))
  (defun calcFunc-oppotwo (x) (let ((pw 2))
			(while (< (+ 1 pw) x)
			  (setq pw (* 2 pw)))
			(+ 1 pw)))
  (put 'csound-score 'math-oper-table       ;; table borrowed from calc-lang.el
       '( ("u+" ident -1 1000 )
	  ("u-" neg -1 1000 )
	  ("^" ^ 201 200)
	  ("@" calcFunc-potwo -1 1000)
	  ("@@" calcFunc-oppotwo -1 1000)   ;; this one doesn't work (why ?)
	  ("!!" calcFunc-oppotwo -1 1000)   ;; so we use this one through scomx-code-parameters
	  ("*" * 190 191 )
	  ("/" / 190 191 )
	  ("%" % 190 191 )
	  ("+" + 180 181 )
	  ("-" - 180 181 )))


  ;; setting calc internal precision
  (calc-precision scomx-calc-precision)

  ;; create the Matrix menu
  (easy-menu-define scomx-menu csound-sco-mode-map
    "Menu provided by csound-mx for csound-sco-mode"
    `("Matrix"
      ["Refresh display"             scomx-highlight-all t]
      "--"
      ["Select rectangular region"   scomx-select-rectangle t]
      ["Set current width"           scomx-change-horizontal-extension t]
      ["Extend selection to point"      scomx-extend-matrix-to-point t]      
      ["Select column"               scomx-select-column t]
      ["Select column (full range)"  (scomx-select-column (read-from-minibuffer "id: ") t) t]
      ["Store current selection"     scomx-name-current (scomx-selection-defined-p "")]
      ["Make a selection current"    scomx-copy-as-current (scomx-matrices-p)]
      ["Remove a selection"          scomx-remove-selection (scomx-matrices-p)]
      "--"
      ["XML to spreadsheet"          (scomx-restore-ses-area) (and (featurep 'csound-ses)
								   (cscsd-buffer-is-a-csd-p)
								   (cscsd-SES-areas))]
      ["Spreadsheet to XML"          (scomx-import-ses-area) (and (featurep 'csound-ses) 
								  (cscsd-buffer-is-a-csd-p)
								  (scomx-associated-SES-buffers))]
      ["Selection to spreadsheet"    (scomx-edit-in-ses) (and (featurep 'csound-ses)
							      (scomx-matrices-p))]
      ["Spreadsheet to selection"    (scomx-import-from-ses) (and (featurep 'csound-ses)
								  (scomx-associated-SES-buffers))]
      "--"
      ["Remove comments in region"   scomx-nocomment-region t]
      ["Sort"                        scomx-sort-along (scomx-selection-defined-p "")]
      "--"
      ["Operate on matrix"           scomx-mop (scomx-selection-defined-p "")]
      ["Apply script on matrix"      scomx-apply-script (scomx-selection-defined-p "")]
      ["Operate on matrix columns"   (scomx-operate-columns "") (scomx-selection-defined-p "")]
      ["Operate on parameters"       scomx-pop (scomx-selection-defined-p "")]
      "--"
      "repository:"
      ["Update"                      scomx-change-Matrix-menu t]))
  (scomx-change-Matrix-menu)

  ;; display the current matrices if necessary
  (scomx-maybe-display scomx-ALL)
;  (add-hook 'post-command-hook (lambda () (scomx-maybe-display scomx-ALL)) nil t) 
;;; trop lent !!

  ;; toggle truncate lines if required
  (if scomx-toggle-truncate-at-first
    (condition-case nil
      (toggle-truncate-lines) ;; no argument for emacs 21.2 and earlier
      (error (toggle-truncate-lines nil)))))


;;=================================================================================
;;                      queries, motions & other low-level functions
;;=================================================================================

;; note: the following defvars supposes there is never any space character in [] expressions
;; (but you can have spaces: this is handled as a special case in the appropriate functions)

(defvar scomx-between-columns-regexp "\\(^i[ \t]*\\)\\|\\([\t ]+\\)"
  "regexp attempting to identify all possible types of colum separators in a score") 

(defvar scomx-column-regexp "[^][ \t\n\r]+"
  "regexp attempting to identify all possible types of p-fields (colums) in a score") 

(defvar scomx-chars-in-columns "^ \t\n\r"
  "regexp to be used with skip-chars-forward in order to get to the end of a column") 


(defun scomx-current-column ()
  "returns the number of the column at point in the score
comments are ignored: if the point is in a comment, the last column number is returned"
  (let ((c 0)
	(end (1+ (point))))
    (save-excursion
      (if (thing-at-point-looking-at ";.*$") ;; on est dans un commentaire..
	  (setq end (progn                   ;; -> renvoie à la colonne précédente
		      (beginning-of-line)
		      (search-forward ";")
		      (skip-chars-backward " \t;")
		      (point))))
      (scomx-beginning-of-i-statement)
      (while (<= (point) end)
	(unless (and (re-search-forward scomx-between-columns-regexp (point-at-eol) t)
		     (not (looking-at "\\($\\|;\\|/\\)")))
	  (progn
	    (forward-line 1)
	    (re-search-forward (concat "^\\(" scomx-between-columns-regexp "\\)") (point-at-eol) t)))
	(setq c (1+ c))
	(if (looking-at "\\[")
	    (forward-sexp))))
    (if (and (<= c 1)
	     (scomx-i-statement-p))
	1
      (1- c))))

(defun scomx-beginning-of-i-statement ()
  "goto the beginning of the current i-statement
return nil if current-line is not part of an i-statement, else return point"
  (when (save-excursion
	  (beginning-of-line)
	  (looking-at "^[ \ti]*[.0-9-]"))
    (while (null (scomx-i-statement-p))
      (forward-line -1))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))

(defun scomx-end-of-i-statement ()
  (while (scomx-next-i-statement-line)))

(defun scomx-next-i-statement-line ()   ;;;;;;;;; c'est blindé, ça ?????
  (end-of-line)
  (let ((moved (looking-at "[\n\r][ \t]*[.0-9-]"))) 
    (if moved (forward-line 1))
    (search-backward ";" (point-at-bol) t)
    (search-backward "/*" (point-at-bol) t)
    moved)) 

(defun scomx-goto-column (n)
  "goto the beginning of column N in the current line of score
returns the position, or nil if there is no such column (in this case, the point ends up at the beginning of the last column)"
  (scomx-beginning-of-i-statement)
  (let* ((beg (point))
	 (end (save-excursion (re-search-forward "\\(;\\|$\\|/\\*\\)")
			      (skip-chars-backward "; \t")         
			      (if (< (point) beg) beg (point))))
	 ok )
    (dotimes (nc n)
      (setq ok (re-search-forward scomx-between-columns-regexp end t))
      (if (looking-at "\\[")
	  (forward-sexp))
      (unless ok
	(when (save-excursion (scomx-next-i-statement-line))
	  (forward-line 1)
	  ;;simplifier les redondances:
	  (setq end (save-excursion (re-search-forward "\\(;\\|$\\|/\\*\\)")
				    (skip-chars-backward "; \t")
				    (if (< (point) beg) beg (point)))
		ok (progn (re-search-forward (concat "^\\(" scomx-between-columns-regexp "\\)") end t)
			  (looking-at scomx-column-regexp))))))
    (if (save-excursion (backward-char 1) (looking-at "\\]"))
	(backward-sexp))
    (and ok (point))))

(defun scomx-goto-end-of-column (n)
  "goto the end of column N in the current line of score
returns the position, or nil if there is no such column (in this case, the point ends up at the end of the last column)"
  (if (scomx-goto-column n)
      (re-search-forward scomx-column-regexp)))

(defun scomx-i-statement-p ()
  "Tests weither the line at point is an i-statement
If it is one, returns its number else returns nil
As a special case, if scomx-no-i-macros is non nil, a line starting with a macro $likethis. is considered as a i-statement; in this case the macro name is returned.
Otherwise, such a macro is considered a i-statement if its name is listed in scomx-i-macros.

When an i-statement is found,
\(match-string 1) stores the i number \(p1)
\(match-string 2) stores p2
\(match-string 3) stores p3
\(match-string 4) stores all other parameters and possibly comments or is nil
"
  (save-excursion
    (beginning-of-line)
    (if (looking-at 
         "[ \t]*i[ \t]*\\([0-9.]+\\)[ \t]+\\([e0-9.+-]+\\)[ \t]+\\([e0-9.-]+\\)\\(.*\\)")
	(string-to-number (match-string 1))
      (if (looking-at 
           "[ \t]*\\$\\([0-9a-zA-Z]+\\)[ \t]+\\([e0-9.+-]+\\)[ \t]+\\([e0-9.-]+\\)\\(.*\\)")
	  (let ((macro-name (match-string 1)))
	    (if (or (null scomx-no-i-macros)
		    (member macro-name scomx-i-macros))
		macro-name
	      nil))
	nil))))

(defun scomx-search-next-tag (&optional bound)
  (if (re-search-forward ";_[a-zA-Z0-9]*_[eb]\\.[0-9]+" bound t)
      (match-string 0)
    nil))                                    

(defun scomx-matrices-p ()
  (save-excursion
    (goto-char (point-min))
    (and (scomx-search-next-tag)
	 (scomx-search-next-tag))))

(defun beginning-of-score () 
  (goto-char (point-min))
  (or (search-forward "<CsScore>" nil t)
      (goto-char (point-min))))

(defun end-of-score ()   
  (goto-char (point-min))
  (or (search-forward "</CsScore>" nil t)
      (goto-char (point-max))))

(defun scomx-beg-column (sid)
  "returns the first column of selection SID"
  (beginning-of-score)
  (if (search-forward (scomx-beg-tag sid) nil t)
      (string-to-number (word-at-point))))

(defun scomx-end-column (sid)
  "returns the last column of selection SID"
  (beginning-of-score)
  (if (search-forward (scomx-end-tag sid) nil t)
      (string-to-number (word-at-point))))

(defun scomx-selection-defined-p (sid)
  "test weither SID is a valid selection id"
  (save-excursion
    (and (scomx-beg-column sid)
	 (scomx-end-column sid))))

(defun scomx-goto-selection (sid)
  (scomx-goto-column (scomx-beg-column sid)))

(defun scomx-goto-selection-row (row sid)
  (let ((line ""))
    (scomx-goto-selection sid)
    (while (and (> row 1)
		(null (string= line (what-line))))
      (setq line (what-line))
      (forward-line 1)
      (if (scomx-i-statement-p)
	  (decf row))))
  (beginning-of-line))

;;=================================================================================
;;                                selecting
;;=================================================================================

(defun scomx-select-column (&optional sid &optional full-range)
  "set the current column as selection SID
if FULL-RANGE is t, do not consider comments nor empty lines nor empty p-field as column delimiters"
  (interactive)
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (scomx-remove-selection sid)
  (let ((mcol (scomx-current-column))
	(line "")
	(so-far (point)))
    (save-excursion
      (while (and (or full-range (and (scomx-i-statement-p)
				      (scomx-goto-column mcol)))
		  (not (equal line (what-line)))
		  (> (point) (save-excursion (beginning-of-score) (point))))
	(when (scomx-beginning-of-i-statement)
	  (setq so-far (point)))
	(setq line (what-line))
	(forward-line -1))
      (goto-char so-far)
      (scomx-add-tag (scomx-beg-tag sid mcol)))
    (setq line ""
	  so-far (point))
    (save-excursion
      (while (and (or full-range (and (scomx-i-statement-p)
				      (scomx-goto-column mcol)))
		  (not (equal line (what-line)))
		  (< (point) (save-excursion (end-of-score) (point))))
	(when (scomx-beginning-of-i-statement)
	  (setq so-far (point))
	  (scomx-end-of-i-statement))
	(setq line (what-line))
	(forward-line 1))
      (goto-char so-far)
      (scomx-add-tag (scomx-end-tag sid mcol)))
    (scomx-maybe-display sid)))

(defun scomx-select-rectangle (beg end &optional sid)
  "set the rectangular region from BEG to END as selection SID"
  (interactive "r")
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (let ((mbeg (make-marker))
	(mend (make-marker)))
    (set-marker mbeg beg)
    (set-marker mend end)    
    (scomx-remove-selection sid)
    (save-excursion
      (goto-char mbeg)
      (scomx-add-tag (scomx-beg-tag sid (scomx-current-column)))
      (goto-char mend)
      (scomx-add-tag (scomx-end-tag sid (scomx-current-column))))
    (scomx-maybe-display sid)))

(defun scomx-current-selection-p ()
  "tests weither a current selection (i.e. with SID \"\") is defined"
  (save-excursion
    (beginning-of-score)
    (and (search-forward (scomx-beg-tag "") nil t)
	 (search-forward (scomx-end-tag "") nil t))))

(defun scomx-change-horizontal-extension ()
  "Query through the minibuffer for beginning and starting columns of the current selection.
To have a one-column matrix in one shot you may also give a negative number as the beginning value"
  (interactive)
  (unless (scomx-current-selection-p)
    (scomx-select-column ""))
  (save-excursion
    (let* ((beg (read-calc-expr-from-minibuffer "begin at column: "))
	   (end (if (< beg 0)
		    (* -1 beg)
		  (read-calc-expr-from-minibuffer "end at column: "))))
      (scomx-select-rectangle
       (save-excursion
	 (scomx-beg-column "")
	 (scomx-goto-column (abs beg))
	 (point))
       (save-excursion
	 (scomx-end-column "")
	 (scomx-goto-column end)
	 (point))
       ""))))

(defun scomx-extend-matrix-to-point (&optional sid)
  "Redefine the size of selection SID so that point defines one of its corners
If SID is not defined, select the p-field at point as the single element of a new matrix"
  (interactive "smatrix id: ")
  (let ((mcol (scomx-current-column)))
    (if (null (scomx-selection-defined-p sid))
	(save-excursion
	  (scomx-add-tag (scomx-beg-tag sid mcol))
	  (scomx-add-tag (scomx-end-tag sid mcol))
	  (scomx-highlight sid))
      (let* ((bcol (save-excursion (scomx-beg-column sid)))
	     (ecol (save-excursion (scomx-end-column sid)))
	     (beg (save-excursion (scomx-goto-column (scomx-beg-column sid))))
	     (end (save-excursion (scomx-goto-column (scomx-end-column sid))))
	     (obeg (save-excursion (scomx-beg-column sid)
				   (scomx-goto-column ecol)))
	     (oend  (save-excursion (scomx-end-column sid)
				    (scomx-goto-column bcol))))
	(when (> bcol ecol)
	  (rotatef bcol ecol)
	  (rotatef beg obeg)
	  (rotatef end oend))
	;;  so that we always have:             bcol      ecol
	;;                                   beg +----------+ obeg
	;;                                       |          |
	;;                                  oend +----------+ end
	;;
	(cond ((and (<= (point) beg) (<= mcol bcol)) (setq beg (point)))
	      ((and (>= (point) end) (>= mcol ecol)) (setq end (point)))
	      ((and (>= (point) end) (<= mcol bcol)) (setq end (point) beg obeg))
	      ((and (<= (point) beg) (>= mcol ecol)) (setq beg (point) end oend))
	      ((< (abs (- mcol ecol)) (abs (- mcol bcol)))
	       (if (< (- (point) obeg) (- end (point)))
		   (setq beg (point) end oend)
		 (setq end (point))))
	      (t (if (< (- (point) beg) (- oend (point)))
		     (setq beg (point))
		   (setq end (point) beg obeg))))
	(scomx-select-rectangle beg end sid)))))

(defun scomx-name-current (&optional name)
  ""
  (interactive) 
  (if (scomx-current-selection-p)
      (progn
	(setq name (or name (read-from-minibuffer "id: ")))
	(save-excursion
	  (beginning-of-score)
	  (while (search-forward ";__" nil t)
	    (replace-match (concat ";_" name "_"))))
	(scomx-maybe-display name))))

(defun scomx-copy-as-current (&optional name)
  ""
  (interactive) 
  (setq name (or name (read-from-minibuffer "id: ")))
  (scomx-remove-selection "")
  (save-excursion
    (beginning-of-score)
    (while (re-search-forward (concat ";_" name "_\\([be]\\.[0-9]+\\)") nil t)
      (insert (concat " ;__" (match-string 1)))))
  (scomx-maybe-display scomx-ALL))

(defun scomx-remove-selection (&optional sid)
  "Removes selection SID"
  (interactive)
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (scomx-invisible sid)
  (save-excursion
    (beginning-of-score)              
    (let (ntag)
      (while (setq ntag (scomx-search-next-tag))
	(if (string-match (concat ";_" sid "_") ntag)
	    (kill-region (point) (- (search-backward ntag) 1)))))))

(defun scomx-beg-tag (sid &optional column)
  (concat ";_" sid "_b." (if column (number-to-string column) "")))

(defun scomx-end-tag (sid &optional column)
  (concat ";_" sid "_e." (if column (number-to-string column) "")))

(defun scomx-add-tag (tag)
  (save-excursion
    (end-of-line)
    (insert " " tag)))

(defun scomx-get-all-selections ()
  "returns a list of all defined selections"
  (let ((liss-b ())
	(liss-e ())
	tag elm)
    (union (and (scomx-selection-defined-p "") '(""))
	   (save-excursion
	     (beginning-of-score)
	     (while (setq tag (scomx-search-next-tag))
	       (setq elm (split-string tag "[_.]" t))
	       (if (string= (caddr elm) "b")
		   (setq liss-b (append liss-b (list (cadr elm))))
		 (if (string= (caddr elm) "e")
		     (setq liss-e (append liss-e (list (cadr elm)))))))
	     (intersection liss-e liss-b :test 'equal)))))

(defun scomx-selection-beginning (sid)  
  (save-excursion
    (beginning-of-score)
    (if (re-search-forward (scomx-beg-tag sid) nil t)
	(progn
	  (search-backward ";")
	  (scomx-goto-column 
	   (string-to-number (cadr (split-string (scomx-search-next-tag) "\\." t))))
	  (point)))))

(defun scomx-selection-end (sid)
  (save-excursion
    (beginning-of-score)
    (if (re-search-forward (scomx-end-tag sid) nil t)
	(progn
	  (search-backward ";")
	  (scomx-goto-end-of-column 
	   (string-to-number (cadr (split-string (scomx-search-next-tag) "\\." t))))
	  (point)))))


;;=================================================================================
;;                                    display
;;=================================================================================

(defun scomx-maybe-display (sid)
  (if scomx-highlight-matrices
      (if (string= sid scomx-ALL)
	  (scomx-highlight-all)
	(scomx-highlight sid))))

(defun scomx-invisible (&optional sid)
  (interactive)
  (mapcar (lambda (ov)
	    (let ((id (overlay-get ov 'sid))
		  (ok (overlay-get ov 'scomx)))
	    (if (and ok
		     (or (null id) (equal id sid)))
		(delete-overlay ov)))) 
	  (scomx-overlays-in-score)))

(defun scomx-highlight (&optional sid &optional color)  
  (interactive)
  (setq color (or color (cdr (assoc sid scomx-colors-alist)))
	color (cond (color color)
		    ((null sid) scomx-selection-color)
		    ((string= sid "") scomx-main-selection-color)
		    ((color-defined-p sid) sid)
		    (t scomx-selection-color)))
  (save-excursion
    (let* ((sid (or sid ""))
	   (deb-col (scomx-beg-column sid))
	   (end-col (scomx-end-column sid))
	   (tmp (if (> end-col deb-col) end-col deb-col))
	   (deb-col (if (> end-col deb-col) deb-col end-col))
	   (end-col tmp)
	   (end (save-excursion
		  (goto-char (scomx-selection-end sid))
		  (beginning-of-line)
		  (point)))                            
	   (lend 0))
      (scomx-invisible sid)
      (goto-char (scomx-selection-beginning sid))
      (scomx-beginning-of-i-statement)
      (while (<= lend end)
	(if (and (scomx-i-statement-p)
		 (scomx-goto-column deb-col))
	    (let ((tov (make-overlay (point)
				     (setq lend (or (scomx-goto-end-of-column end-col)
						    (point))))))
	      (overlay-put tov 'face (if (stringp color)
					 `(:background ,color)
				       color))
	      (overlay-put tov 'sid sid)
	      (overlay-put tov 'scomx t)))
	(forward-line 1)
	(beginning-of-line)))))

(defun scomx-all-invisible ()
  (interactive)
  (mapcar 'scomx-invisible (scomx-get-all-selections)))

(defun scomx-highlight-all ()
  (interactive)
  (mapcar (lambda (ov) (if (overlay-get ov 'scomx) (delete-overlay ov))) 
	  (scomx-overlays-in-score))
  (mapcar 'scomx-highlight (scomx-get-all-selections)))

(defun scomx-overlays-in-score ()
  (overlays-in (save-excursion (beginning-of-score) (point))
	       (save-excursion (end-of-score) (point))))


;;=================================================================================
;;                        grabbing & yanking as plain text
;;=================================================================================

(defun scomx-grab-matrix-as-text (sid)      
  (save-excursion
    (let* ((deb-col (scomx-beg-column sid))
	   (end-col (scomx-end-column sid))
	   (tmp (if (> end-col deb-col) end-col deb-col))
	   (deb-col (if (> end-col deb-col) deb-col end-col))
	   (end-col tmp)
	   str
	   (end (scomx-selection-end sid)))
      (goto-char (scomx-selection-beginning sid))
      (scomx-beginning-of-i-statement)
      (let ((lend 0))
	(while (<= lend end)
	  (when (scomx-i-statement-p)
	      (setq str (concat (if str (format "%s\n" str) "")
				(buffer-substring-no-properties
				 (scomx-goto-column deb-col) 
				 (setq lend (or (scomx-goto-end-of-column end-col) (point)))))))
	  (forward-line 1)
	  (if (save-excursion (not (zerop (forward-line 1))))
	      (setq lend (1+ end))
	    (setq lend (1+ lend)))
	  (beginning-of-line))
	str))))

(defun scomx-replace-matrix-from-text (str sid)
  "write STR, a string, in place of the score selection SID
STR must fit SID dimensions (although empty elements are allowed)"
  (scomx-yank-matrix
   (with-temp-buffer
     (insert str)
     (goto-char (point-min))
     (dolist (line (split-string str "\n"))
       (insert "i " line "\n"))
     (scomx-select-rectangle (save-excursion (goto-char (point-min)) (re-search-forward "[[:print:]]"))
			     (re-search-backward "[[:print:]]")
			     "bof")
     (scomx-grab-matrix "bof"))
   sid))

(defmacro with-matrix-buffer (sid &rest body)
  (declare (indent 1))
  `(let* ((str (scomx-grab-matrix-as-text ,sid))
	  (newstr (save-excursion
		    (with-temp-buffer
		      (insert str)
		      ,@body
		      (buffer-string)))))
     (scomx-replace-matrix-from-text newstr ,sid)))

(defun scomx-shell-command-with-matrix (command sid)
  "invoke the shell command COMMAND INPUT-FILE > OUTPUT-FILE
where INPUT-FILE is a temporary file containing selection SID as plain text,
and OUTPUT-FILE a temporary file where a replacement for SID is read"
  (let ((input-file (make-temp-file "csoundmx"))
	(output-file (make-temp-file "csoundmx")))
    (unwind-protect
	(with-matrix-buffer sid
	  (write-region (point-min) (point-max) input-file)
	  (shell-command (format "%s < \"%s\" > \"%s\"" command input-file output-file))
	  (erase-buffer)
	  (insert-file-contents output-file))	
      (delete-file input-file)
      (delete-file output-file))))

(defun scomx-apply-script (&optional script)
    (interactive "sScript:")
    (scomx-shell-command-with-matrix script ""))

;;=================================================================================
;;                                 grabbing & yanking 
;;=================================================================================


(defun scomx-elt (col row &optional sid set-to)
  "fetch the element, a string or nil, at column COL and row ROW in matrix SID
if SID is not provided, it defaults to \"\", the current selection.
columns and rows indexes start at 1
out-of-range COL and ROW lead to undefined results, with no error raised

If SET-TO is non nil, it replaces the previous matrix element
SET-TO may be a string or a number"
  (if (scomx-selection-defined-p (setq sid (or sid "")))
      (save-excursion
	(let ((ecol (scomx-end-column sid))
	      (bcol (scomx-beg-column sid))
	      (line ""))
	  (if (< ecol bcol)
	      (rotatef ecol bcol))
	  (scomx-goto-selection-row row sid)
	  (scomx-goto-column (+ col (1- bcol)))
	  (if (thing-at-point-looking-at "[^ \t\n\r]+")
	      (progn
		(if (stringp set-to) 
		    (replace-match set-to)
		  (if set-to
		      (replace-match (number-to-string set-to))))
		(match-string-no-properties 0)))))))


(defun scomx-grab-matrix (sid)      
  (save-excursion
    (let* ((deb-col (scomx-beg-column sid))
	   (end-col (scomx-end-column sid))
	   (tmp (if (> end-col deb-col) end-col deb-col))
	   (deb-col (if (> end-col deb-col) deb-col end-col))
	   (end-col tmp)
	   (mtx '(vec))
	   (end (scomx-selection-end sid)))
      (goto-char (scomx-selection-beginning sid))
      (scomx-beginning-of-i-statement)
      (setcdr mtx nil)  ; à cause du nconc plus bas...
      (let ((lend 0))
	(while (<= lend end)
	  (if (scomx-i-statement-p)
	      (let ((col deb-col)
		    (v '(vec)))
		(while (<= col end-col)		  
		  (setq v (append v (list (if (scomx-goto-column col)   
					      (scomx-val-at-point)
                                            nil)))
			lend (or (scomx-goto-end-of-column col) (point))   
			col (1+ col)))
		(nconc mtx (list v))))
	  (forward-line 1)
	  (if (save-excursion (not (zerop (forward-line 1))))
	      (setq lend (1+ end))
	    (setq lend (1+ lend)))
	  (message "grabbing matrix %s... (%d)" sid (- end lend)) ;; à changer ??
	  (beginning-of-line))
	(message " ...done")
	(math-transpose mtx)))))

(defmacro with-csound-score-calc-language (&rest body)
  "Evaluate BODY with calc language set to 'csound-score,
then restore calc language to its normal value"
  `(unwind-protect
       (progn
	 (calc-set-language 'csound-score)
	 ,@body)
     (calc-set-language nil)))

(defun scomx-val-at-point ()        
  "Reads the score parameter at point and return a corresponding calc raw data suitable as a matrix element
Some filtering is performed by scomx-code-parameters in order to manage special (i.e. non numerical) parameters"
  (save-match-data
    (let ((ms nil))
      (if (equal (thing-at-point 'char) "[")
	  (forward-char 1))
      (if (equal (thing-at-point 'char) "]")
	  (backward-char 1))

    ;; fast track for simple numbers:
;    (if (thing-at-point-looking-at "[ \ti]\\([0-9.-]+\\)") ; 4.5e-7 fails !
;	(setq ms (math-read-number (match-string 1)))

      (progn
	(if (thing-at-point-looking-at "\\[\\([^][\n]*\\)\\]")
	    (setq ms (scomx-code-parameters (match-string 1))))

	(if (thing-at-point-looking-at ";.*$") ; comments 
;	(setq ms (math-read-expr (scomx-code-comments (match-string 0))) )          
	    (setq ms nil) ;; we don't allow them in matrices

	  (and (or ms
		   (and (thing-at-point-looking-at scomx-column-regexp)
			(setq ms (scomx-code-parameters (match-string 0)))))
	       (or (math-read-number ms)
		   (with-csound-score-calc-language
		    (math-read-expr ms)))))))));)


(defun scomx-code-parameters (str)
  "Code the score parameter STR so that it can safely be read by math-read-number or math-read-expr (returns a string)
p1 is stripped from its `i'
macros like $x. are converted into macrox calc variables 
@@ is replaced with !! in order to be understood by 'csound-score 'math-oper-table"
  (if (string-match "^i\\([0-9.]+\\)$" str)
    (setq str (replace-match "\\1" t nil str)))  
  (while (string-match "\\$\\([a-zA-Z0-9_]*\\)\\.*" str)
    (setq str (replace-match "macro\\1" t nil str)))
  (while (string-match "@@" str)
    (setq str (delete ? (replace-match "!!" t nil str))))
  str)

(defun scomx-encode-string-elt (str)
  "Format STR (a matrix value as a string) so that it can be written as a score parameter
This function should only return valid score expressions. It notably convert macrox variables back to [$x.] format"
  (if (string-match "'nil" str)
      (setq str "")
    (if (and (>= (length str) 4)
	     (string= (substring str 0 4) "[59,"))
	;; comments (not allowed in matrices at the moment, see scomx-val-at-point):
	(progn
	  (setq str (math-vector-to-string (math-read-expr str)))
	  (while (string-match "°" str)
	    (setq str (replace-match " " t nil str))))
      
      (unless (or (string-match "^[np]p[0-9]+$" str)      ; np.. et pp..
		  (string-match "^[.<>(){}+^~]*$" str))   ; carry symbols
          ;; [] expression (with or without macros)
	  (progn
	    (while (string-match "macro\\([a-zA-Z0-9]+\\)" str)
	      (setq str (delete ? (replace-match "$\\1." t nil str))))
	    (while (string-match "!!" str)
	      (setq str (delete ? (replace-match "@@" t nil str))))	
	    (setq str (concat "[" str "]"))))))
  str)

;; inutilisé tant que les commentaires ne peuvent être lus en matrice  
(defun scomx-code-comments (str)
  (while (string-match " " str)
    (setq str (replace-match "°" t nil str)))
  (concat "\"" str "\""))

(defun scomx-yank-matrix (matrix sid)
  "write MATRIX in place of the score selection SID
MATRIX must fit SID dimensions (although nil elements are allowed)"
  (save-excursion
    (let* ((beg-col (scomx-beg-column sid))
	   (end-col (scomx-end-column sid))
	   (tmp (if (> end-col beg-col) end-col beg-col))
	   (beg-col (if (> end-col beg-col) beg-col end-col))
	   (end-col tmp)
	   (beg (scomx-selection-beginning sid))
	   (end (scomx-selection-end sid))
	   (nl 1)
	   lenm)
      (setq matrix (math-transpose matrix)
	    lenm (length matrix))
      (goto-char beg)
      (scomx-beginning-of-i-statement)
      (while (< nl lenm)
	(if (scomx-i-statement-p)
	    (progn
	      (if (scomx-goto-column beg-col) 
		  (kill-region (point) 
			       (or (scomx-goto-end-of-column end-col) (point)))
		(scomx-goto-end-of-column end-col)
		(insert "\t"))
	      (insert
	       (mapconcat
		(lambda (elt)
		  (let (velt)
		    (if (numberp elt)
			(number-to-string elt)
		      (if (string-match "^[0-9-.e]+$"
					(with-csound-score-calc-language
					 (setq velt (calc-eval elt))))
			  velt
			(scomx-encode-string-elt velt)))))
		(cdr (nth nl matrix))
		"\t"))
	      (setq nl (1+ nl))
	      (message "yanking matrix %s... (%d)" sid (- lenm nl))))     ;; à changer ??
	(forward-line 1))
      (message " ...done")
      (scomx-maybe-display sid))))

(defun scomx-replace-ftgen-formulas ()
  "Replaces all formulas in ftgen arguments
with their values as computed by calc"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (when (search-forward "ftgen" nil t)
      (while (re-search-forward "\\([^,]+\\)" (point-at-eol) t)
	(replace-match
	 (concat " " (scomx-formula (match-beginning 1) (match-end 1)))
	 nil nil nil 1)))))

(defun scomx-replace-bracketed-formulas (&optional beg end)
  "Replaces all bracketed formulas between BEG and END
with their values as computed by calc"
  (interactive "r")
  (save-excursion
    (goto-char (or beg (point-min)))
    (while (re-search-forward "\\[.*?\\]" end t)
      (replace-match (scomx-formula (match-beginning 0) (match-end 0))))))

(defun scomx-formula (&optional beg end)
  "Gives the calc value for the formula between BEG and END"
  (interactive "r")
  (save-match-data
    (let ((formula (buffer-substring beg end)))
      (with-temp-buffer
	(if (= (elt formula 0) 91)
	    (insert formula)
	  (insert "[" (remove ? formula) "]"))
	(let ((result (with-csound-score-calc-language
		       (calc-eval (scomx-val-at-point)))))
	  (if (= (elt result 0) ?e) formula ; error
	    result))))))
  
;;=================================================================================
;;                                high-level macros
;;=================================================================================

(defmacro scomx-operate-m (sid &rest body)   
  "evaluates BODY within a defmath
`m' stands the selection SID (as a transposed matrix)"
  (declare (indent 1))
  `(progn
     (scomx-yank-matrix
      (let ((var-m (scomx-grab-matrix ,sid)))
	(with-raw-defmath ,@body)) 
      ,sid)
     (scomx-maybe-display ,sid)))

(defmacro scomx-operate-p-nc-nr (sid &rest body)   
  "evaluates BODY within a defmath
`p' stands in turn for each field in selection SID (as a transposed matrix)
`nc' and `nr' stand for its coordinates in the transposed matrix"
  (declare (indent 1))
  `(progn
     (scomx-yank-matrix
      (let* ((var-m (scomx-grab-matrix ,sid))
	     (var-mp (math-copy-matrix var-m)))
	(with-raw-defmath 
	  (for ((nc 1 (- (length m) 1)))
	       (for ((nr 1 (- (length (cadr m)) 1)))
		    (let ((p (elt m nc nr)))
		      (setf (elt mp nc nr)
			    (condition-case nil
				,@body
			      (error nil))))))
	 mp))
      ,sid)
     (scomx-maybe-display ,sid)))


;;=================================================================================
;;                                menu operations
;;=================================================================================


;; ------------------------------------------- operate on matrix 

(defun scomx-mop ()
  (interactive)
  (if (scomx-current-selection-p)
      (let (str)
	(if (and scomx-propose-algebraic-input-first
		 (not (string= "" (setq str (read-from-minibuffer "algebraic(m): ")))))
	    (scomx-alg-mop "" str)
	  (if (not (string= "" (setq str (read-from-minibuffer "(operation m): "))))
	      (eval `(scomx-operate-m "" ,(read str)))
	    (if (not scomx-propose-algebraic-input-first)
		(scomx-alg-mop "" (read-from-minibuffer "algebraic(m): "))))))))

(defun scomx-alg-mop (sid str)   
  "evaluates BODY within a defmath with algebraic format
`m' stands the selection SID (as a transposed matrix)"
  (eval `(scomx-yank-matrix
	  (let ((var-m (scomx-grab-matrix ,sid)))
	    (with-raw-defmath : ,str))
	  ,sid)))


;; ------------------------------------------- operate on parameters

(defun scomx-pop ()
  (interactive)
  (if (scomx-current-selection-p)
      (let (str)
	(if (and scomx-propose-algebraic-input-first
		 (not (string= "" 
			       (setq str (read-from-minibuffer "algebraic(p, nc, nr, m): ")))))
	    (scomx-alg-pop "" str)
	  (if (not (string= "" (setq str (read-from-minibuffer "(operation p nc nr m): "))))
	      (eval `(scomx-operate-p-nc-nr "" ,(read str)))
	    (if (not scomx-propose-algebraic-input-first)
		(scomx-alg-pop "" (read-from-minibuffer "algebraic(p, nc, nr, m): "))))))))

(defun scomx-alg-pop (sid str)   
  "evaluates BODY within a defmath with algebraic format
`p' stands in turn for each field in selection SID (as a transposed matrix)
`nc' and `nr' stand for its coordinates in the transposed matrix"
  (interactive)
  (eval `(scomx-yank-matrix
	  (let ((var-m (scomx-grab-matrix ,sid))
		var-p)
	    (with-raw-defmath
	      (for ((nc 1 (- (length m) 1)))
		   (for ((nr 1 (- (length (cadr m)) 1)))
			(setq p (elt m nc nr))
			(let ((bof (condition-case nil
				       : ,str
				       (error nil))))
			  (setf (elt m nc nr) bof ))))
	      m ))
	  ,sid)))


;; ------------------------------------------- sort

(defun scomx-sort-along (&optional column sense)
  "Sort current selection according to column"
  (interactive)
  (if (scomx-current-selection-p)
      (progn
	(if (save-excursion (equal (scomx-beg-column "") (scomx-end-column "")))
	    (setq column 1))
	(eval
	 `(scomx-operate-m 
	   ""
	   (transpose
	    (scomx-sort-matrix 
	     (transpose m)
	     ,(or column (string-to-number (read-from-minibuffer "Sort along column: ")))
	     ,(or sense (string-to-number (read-from-minibuffer "Order (-1 or 1): "))))))))))


(defun scomx-sort-matrix (m col &optional sense)
  (sort m (lambda (e1 e2)
	    (if (or (null e1) (equal e1 'vec)) t
	      (if (or (null e2) (equal e2 'vec)) nil
		(if (or (null (nth col e1)) (null (nth col e2))) t
		  (let ((m-comp (math-compare (nth col e1) (nth col e2))))
		    (if (and sense (< sense 0))
			(> m-comp 0)
		      (< m-comp 0)))))))))


;; ------------------------------------------- safely remove comments

(defun scomx-nocomment-region (beg end)
  "removes all comments in region BEG END except the ones used as tags for defining matrices"
  (interactive "r")
  (save-excursion
    (let (p)
      (goto-char end)
      (beginning-of-line)
      (while (>= (point) beg)
	(beginning-of-line)
	(if (search-forward ";" (save-excursion (re-search-forward "$")) t) 
	    (kill-region (goto-char (setq p (- (point) 1)))
 			 (if (scomx-search-next-tag (save-excursion (re-search-forward "$")))
 			     (search-backward ";")
 			   (progn
 			     (goto-char p)
 			     (re-search-forward "$")))))
	(end-of-line)
	(if (search-backward ";" (save-excursion (re-search-backward "^")) t) 
	    (kill-region (progn
			   (scomx-search-next-tag (save-excursion (re-search-forward "$")))
			   (point))
			 (re-search-forward "$")))
	(forward-line -1)))))


;; ------------------------------------------- multi-column processing

(defun scomx-operate-columns (sid &rest args) 
  ""
  (interactive)
  (scomx-yank-matrix
   (let ((bigm (scomx-grab-matrix sid))
	 (n 1))
     (while (<= n (- (length bigm) 1))
       (let* ((def (nth (- n 1) args))
	      (var-m (list 'vec (elt bigm n))))
	 (setf (elt bigm n) 
	       (elt 
		(or (condition-case ()
			(eval `(with-raw-defmath 
				 ,(read 
				   (read-from-minibuffer
				    (concat "(operation m) col " (number-to-string n) " > ")
				    def)))) 
		      (error nil))
		    var-m)
		1)))
       (setq n (1+ n)))
     bigm) sid)
  (scomx-maybe-display sid))



;;=================================================================================
;;                                Cmask integration
;;=================================================================================

(defcustom scomx-Cmask-support nil
  "weither Cmask is available from csound-mx"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-Cmask-binary "cmask"
  "Cmask binary"
  :type 'string
  :group 'csound-mx)

(defun scomx-Cmask-binary ()
  (substitute-in-file-name scomx-Cmask-binary))

(defcustom scomx-Cmask-tmp-file "c:/csound/cmask/scomx-tmp"
  "Cmask temporary file (WITHOUT extension !)"
  :type 'string
  :group 'csound-mx)

(defun scomx-Cmask-tmp-file ()
  (substitute-in-file-name scomx-Cmask-tmp-file))

(defvar scomx-cmask-memory ()
  "Keep in memory the last arguments passed to cmask")

(defmath cmask (m l1 &optional l2 l3 l4 l5)
  ;;; mémoire:
  (setq scomx-cmask-memory (list l1 l2 l3 l4 l5))

  ;;; extension de la syntaxe:
  ;;; si l1 est "column", il est remplacé par une liste des valeurs courantes
  (when (string= l1 "column")
    (setq l1 (m-concat "item cycle (" (scomx-list-to-string (cdar (last m))) ")")))
  ;;; si l1 est "col01", il est remplacé par une liste des valeurs courantes normalisées
  (when (string= l1 "col01")
    (let ((v0 (min-value (cdar (last m))))
	  (v1 (max-value (cdar (last m)))))
      (setq l1 (m-concat "item cycle (" (scomx-list-to-string 
					 (cdr (/ (- (car (last m)) v0 ) (- v1 v0)))) ")"))))

  ;;; 2 cas: 
  (if (> (length m) 2)
      ;;; 1) matrice n colonnes
      ;;; -> on travaille sur la dernière
      ;;; -> la première fait office de p2
      (progn
	(let ((p2s (cadr m)) d (p2d ()) (dmin ()))
	  (for (i 2 (- (length p2s) 1))
	       (setq d (- (elt p2s i) (elt p2s (- i 1))))
	       (if (or (null dmin) (< d dmin))
		   (setq dmin d))
	       (setq p2d (real-append p2d (list d))))
	  (if (< dmin 0)
	      (error "The first column needs to be ordered first !")
	    (progn
	      (with-temp-file scomx-Cmask-tmp-file
		(insert "f " (calc-eval
			      (min-value p2s))
			" " (calc-eval
			     (+ (max-value p2s) (/ dmin 1000))) "\n"    ;;; robuste ???
;			     (max-value p2s)) "\n"
			     "p1 const 1" "\n"
			     "p2 item cycle (" (scomx-list-to-string p2d) ")\n")
		(insert "p3 " l1 "\n"
			(or l2 "") "\n"
			(or l3 "") "\n"
			(or l4 "") "\n"
			(or l5 "") "\n"))
	      (concat (mat-less-row m (- (length m) 1))
		      (scomx-Cmask-matrix-3))))))
    ;;; 2) matrice 1 colonne 
    ;;; -> p2 va toujours de 0 à 1, par paliers aussi petits que nécessaire
    (progn
      (with-temp-file scomx-Cmask-tmp-file
	(insert "f 0 "   "1.001 \n"                      ;;;; robuste ???????
		"p1 const 1" "\n"
		"p2 const " (calc-eval (/ 1 (- (length (cadr m)) 2))) "\n")
	(insert "p3 " l1 "\n"
		(or l2 "") "\n"
		(or l3 "") "\n"
		(or l4 "") "\n"
		(or l5 "") "\n"))
      (scomx-Cmask-matrix-3))))

;; example for use in a single column matrix, with "Operate on matrix":
;(cmask m "rnd uni" "mask 10 20" "prec 2")


(defun real-append (l1 l2)      ;; vraiment très laid !!
  (append l1 l2))

(defun scomx-list-to-string (l)  ;; vraiment très très laid !!
  (mapconcat 'calc-eval l " "))

(defun scomx-Cmask-matrix-3 ()
  (save-window-excursion
    (shell-command (concat (scomx-Cmask-binary) " " (scomx-Cmask-tmp-file)) "*Messages*"))
  (with-temp-buffer
    (insert-file-contents (concat (scomx-Cmask-tmp-file) ".sco"))
    (re-search-forward "^i")
    (scomx-goto-column 3)
    (scomx-select-column "")
    (scomx-grab-matrix "")))



;;=================================================================================
;;                        extensible code repository
;;=================================================================================

(defcustom scomx-Matrix-menu-definitions '(scomx-Matrix-menu)
  "List of functions to be evaluated in order to build up the 'Matrix' menu
These functions must return the same kind of list as 'scomx-Matrix-menu' does,
so use its code as a template: C-h f scomx-Matrix-menu will bring you there."
  :type '(repeat symbol)
  :group 'csound-mx)

(defun scomx-change-Matrix-menu ()
  (interactive)
  (dolist (mx-contrib scomx-Matrix-menu-definitions)
    (dolist (submenu (apply mx-contrib ()))
      (easy-menu-change '("Matrix") (car submenu) (cdr submenu)))))

(defun read-calc-expr-from-minibuffer (&rest args)
  (math-read-expr (apply 'read-from-minibuffer args)))

(defun scomx-Matrix-menu ()
  "Provide a code repository making it easier to handle complex operations.
This is read by scomx-change-Matrix-menu in order to build the 'Matrix' menu. You may add your own code by writing similar functions and adding their names to scomx-Matrix-menu-definitions"
  (list 

   ;; basic operations
   ;;------------------
    '("Simple"
      ["set precision"
       (scomx-operate-m ""
	 (let ((prec 
		(read-calc-expr-from-minibuffer "float precision: " "3")))
	   (/ (floor (* (^ 10 prec) m)) (^ 10 prec))))
       ]
      ["normalize"
       (scomx-operate-m ""
	 (let 
	     ((r0 (read-calc-expr-from-minibuffer "from: " "0"))
	      (r1 (read-calc-expr-from-minibuffer "to: " "1"))
	      (v0 (min-value m))
	      (v1 (max-value m)))
	   (+ (* (- m v0) (/ (- r1 r0) (- v1 v0))) r0)))
       ]
      ["quantize"
       (let ((var-dv (read-calc-expr-from-minibuffer "grid width: " "0.1"))
	     (var-off (read-calc-expr-from-minibuffer "offset: " "0")))
	  (scomx-operate-p-nc-nr ""
				 (+ (* (round (/ (- p off) dv)) dv) off)))
       ]
      ["swap first and last columns" 
       (scomx-operate-m "" 
	 (swap-rows m 1 (- (length m) 1)))
       ]
      ["copy first to last column" 
       (scomx-operate-p-nc-nr ""
	 (if (equal nc (- (length m) 1))
	     (elt m 1 nr)
	   p))
       ]
      ["cycle downwards" 
       (scomx-operate-m ""
	 (let ((m2 (copy-matrix m))
	       (w (- (length (cadr m)) 1))
	       (ns (read-calc-expr-from-minibuffer "step: " "1")))
	   (for ((nc 1 (- (length m) 1)))
		(for ((nr 1 w))
		     (setf (elt m nc nr)
			   (elt m2 nc (1+ (mod (- nr (1+ ns)) w))))))
	   m))
       ])

    ;; one-column
    ;;-----------
    '("One column"
      ["ramp from top to bottom"
       ;; ugly hack: the let should be out of the loop !
        (scomx-operate-p-nc-nr ""
	  (let ((step (div (- (elt m 1 (1- (length (cadr m))))
			      (elt m 1 1))
			   (- (length (cadr m)) 2))))
	    (+ (elt m 1 1) (* step (- nr 1)))))
	])

    ;; filling
    ;;--------
    '("Fill"
      ["linear: step*(nr index)+offset"
       (let ((var-step (read-calc-expr-from-minibuffer "step: " "1"))
	     (var-off (read-calc-expr-from-minibuffer "offset: " "0")))
	 (scomx-operate-p-nc-nr "" 
	   (+ off (* step nr))))
       ]
      ["geometric: (nr index)^expo+offset"
       (let ((var-expo (read-calc-expr-from-minibuffer "expo: " "2"))
	     (var-off (read-calc-expr-from-minibuffer "offset: " "0")))
	 (scomx-operate-p-nc-nr ""
	   (+ off (^ nr expo))))
       ])

    ;; Cmask operations
    ;;-----------------
    (when scomx-Cmask-support
	 '("Cmask"
	   ["(repeat last)"
	    (if scomx-cmask-memory
		(eval `(scomx-operate-m "" (cmask m ,@scomx-cmask-memory))))
	    ] 
	   ["segment function"
	    (scomx-operate-m ""
	      (let ((seg (read-from-minibuffer "segment> " "(0 0 1 1)"))
		    (quant (read-from-minibuffer "QUANTIZER> " "quant 1 0"))
		    (prec (read-from-minibuffer "PRECISION> " "prec 3")))
		(cmask m (concat "seg " seg) quant prec)))
	    ]
	   ["items from a list"
	    (scomx-operate-m ""
	      (let ((mode (read-from-minibuffer
			   "mode (cycle, swing, heap, random)> " "cycle"))
		    (li (read-from-minibuffer "list> " "(1 2 3 4)")))
		(cmask m (m-concat "item " mode " " li))))
	    ]
	   ["periodic function"
	    (scomx-operate-m ""
	      (let* ((func (read-from-minibuffer 
			    "function (sin, cos, sawup, sawdown, square, triangle, powup, powdown)> " "sin"))
		     (periods (read-from-minibuffer "frequency> " "1"))
		     (phase (read-from-minibuffer "phase> " "0"))
		     expo mask quant prec)
		(if (or (string= func "powup") (string= func "powdown"))
		    (setq expo (read-from-minibuffer "exponent> " "1")))
		(setq mask (read-from-minibuffer "MASK> " "mask 0 1")
		      quant (read-from-minibuffer "QUANTIZER> " "quant 1 0")
		      prec (read-from-minibuffer "PRECISION> " "prec 3"))
		(cmask m
		       (m-concat "osc " func " " periods " " phase expo)
		       mask quant prec)))
	    ]
	   ["uniform random values"
	    (scomx-operate-m ""
	      (let ((mask (read-from-minibuffer "MASK> " "mask 0 1"))
		    (quant (read-from-minibuffer "QUANTIZER> " "quant 1 0"))
		    (prec (read-from-minibuffer "PRECISION> " "prec 3")))
		(cmask m "rnd uni" mask quant prec)))
	    ]
	   ["random distribution"
	    (scomx-operate-m ""
	      (let* ((func (read-from-minibuffer 
			    "distribution (lin, tri, exp, rexp, bexp, gauss, cauchy, beta, wei)> " "gauss"))
		     (val1 "") (val2 "") mask prec)
		(cond
		 ((string= func "tri")
		  (setq val1 (read-from-minibuffer "slope> " "1")))
		 ((or (string= func "exp") (string= func "bexp") (string= func "rexp"))
		  (setq val1 (read-from-minibuffer "exponent> " "1")))
		 ((string= func "gauss")
		  (setq val1 (read-from-minibuffer "standard deviation> " "0.1")
			val2 (read-from-minibuffer "mean> " "0.5")))
		 ((string= func "cauchy")
		  (setq val1 (read-from-minibuffer "spread> " "0.1")
			val2 (read-from-minibuffer "mean> " "0.5")))
		 ((string= func "beta")
		  (setq val1 (read-from-minibuffer "a> " "0.1")
			val2 (read-from-minibuffer "b> " "0.1")))
		 ((string= func "wei")
		  (setq val1 (read-from-minibuffer "s> " "0.1")
			val2 (read-from-minibuffer "t> " "0.1"))))
		(setq mask (read-from-minibuffer "MASK> " "mask 0 1")
		      quant (read-from-minibuffer "QUANTIZER> " "quant 1 0")
		      prec (read-from-minibuffer "PRECISION> " "prec 3"))
		(cmask m
		       (m-concat "rnd " func " " val1 " " val2)
		       mask quant prec)))
	    ]
	   ["apply accumulator"
	    (scomx-operate-m "" (cmask m "column" 
				       (read-from-minibuffer "ACCUMULATOR> " "accum on")))
	    ]))

    ;; specific to p2-p3 two-column matrix
    ;;------------------------------------
    '("p2-p3"
      ["make legato"
       (scomx-operate-p-nc-nr "" 
	 (if (and (equal nc 2)
		  (elt m 1 (1+ nr)))
	     (- (elt m 1 (1+ nr)) (elt m 1 nr))
	   p ))
       ])))


;;=================================================================================
;; this is it

(provide 'csound-mx)

;; csound-mx.el ends here