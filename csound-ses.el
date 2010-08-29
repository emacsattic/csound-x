;;; -*- auto-recompile: t -*-

;;; csound-ses.el --- interfacing scores and SES spreadsheets

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-ses.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-ses.el is distributed in the hope that it will be useful,
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
;;    http://www.zogotounga.net/comp/csoundx.html

;;
;; ==========================================================
;; this file should be installed through the csound-x package
;; (it is actually a part of csound-mx)
;; ==========================================================
;;


;; last modified January 6, 2004

;;;=====================================================================
;;; Code:

(defcustom scomx-ses-translate-symbols nil            
  "If non-nil, the spreadsheets attempt to translate score symbols such as + and . into formulas
if nil, everything in the matrix is imported as is in the spreadsheet"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-framed-spreadsheets nil            
  "If non-nil, the SES buffers appear in a specific frame"
  :type 'boolean
  :group 'csound-mx)

(defcustom scomx-spreadsheets-borders 3            
  "Numbers of extra rows and columns to be added at the right and bottom of a SES spreadsheet"
  :type 'integer
  :group 'csound-mx)

(defcustom cscsd-ses-areas-invisible t           
  "If non-nil, the <SES>...</SES> areas contents are invisible
use the function cscsd-show-ses-area to display a given area
use the function cscsd-hide-ses-areas to hide them all again"
  :type 'boolean
  :group 'csound-csd)


;;;================== calc support

; ex: (calc-cell (/ A1 B1))
;     (calc-cell :"A1/B1")

(defmacro calc-cell (&rest body)
  "A specific form of `with-defmath' binding all cell symbols so that there are allowed in formulas"
  (if (not (boundp 'var-A1))
      (let ((clist '(col row)))
	(dotimes (c numcols)
	  (dotimes (r numrows)
	    (setq clist (cons (ses-cell-symbol r c) clist))))
	`(with-defmath-import ,clist ,@body)) 
;    (message "yep")                             ;;; et pourquoi on le voit pas, lui ??
    `(with-defmath ,@body)))

(put 'calc-cell 'safe-function t)

; deux raccourcis:

(defcustom scomx-ses-calc-shortcut 'm:            
  "shortcut for `calc-cell'"
  :type 'symbol
  :group 'csound-mx)

(fset scomx-ses-calc-shortcut 'calc-cell)
(put scomx-ses-calc-shortcut 'safe-function t)

(defcustom scomx-ses-calc-alg-shortcut 's:            
  "shortcut for `calc-alg-cell'"
  :type 'symbol
  :group 'csound-mx)

(defmacro calc-alg-cell (str) 
  "Evaluate STR as a calc algebraic formula, using local bindings for the cells name"
  `(calc-cell : ,str))

(fset scomx-ses-calc-alg-shortcut 'calc-alg-cell)
(put scomx-ses-calc-alg-shortcut 'safe-function t)


;;;================== matrix to spreadsheet

(defun scomx-ses-name (sid &optional bname)
  (if scomx-framed-spreadsheets
      (add-to-list 'special-display-regexps "\\[ matrix .* in .* \\]"))
  (setq bname (or bname (buffer-name (buffer-base-buffer (current-buffer)))))
  (concat "[ matrix \"" sid "\" in " bname " ]"))

(defun scomx-make-ses-buffer (sid rows columns)
  (let ((sname (scomx-ses-name sid))
	ses-buffer)
    (when (or (null (get-buffer sname))
	      (cscsd-usually-ask-p "Erase the existing spreadsheet ? "))
      (if (get-buffer sname)
	  (kill-buffer sname))
      (setq ses-buffer (get-buffer-create sname))
      (save-excursion
	(set-buffer ses-buffer)
	(ses-mode)
	(if (> (+ rows scomx-spreadsheets-borders) 1)
	    (ses-insert-row (+ scomx-spreadsheets-borders (- rows 1))))
	(if (> (+ columns scomx-spreadsheets-borders) 1)
	    (ses-insert-column (+ scomx-spreadsheets-borders (- columns 1))))
	(if (> columns 10)
	    (dotimes (col numcols)
	      (ses-set-column-width col 3))))
      ses-buffer)))

(defun scomx-count-i-statements (beg end)
  (save-excursion
    (let ((n 0)
	  (line ""))
      (goto-char beg)
      (while (and (<= (point) end)
		  (null (string= line (what-line))))
	(if (scomx-i-statement-p)
	    (incf n))
	(setq line (what-line))
	(forward-line 1))
      n)))

(defun scomx-edit-in-ses (&optional sid) 
  "Export the SID matrix as a SES buffer"
  (interactive)
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (save-excursion
    (let* ((deb-col (scomx-beg-column sid))
	   (end-col (scomx-end-column sid))
	   (tmp (if (> end-col deb-col) end-col deb-col))
	   (deb-col (if (> end-col deb-col) deb-col end-col))
	   (end-col tmp)
	   (end (scomx-selection-end sid))
	   (crow 0)
	   ses-buffer)
      (goto-char (scomx-selection-beginning sid))
      (setq ses-buffer (scomx-make-ses-buffer sid
					      (scomx-count-i-statements (point) end)
					      (1+ (- end-col deb-col))))    
      (scomx-beginning-of-i-statement)
      (let ((lend 0))
	(while (<= lend end)
	  (if (scomx-i-statement-p)
	      (let ((ccol deb-col))
		(while (<= ccol end-col)
		  (let* ((ses-col (- ccol deb-col))
			 (val (if (scomx-goto-column ccol)
				  (progn (or (thing-at-point-looking-at scomx-column-regexp) 
					     (thing-at-point-looking-at "\\[[^][\n]*\\]"))
					 (match-string-no-properties 0))
				nil)))
		    (save-excursion
		      (set-buffer ses-buffer)
		      (when (> (length (prin1-to-string (setq val (scomx-ses-read val crow ses-col ccol))))
			     (ses-col-width ses-col))
			  (ses-set-column-width ses-col (length (prin1-to-string val))))
		      (ses-set-cell crow ses-col 'formula val))
		    (setq lend (or (scomx-goto-end-of-column ccol) (point)))
		    (incf ccol))))
	    (decf crow))
	  (forward-line 1)
	  (if (save-excursion (not (zerop (forward-line 1))))
	      (setq lend (1+ end))
	    (setq lend (1+ lend)))
	  (incf crow)
	  (beginning-of-line)))
      (switch-to-buffer-other-window ses-buffer)
      (ses-recalculate-all))))


(defun scomx-ses-read (val-string ses-row ses-col score-col)
  ""
  (cond ((null (stringp val-string))
	 val-string)
	;;
	((and (string-match "^i\\([0-9.]*\\)" val-string)
	      (equal score-col 1))
	 (string-to-number (match-string-no-properties 1 val-string)))
	;;
	((string= "." val-string)
	 (if scomx-ses-translate-symbols
	     (ses-relocate-formula (ses-cell-formula (1- ses-row) ses-col)
				   0 0 1 0)
	   "."))
	;;
	((string= "+" val-string)
	 (if scomx-ses-translate-symbols
	     `(+ ,(ses-cell-symbol (1- ses-row) ses-col) ,(ses-cell-symbol (1- ses-row) (1+ ses-col)))
	   "+"))
	;;
	((string-match "^[0-9.-]+" val-string)
	 (string-to-number val-string))
	;;
	((string= "" val-string)
	 nil)
	;;
	(t
	 val-string)))


(defun scomx-restore-ses-area (&optional sid)
  "Make a SES buffer from the <SES> area associated to SID"
  (interactive)
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (let ((csd (current-buffer))
	ses beg end)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "<SES>" sid "$"))
	  (setq beg (progn (forward-line 1) (point)))
	  (search-forward "</SES>")
      	  (setq end (progn (forward-line -1) (point)))))
    (if (null (and beg end))
	(error (concat "selection " sid " is not correctly defined in this CSD !"))
      (when (or (null (get-buffer (scomx-ses-name sid)))
		(cscsd-usually-ask-p "Erase the existing spreadsheet ? "))
	(setq ses (set-buffer (get-buffer-create (scomx-ses-name sid))))
	(erase-buffer)
	(insert-buffer-substring csd beg end)
	(goto-char (point-min))
	(ses-mode)
	(switch-to-buffer-other-window ses)))))


;;;================== spreadsheet to matrix

(defun scomx-associated-ses-buffer (&optional sid)
  "Test weither a SES buffer is associated to selection SID
return nil or the buffer"
  (setq sid (or sid (read-from-minibuffer "id: ")))
  (get-buffer (scomx-ses-name sid)))

(defun scomx-import-from-ses (&optional sid)
  (interactive)
  (save-excursion
    (let ((ses (scomx-associated-ses-buffer (or sid (read-from-minibuffer "id: ")))))
      (if (null ses)
	  (error "couldn't find the corresponding spreadsheet !")
	(set-buffer ses)
	(scomx-stuff-back-ses-matrix)))))

(defun scomx-stuff-back-ses-matrix ()
  "Use the content of the current SES buffer to update its corresponding score matrix"
  (let* ((bits (split-string (buffer-name) " " t))
	 (sid (car (split-string (nth 2 bits) "\"" t)))
	 (sco (nth 4 bits))
	 (ses (current-buffer)))
    (if (get-buffer sco)
	(save-excursion
	  (goto-char (point-min))
	  (set-buffer (or (buffer-base-buffer (get-buffer sco)) sco))
	  (if (null (scomx-selection-defined-p sid))
	      (error (concat "selection " sid " is not defined in " sco " !"))
	    (let* ((beg-col (scomx-beg-column sid))
		   (end-col (scomx-end-column sid))
		   (tmp (if (> end-col beg-col) end-col beg-col))
		   (beg-col (if (> end-col beg-col) beg-col end-col))
		   (end-col tmp)
		   (beg (scomx-selection-beginning sid))
		   (end (scomx-selection-end sid))
		   (nl 1)
		   (crow 0)
		   sisp)
	      (goto-char beg)
	      (scomx-beginning-of-i-statement)
	      (set-buffer ses)
	      (goto-char (point-min))
	      (while (not (looking-at "^[ \t]*$"))
		(set-buffer sco)
		(if (setq sisp (scomx-i-statement-p))
		    (progn
		      (if (scomx-goto-column beg-col) 
			  (kill-region (point) 
				       (or (scomx-goto-end-of-column end-col) (point)))
			(scomx-goto-end-of-column end-col)
			(insert "\t"))
		      (dotimes (ccol (1+ (- end-col beg-col)))
			(let ((val (save-excursion (set-buffer ses)
						   (ses-cell-value crow ccol))))
			  (insert (if (stringp val)
				      val
				    (prin1-to-string val)) 
				  "\t")))
		      (kill-backward-chars 1)))
		(forward-line 1)
		(set-buffer ses)
		(when sisp
		  (incf crow)
		  (forward-line 1)))
	      (set-buffer sco)
	      (scomx-maybe-display sid))))
      (error (concat "buffer " sco " doesn't seem to exist anymore, sorry !")))))

(defun scomx-import-ses-area (&optional sid)
  (interactive)
  (save-excursion
    (let ((ses (scomx-associated-ses-buffer (or sid (read-from-minibuffer "id: ")))))
      (if (null ses)
	  (error "couldn't find the corresponding spreadsheet !")
	(set-buffer ses)
	(scomx-store-ses-area)))))

(defun scomx-store-ses-area ()
  "Store the current SES buffer as an XML area in the source buffer"
  (save-restriction
    (widen)
    (let* ((bits (split-string (buffer-name) " " t))
	   (sid (car (split-string (nth 2 bits) "\"" t)))
	   (csd (nth 4 bits))
	   (ses (current-buffer))
	   (end (point-max))
	   sbeg tmp)
      (set-buffer (or (buffer-base-buffer (get-buffer csd)) csd))      
      (when (or (cscsd-buffer-is-a-csd-p)
		(y-or-n-p "The source buffer does not seem to be a valid CSD. Continue ? "))
      (save-excursion
	(goto-char (point-min))
	(if (and (re-search-forward (concat "<SES>" sid "$") nil t)
		 (cscsd-usually-ask-p "erase the existing <SES> area ? "))
	      (kill-region (point-at-bol) (search-forward "</SES>"))
	  (message (concat "you will have two areas for " sid "..."))
	  (cscsd-go-after-cssynth)
	  (insert "\n\n"))
	(insert "<SES>" sid "\n")
	(setq sbeg (point))
	(insert-buffer-substring ses 1 end) 
	;;; disabling the Local Stuff:
	(save-excursion
	  (goto-char sbeg)
	  (if (search-forward ";;; Local" nil t)
	      (insert " <ignore>")))
	;;;
	(setq tmp inhibit-read-only
	      inhibit-read-only t)
	(set-text-properties sbeg (point) nil)    
	(setq inhibit-read-only tmp)
	(insert "</SES>\n")
	(if cscsd-ses-areas-invisible
	    (cscsd-hide-ses-areas)))))))


;;;================== utilities


(defun cscsd-hide-ses-areas ()
  "Hide the contents of all <SES> areas"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<SES>\\([a-z]+\\).*[\n\r]" nil t)
      (overlay-put (set (intern (concat "ses-ov-" (match-string-no-properties 1)))
			(make-overlay (point) (progn (re-search-forward "</SES>")
						      (backward-char 6)
						      (point))))
		   'invisible 'csd))))

(defun cscsd-show-ses-area (&optional name)
  "Display the contents of the <SES> area NAME
<SES> areas are invisible if `cscsd-ses-areas-invisible' is non-nil"
  (interactive "s<SES>name ? ")
  (let ((ov (eval (intern (concat "ses-ov-" name)))))
    (if (overlayp ov)
	(delete-overlay ov))))


(defun scomx-associated-SES-buffers ()
  "Return a list of SES buffers associated to the current sco or csd buffer"
  (let* ((name-bits (split-string (buffer-name) "\\." t))
	 (breg (concat "matrix.*in " (car name-bits) "\\." (cadr name-bits))))
    (delete-if-not (lambda (buf)
		     (string-match breg (buffer-name buf))) 
		   (buffer-list))))


(defun cscsd-SES-areas ()
  "Return a list of the SES areas in the current csd buffer"
  (let (lses)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<SES>[ \t]*\\([^ \t]+\\)[ \t]*$" nil t)
	(add-to-list 'lses (match-string-no-properties 1))))
    lses))


;;=================================================================================
;; this is it

(provide 'csound-ses)

;; csound-ses.el ends here