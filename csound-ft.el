;;; -*- auto-recompile: t -*-

;;; csound-ft.el --- handling f-tables

;; This file is not part of GNU Emacs.
;; 
;; csound-ft.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-ft.el is distributed in the hope that it will be useful,
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
;;    http://www.zogotounga.net/comp/csoundx_doc_csd.html

;;
;;; Installation:
;;                  
;; ==========================================================
;; this file should be installed through the csound-x package
;; ==========================================================
;;

;; last modified March 15, 2010

;;;=====================================================================
;;; Code:

(require 'widget)
(require 'graphs)
(require 'cl)

;;;===================================================================
;;;                    f-table display & edition
;;;===================================================================

(defgroup csound-ft nil
  "Managing f-tables in scores and orchestras"
  :group 'csound-x
  :prefix "csft-")


;;======================================================================
;; display
;;======================================================================

(defcustom csft-background "honeydew"
  "background color for ftable plots"
  :type 'string
  :group 'csound-ft)

(defcustom csft-foreground "blue"
  "foreground color for ftable plots"
  :type 'string
  :group 'csound-ft)

(defcustom csft-height 200
  "height in pixels for the plotting area"
  :type 'integer
  :group 'csound-ft)

(defcustom csft-width 500
  "width in pixels for the plotting area"
  :type 'integer
  :group 'csound-ft)

(defcustom csft-enable-csound-display nil
  "if nil, csound is invoked with the -d command line flag"
  :type 'boolean
  :group 'csound-ft)

(defvar csft-xratio)
(defvar csft-mouse-click-action)
(defvar csft-ftable-statement)
(defvar csft-original-ftable-statement)
(defvar csft-current-ftable-statement)
(defvar csft-plotting-area)
(defvar csft-source-buffer)
(defvar csft-tmp-buffer "*csound-ft-tmp*")

(defun csft-display-ftable (ft-num)
  "Display f-table numbered FT-NUM"
  (interactive (list (string-to-number 
		      (read-from-minibuffer
		       "Table number: "
		       (when (save-excursion 
			       (end-of-line)
			       (re-search-backward "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*\\([0-9]+\\)" nil t))
			 (match-string 3))))))
  (save-window-excursion
    (save-excursion
      (goto-char (point-min))
      (let* ((ft-buffer (buffer-name))
	     (ft-exists (re-search-forward 
			 (concat "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*0*" 
				 (number-to-string ft-num) "[ \t,]+") nil t))
	     ft-line)
	(if (not ft-exists)
	    (message "Sorry, could not find f-table %s" ft-num)	 
	  (beginning-of-line)
	  (while (re-search-forward "\\\\[ \t]*$" (point-at-eol) t)
	    (replace-match "")
	    (delete-char 1)
	    (when (thing-at-point-looking-at "[ \t]+")
	      (replace-match " ")))
	  (setq ft-line (thing-at-point 'line))
	  (csft-set-up-buffer (format "f%s in %s" ft-num ft-buffer))    
	  (setq csft-mouse-click-action 'csft-display-position
		csft-original-ftable-statement ft-line
		csft-current-ftable-statement ft-line
		csft-source-buffer ft-buffer)
	  (csft-refresh-display ft-line))))))

(defun csft-refresh-display (ft-line)
  (let ((inhibit-read-only t)
	(ft-line (csft-interpret-ft-line ft-line)))
    (erase-buffer)
    (setq csft-ftable-statement ft-line
	  csft-xratio (/ (string-to-number (csft-ftable-size ft-line))
			 (float csft-width)))
    (condition-case nil
	(progn
	  (setq csft-plotting-area (csft-plot-ftable ft-line csft-width))
	  (insert-plotting-area csft-plotting-area csft-foreground csft-background 'csft-do-the-click)
	  (insert "\n")
	  (insert ft-line "\n")    
	  (csft-insert-radio-buttons (abs (string-to-number (csft-ftable-gen ft-line))))
	  (csft-insert-push-buttons)
	  (goto-char (point-min)))
      (error (progn
	       (insert "unable to compute the table display, sorry.
see function `csft-plot-ftable' and buffer \"" csft-tmp-buffer "\"
which is the ouput of the CSD in \"" (cscsd-temp-csd-file) "\"")
	       (switch-to-buffer-other-window csft-tmp-buffer)
	       (find-file-other-frame (cscsd-temp-csd-file)))))))

(defun csft-set-up-buffer (title)
  (set-buffer (generate-new-buffer "*FTABLE*"))
  (modify-frame-parameters
   (select-frame 
    (make-frame `((title . ,(or title "plot"))
		  (cursor-type . nil)
;		  (unsplittable . t)
		  (left-fringe . 0)
		  (right-fringe . 0)
		  (tool-bar-lines . nil)
		  (menu-bar-lines . 0))))
   `((width . ,(1+ (/ csft-width (frame-char-width))))
     (height . ,(+ 10 (/ csft-height (frame-char-height))))))
  (make-local-variable 'csft-xratio)
  (make-local-variable 'csft-mouse-click-action)
  (make-local-variable 'csft-ftable-statement)
  (make-local-variable 'csft-original-ftable-statement)
  (make-local-variable 'csft-current-ftable-statement)
  (make-local-variable 'csft-plotting-area)
  (make-local-variable 'csft-source-buffer)
  (current-buffer))

(defun csft-interpret-ft-line  (ft-line)
  "Handles [..] syntax, do not handle macros yet"
  (if (or (find 91 ft-line) ; 91 is ascii code for [
	  (string-match "ftgen" ft-line))
      (with-temp-buffer
	(require 'csound-mx)
	(save-excursion
	  (insert ft-line))
	(scomx-replace-ftgen-formulas)
	(scomx-replace-bracketed-formulas)
	(buffer-string))
    ft-line))

;TEST (csft-interpret-ft-line "f 102 0 [2^11] 21 3 1") => "f 102 0 2048 21 3 1"
;TEST (csft-interpret-ft-line "f 102 0 [2 ^ 11] 21 3 1") => "f 102 0 2048 21 3 1"
;TEST (csft-interpret-ft-line "f 102 0 [2 ^ 11] 21 3 1\n") => "f 102 0 2048 21 3 1\n"
;TEST (csft-interpret-ft-line "gitemp ftgen 1, 0, 2 ^ 16, 10, 1") => "gitemp ftgen 1, 0, 65536, 10, 1"

(defun csft-plot-ftable (ft-line width)
  "Plot the table defined by statement FT-LINE"
  (let* ((fv (csft-get-ftable-vector ft-line width))
	 (parea (make-plotting-area 
		 width csft-height 
		 0 (* 1.01 width)
		 (min -0.01 (* 1.01 (apply 'min (append fv nil))))
		 (* 1.01 (apply 'max (append fv nil))))))
      (plot-vector fv parea t)
      parea))

(defun csft-get-ftable-vector (ft-line len)
  "Compute the table defined by statement FT-LINE"
  (let ((in-score (not (string-match "ftgen" ft-line)))
	(ft-size (csft-ftable-size ft-line)))
    (string-match "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*[0-9]+[ \t,]+\\([0-9.]+\\)" ft-line)
    (setq ft-line (replace-match "0" nil nil ft-line 3))
    (with-temp-file (cscsd-temp-csd-file)
      (insert "<CsoundSynthesizer>\n<CsOptions>\n--output=null\n</CsOptions>\n<CsInstruments>\n"
	      "sr = 1\nkr = 1\n"
	      "ksmps = 1\nnchnls = 1\ngkindex init 0\n"
	      (if in-score "" (csft-unnormalized ft-line))
	      "instr 1 \n kar tablei gkindex, " (csft-ftable-num ft-line) "\n"
	      "printk 0, kar\ngkindex = gkindex+" 
	      (number-to-string (/ (string-to-number ft-size) (float len))) "\n"
	      "endin\n</CsInstruments>\n<CsScore>\n" 
	      (if in-score (csft-unnormalized ft-line) "")
	      "i1 0 " (number-to-string len) " \ne\n</CsScore>\n</CsoundSynthesizer>"))
    (let ((buff (get-buffer-create csft-tmp-buffer))
	  (ft-nsize (string-to-number ft-size)))
      (save-window-excursion
	(shell-command 
	 (concat (shell-quote-argument (cscsd-csound-binary)) 
		 (if csft-enable-csound-display " " " -d ") 
		 (shell-quote-argument (cscsd-temp-csd-file)))
	 csft-tmp-buffer))
      (save-excursion
	(set-buffer buff)
	(let ((fv (make-vector len 0)))
	  (goto-char (point-min))
	  (re-search-forward "^new alloc.*$")
	  (dotimes (n len fv)
	    (re-search-forward  "[-.0-9]+$")
	    (aset fv n (string-to-number (match-string-no-properties 0))))
;	  (kill-buffer buff)
	  fv)))))

;(csft-get-ftable-vector "f 200 0 16 -2 1 0.5 1 1 1 1 1 1 1 1 1.5 1 1 1 2 1\n" 100)
;((csft-ftable-size "f 200 0 16 -2 1 0.5 1 1 1 1 1 1 1 1 1.5 1 1 1 2 1\n")

(defun csft-do-the-click ()
  (interactive)
  (when csft-mouse-click-action
    (let ((pos (posn-x-y (event-start last-input-event))))
      (funcall csft-mouse-click-action 
	       pos  
	       (* (funcall (plist-get (cdr csft-plotting-area) :x) (car pos)) csft-xratio)
	       (funcall (plist-get (cdr csft-plotting-area) :y) (cdr pos))))))

(defun csft-display-position (pos x y)
  (message "%s %f %f" pos x y))


;;======================================================================
;; utils.
;;======================================================================

(defun csft-ftable-num (ft-line)
  (save-match-data
    (string-match "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*\\([0-9]+\\)" ft-line)
    (match-string 3 ft-line)))

;TEST (csft-ftable-num "f2 0 1024 5 1 400 .0012 624 5") => "2"
;TEST (csft-ftable-num "gi ftgen 2, 0, 1024, 5, 1, 400, .0012, 624, 5") => "2"

(defun csft-ftable-nsize (ft-line)
  (string-to-number (csft-ftable-size ft-line)))

(defun csft-ftable-size (ft-line)
  (save-match-data
    (string-match "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*[0-9.]+[ \t,]+[0-9.]+[ \t,]+\\([0-9]+\\)" ft-line)
    (match-string 3 ft-line)))

;TEST (csft-ftable-size "f2 0 1024 5 1 400 .0012 624 5") => "1024"
;TEST (csft-ftable-size "gi ftgen 2, 0, 1024, 5, 1, 400, .0012, 624, 5") => "1024"

(defun csft-ftable-ngen (ft-line)
  (save-match-data
    (abs (string-to-number (csft-ftable-gen ft-line)))))

(defun csft-ftable-gen (ft-line &optional unnormalize)
  (save-match-data
    (string-match "^[ \t]*\\(f\\|\\(.*ftgen\\)\\)[ \t]*[0-9.]+[ \t,]+[0-9.,]+[ \t,]+[0-9.,]+[ \t,]+\\([-0-9,]+\\)" ft-line)
    (let ((genum (match-string 3 ft-line)))
      (when (and unnormalize
		 (> (string-to-number genum) 0))
	(setq ft-line (replace-match (format "-%s" genum) nil nil ft-line 3)))
      (if unnormalize ft-line genum))))

;TEST (csft-ftable-gen "f2 0 1024 5 1 400 .0012 624 5") => "5"
;TEST (csft-ftable-gen "f2 0 1024 -7 1 400 .0012 624 5") => "-7"

(defun csft-unnormalized (ft-line)
  (csft-ftable-gen ft-line t))

;TEST (csft-unnormalized "f2 0 1024 5 1 400 .0012 624 5") => "f2 0 1024 -5 1 400 .0012 624 5"
;TEST (csft-unnormalized "f2 0 1024 -5 1 400 .0012 624 5") => "f2 0 1024 -5 1 400 .0012 624 5"

(defun csft-convert-ftable-style (ft-line &optional gvarname)
  "Convert FT-LINE from a score f-statement into an orc ftgen statement
or the opposite way round, depending of the initial style.
GVARNAME if not nil is a string, the global variable name for ftgen output
it defaults to \"gif_\""
  (let ((spec ft-line)
	(comment ""))
    (when (string-match "\\(.*\\)\\(\\(;\\|/\\*\\).*$\\)" ft-line)
	(setq spec (match-string 1 ft-line) 
	      comment (match-string 2 ft-line))) 
    (if (string-match "ftgen\\(.*\\)" spec)
         (concat "f"
		 (mapconcat 'identity
			    (split-string (match-string 1 spec)  "[, \t]" t)
			    " ")
		 "\t" comment)
       (concat (or gvarname "gif_") " ftgen "
	       (mapconcat 'identity
			  (split-string spec "[f \t]" t)
			  ", ")
	       "\t" comment))))

;TEST (csft-convert-ftable-style "f6 0 128 -7 -.0 128 0\t; init velocity") => "gif_ ftgen 6, 0, 128, -7, -.0, 128, 0\t; init velocity"
;TEST (csft-convert-ftable-style "gif_ ftgen 6, 0, 128, -7, -.0, 128, 0\t; init velocity") => "f6 0 128 -7 -.0 128 0\t; init velocity"


;;======================================================================
;; edition
;;======================================================================

(defun csft-apply-changes ()
  (let ((ft-line csft-current-ftable-statement)
	(new-ft-line csft-ftable-statement))
    (setq csft-current-ftable-statement new-ft-line)
    (set-buffer csft-source-buffer)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote ft-line)))
	(replace-match (format ";backup; %s%s" ft-line new-ft-line))
      (error "could not find the f-table statement !"))))

(defun csft-insert-push-buttons ()
  (widget-create 'push-button
		 :notify (lambda (widget &rest ignore)
			   (csft-refresh-display
			    (concat (csft-read-string "display:" (delete ?\n csft-ftable-statement)) "\n")))
		 "Edit")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (widget &rest ignore)
			   (csft-apply-changes))
		 "Apply")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (widget &rest ignore)
			   (csft-refresh-display csft-original-ftable-statement))
		 "Reset")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (widget &rest ignore)
			   (kill-buffer (current-buffer))
			   (delete-frame))
		 "Quit")
  (widget-insert "\n\n")
  (use-local-map widget-keymap)
  (widget-setup))

(defun csft-read-string (prompt init)
  (let ((str init))
    (modify-frame-parameters (selected-frame) '((cursor-type . box)))
    (unwind-protect
      (setq str (read-string prompt init))
      (modify-frame-parameters (selected-frame) '((cursor-type . nil))))
    str))

(defun csft-set-parameters (bk-list &optional ft-line)
  (setq ft-line (or ft-line csft-ftable-statement))
  (if (string-match "f[ \t]*[0-9.]+[ \t]+[0-9.]+[ \t]+[0-9.]+[ \t]+[-0-9.]+[ \t]+\\([^;]*\\)" ft-line)  
      (replace-match (mapconcat 'number-to-string bk-list " ") nil nil ft-line 1)
    (string-match "ftgen[ \t]*[0-9.]+[ \t,]+[0-9.]+[ \t,]+[0-9.]+[ \t,]+[-0-9.]+[ \t,]+\\([^;]*\\)" ft-line)  
    (replace-match (mapconcat 'number-to-string  bk-list ", ") nil nil ft-line 1)))


;TEST (csft-set-parameters '(0.5 512 .002) "f2 0 1024 5 1 1024 .0012;comm") => "f2 0 1024 5 0.5 512 0.002;comm"
;TEST (csft-set-parameters '(5 512 2) "ib ftgen 2, 0, 512, 7, 1, 512, -1") => "ib ftgen 2, 0, 512, 7, 5, 512, 2"

(defun csft-get-parameters (&optional ft-line)
  (setq ft-line (or ft-line csft-ftable-statement))
  (save-match-data
    (string-match "\\(ftgen\\|f\\)[ \t]*\\([^;]*\\)" ft-line)
    (mapcar '(lambda (s) (string-to-number (delete ?, s)))
	    (cddddr (split-string (match-string 2 ft-line))))))

;TEST (csft-get-parameters "f2 0 1024 5 1 1024 .0012;comm") => '(1 1024 .0012)
;TEST (csft-get-parameters "itmp\tftgen 257, 0, 16384, 7, 1, 16384, -1	; sawtooth") => '(1 16384 -1)


;;======================================================================
;; GEN specific operations
;;======================================================================

(defcustom csft-precision 3
  "floating-point precision for generator arguments"
  :type 'integer
  :group 'csound-ft)

(defun csft-insert-radio-buttons (ft-gen)
  (when (assoc ft-gen csft-gennum-specs)
    (apply 'widget-create
	   'radio-button-choice
	   :value 'csft-mouse-click-action 
	   :notify (lambda (widget &rest ignore)
		     (setq csft-mouse-click-action (widget-value widget)))
	   (cadr (assoc ft-gen csft-gennum-specs)))
    (widget-insert "\n")))

(defvar csft-gennum-specs
 '((5 ((item :tag "About this breakpoint" 
	     :value csft-click-gen57-document-breakpoint)
       (item :tag "Remove breakpoint" 
	     :value csft-click-gen57-remove-breakpoint)
       (item :tag "Add breakpoint" 
	     :value csft-click-gen57-add-breakpoint)
       (item :tag "Slide breakpoint" 
	     :value csft-click-gen57-slide-breakpoint)))
   (7 ((item :tag "About this breakpoint" 
	     :value csft-click-gen57-document-breakpoint)
       (item :tag "Remove breakpoint" 
	     :value csft-click-gen57-remove-breakpoint)
       (item :tag "Add breakpoint" 
	     :value csft-click-gen57-add-breakpoint)
       (item :tag "Slide breakpoint"
	     :value csft-click-gen57-slide-breakpoint)))))

;=========================== GEN 5, GEN 7

(defun csft-gen57-closest-breakpoint (x bk-spec)
  (let* ((d 100000000)
	 (dtmp (abs (- x (car bk-spec))))
	 (pos (car bk-spec))
	 (n -2))
    (while (< dtmp d)
      (setq d dtmp
	    bk-spec (cddr bk-spec))
	    (if (car bk-spec)
	      (setq pos (+ pos (car bk-spec))
		    dtmp (abs (- x pos)))
	      (setq dtmp 1000000))
	    (incf n 2))
     (/ n 2)))

;TEST (csft-gen57-closest-breakpoint 600 (append '(0) (csft-get-parameters "f2 0 1024 5 1 1024 .0012"))) => 1

(defun csft-click-gen57-document-breakpoint (pos x y)
  (let* ((bkpoints (append '(0) (csft-get-parameters)))
	 (index (* 2 (csft-gen57-closest-breakpoint x bkpoints))))
    (message "%s"
	     (cons (nth index bkpoints) (nth (1+ index) bkpoints)))))

(defun csft-click-gen57-remove-breakpoint (pos x y)
  (let ((bkpoints (append '(0) (csft-get-parameters))))
    (csft-refresh-display
     (csft-set-parameters 
      (cdr (csft-gen57-remove-breakpoint (csft-gen57-closest-breakpoint x bkpoints)
					 bkpoints))))))
  
(defun csft-click-gen57-add-breakpoint (pos x y)
  (let ((bkpoints (append '(0) (csft-get-parameters))))
    (csft-refresh-display
     (csft-set-parameters
      (cdr (csft-gen57-add-breakpoint x y bkpoints))))))

(defun csft-gen57-remove-breakpoint (n bk-spec)
  (if (zerop n)
      bk-spec
    (let ((i 0) (offset 0) nbk-spec)
      (dolist (bkp (csft-pair-breakpoints bk-spec) nbk-spec)
	(if (or (< i n) (> i (1+ n)))
	    (setq nbk-spec (append nbk-spec bkp))
	  (if (= i n)
		(setq offset (car bkp))
	    (setq nbk-spec (append nbk-spec (list (+ offset (car bkp)) (cadr bkp))))))
	(incf i)))))

;TEST (csft-gen57-remove-breakpoint 1 (append '(0) (csft-get-parameters "f2 0 1024 5 1 400 .0012 624 5"))) => '(0 1 1024 5)

(defun csft-gen57-add-breakpoint (x y bk-spec)
  (let ((accum 0)
	prevaccum done nbk-spec
	(x (round x))
	(y (/ (round (* y (expt 10 csft-precision))) (expt 10.0 csft-precision))))
    (dolist (bkp (csft-pair-breakpoints bk-spec) nbk-spec)
      (setq accum (+ accum (car bkp)))
      (if (or (< accum x) done)
	  (setq nbk-spec (append nbk-spec bkp)
		prevaccum accum)
	(setq nbk-spec (append nbk-spec (list (- x prevaccum) y) (list (- accum x) (cadr bkp)))
	      done t)))))

;TEST (csft-gen57-add-breakpoint 500 2 (append '(0) (csft-get-parameters "f2 0 1024 5 1 400 3 624 5"))) => '(0 1 400 3 100 2 524 5)

(defun csft-pair-breakpoints (bk-spec)
  (let (ll)
    (while bk-spec
      (setq ll (append ll (list (list (car bk-spec) (cadr bk-spec))))
	    bk-spec (cddr bk-spec)))
      ll))

;TEST (csft-pair-breakpoints (append '(0) (csft-get-parameters "f2 0 1024 5 1 1024 .0012"))) => '((0 1) (1024 0.0012))

	  

;;======================================================================
;; this is it

(provide 'csound-ft)

;; csound-ft.el ends here




