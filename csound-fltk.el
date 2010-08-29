;;; -*- auto-recompile: t -*-

;;; csound-fltk.el --- Utilities for FLTK programming

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-fltk.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-fltk.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;; last modified July 15, 2009


(defun insert-indent (&rest args)
  (let ((start (point)))
    (apply 'insert args)
    (indent-region start (point))))

(defun FLcolor (color)
  "Insert at point opcode FLColor for COLOR, a string"
  (insert-indent
   (concat "FLcolor " (mapconcat (lambda (c) (format "%s" (/ c 256)))
				 (color-values color) ", ")
	   " ; " color)))

(defun FLsetColor (color ihandle)
  "Insert at point opcode FLsetColor for COLOR, a string, and IHANDLE"
  (insert-indent
   (concat "FLsetColor " (mapconcat (lambda (c) (format "%s" (/ c 256)))
				 (color-values color) ", ")
	   ", " ihandle " ; " color)))

(defun FLinfo (title string &optional color width)
  "Insert at point the orchestra code setting up a FLpanel with TITLE
displaying STRING. Lines within STRING can be separated by \"\\\\\",
which is handy if `FLinfo' is invoked from a DO button \(see `cseel-DO') .
COLOR if nil defaults to white.
WIDTH if nil fits the longest line.
"
  (let* ((info-lines (split-string string "\\(\n\\|\\\\\\)"))
	 (lines-length (apply 'max (mapcar 'length info-lines)))
	 (width (or width (max 250 (* 8 (1+ lines-length)))))
	 (fsp (format "%%-%ss" lines-length)))
    (FLcolor (or color "white"))
    (insert-indent ?\n 
	    (format "FLpanel \"%s\", %s, %s, 512, 0" 
		    title width (* 20 (length info-lines))) 
	    ?\n)
    (loop for info-line in info-lines
	  for y from 0 by 20
	  do (insert-indent
	      (format "ihinfo FLbox \"%s\", 1, 5, 14, %s, 15, 5, %s"
		      (format fsp info-line) 
		      (- width 5) y)
	      ?\n))
    (insert-indent "FLpanelEnd")))

;; === this is it.
(provide 'csound-fltk)

;; csound-fltk.el ends here

