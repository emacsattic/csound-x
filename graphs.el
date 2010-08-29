;;; -*- auto-recompile: t -*-

;;; graphs.el --- basic functions for plotting graphs

;; This file is not part of GNU Emacs.
;; 
;; graphs.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; graphs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

(defun make-plotting-area (nx ny x0 x1 y0 y1)
  (let ((graph (let ((gv (make-vector ny nil)))
		 (dotimes (vline ny gv)
		   (aset gv vline (make-bool-vector nx t)))))
	(dx (/ (- x1 x0) (float nx)))
	(dy (/ (- y1 y0) (float ny))))
  `(plotting-area
    :width ,nx
    :height ,ny
    :graph ,graph
    :x0 ,x0
    :y0 ,y0
    :x1 ,x1
    :y1 ,y1
    :dx ,dx
    :dy ,dy
    :x (lambda (x) (+ ,x0 (* x ,dx)))
    :y (lambda (y) (+ ,y0 (* (- ,ny y) ,dy)))
    :nx (lambda (x) (round (/ (- x ,x0) ,dx)))
    :ny (lambda (y) (round (/ (- ,y1 y) ,dy)))
    :plot (lambda (x y) 
	    (condition-case nil 
		(aset (aref ,graph (round (/ (- ,y1 y) ,dy)))
		      (round (/ (- x ,x0) ,dx))
		      nil)
	      (error nil)))
    :line (lambda (xa ya xb yb)
	    (let* ((nxa (round (/ (- xa ,x0) ,dx)))
		   (nya (round (/ (- ,y1 ya) ,dy)))
		   (nxb (round (/ (- xb ,x0) ,dx)))
		   (nyb (round (/ (- ,y1 yb) ,dy)))
		   (mdn (max (abs (- nxb nxa)) (abs (- nyb nya)))))
	      (if (<= mdn 1)
		  (condition-case nil
		      (aset (aref ,graph nya) nxa nil)
		    (error nil))
		(dotimes (n mdn)
		  (let ((px (+ nxa (round (* n (/ (- nxb nxa) (float mdn))))))
			(py (+ nya (round (* n (/ (- nyb nya) (float mdn)))))))
		    (condition-case nil
			(aset (aref ,graph py) px nil)
		      (error nil))))))))))

(defun plot-point (x y parea)
  (funcall (plist-get (cdr parea) :plot) x y))

(defun plot-segment (x0 y0 x1 y1 parea)
  (funcall (plist-get (cdr parea) :line) x0 y0 x1 y1))

(defun plot-function (f parea &optional segmented)
  (let ((x (plist-get (cdr parea) :x0)) y
	(dx (plist-get (cdr parea) :dx))
	(plot-f (plist-get (cdr parea) :plot))
	(line-f (plist-get (cdr parea) :line)))
    (dotimes (nx (plist-get (cdr parea) :width))
      (condition-case nil
	  (if segmented
	      (funcall line-f 
		       x (or y (funcall f x)) 
		       (incf x dx) (setq y (funcall f x))) 
	    (funcall plot-f x (funcall f (incf x dx))))
	(error nil)))))

(defun plot-vector (v parea &optional segmented)
  (plot-function (lambda (x) (aref v (round x))) parea segmented))

(defun insert-plotting-area (parea &optional foreground background mouse-1-action)
  (let ((image `(image :type xbm 
		       :data ,(plist-get (cdr parea) :graph)
		       :width ,(plist-get (cdr parea) :width) 
		       :height ,(plist-get (cdr parea) :height)
		       :foreground ,(or foreground "black") 
		       :background ,(or background "white")
		       :pointer arrow
		       :plotting-area ,parea)))  ;; currently unused
    (insert-image image
		  (propertize " "
			      'local-map  (let ((map (make-sparse-keymap)))
					    (define-key map [mouse-1] mouse-1-action)
					    map)))
    image))

(defun pop-up-area (parea &optional foreground background text title mouse-1-action)
  (let* ((pop-up-frames t)
	 (frame (window-frame (display-buffer (set-buffer (generate-new-buffer "*PLOT*")))))
	 (fw (/ (plist-get (cdr parea) :width) (frame-char-width frame)))
	 (fh (/ (plist-get (cdr parea) :height) (frame-char-height frame))))
    (erase-buffer)
    (setq mode-line-format nil)
    (insert-plotting-area parea foreground background mouse-1-action)
    (insert (or text "\n"))
    (modify-frame-parameters frame
     `((cursor-type . nil)
       (title . ,(or title "plot"))
;       (unsplittable . t)
       (left-fringe . 0)
       (right-fringe . 0)
       (minibuffer . nil)
       (tool-bar-lines . nil)
       (menu-bar-lines . 0)
       (width . ,(1+ fw))
       (height . ,(1+ fh))))
    (current-buffer)))

;; ========== examples:

; (graphs-example-1)
; (graphs-example-1 t)
; (graphs-example-2)

(defun graphs-example-1 (&optional segmented)
  (let* ((parea (make-plotting-area 800 400 1 5 -0.5 1.5))
	 (image (insert-plotting-area parea "blue" "honeydew")))
    (dotimes (n 5)
      (plot-function (lambda (x) (- (abs (* (cos (sqrt x)) (+ n (sin x)))) (sqrt (/ n 20.0)))) parea segmented)
      (sit-for .4)
      (clear-image-cache (selected-frame)))))

(defun graphs-example-2 ()
  (let ((parea (make-plotting-area 800 400 1 5 -0.5 1.5)))
    (dotimes (n 5)
      (plot-function (lambda (x) (- (abs (* (cos (sqrt x)) (+ n (sin x)))) (sqrt (/ n 20.0)))) parea t))
    (pop-up-area parea "gainsboro" "DarkGreen" nil "un zoli dessin")))

;;======================================================================
;; this is it

(provide 'graphs)

;; graphs.el ends here