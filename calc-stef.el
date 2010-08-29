
;; ---> définit des macros pour incorporer du calc dans du code ordinaire
;;      (et initialise le système)

(provide 'calc-stef)
(or 
 (require 'calc nil nil)  ;; emacs 22 or newer
 (load "calc-math"))      ;; hack

(setq calc-internal-prec 8)         ;; précision
(setq calc-angle-mode 'rad)         ;; angles mesurés en radian

(defvar calc-defmath-depth 0) 

(defmacro with-raw-defmath (&rest body)
  "Evaluate BODY within a \\[defmath] and returns the result"
  (declare (indent 0))
 `(let (result)
    (incf calc-defmath-depth)
    (unwind-protect
	(calc-wrapper
	 (setq result (apply (defmath dummy () ,@body) ())))
      (decf calc-defmath-depth))
    result))

;;; ??? à tester extensivement !

(defmacro with-alg-defmath (&rest body)
  "Evaluate BODY within a \\[defmath] and returns the result as a string (algebraic syntax)"
  (declare (indent 0))
  (if (zerop calc-defmath-depth)
      `(calc-eval (with-raw-defmath ,@body))
    `(with-raw-defmath ,@body)))

(defmacro with-defmath (&rest body)
  "Evaluates BODY within a `defmath' and returns a number.
Be careful as, if the evaluation returns a Calc data type not readable as a number, the macro will return 0
So a 0 output is either the result or an error signal. This should be improved I guess..."
  (declare (indent 0))
  (if (zerop calc-defmath-depth)
      `(string-to-number (with-alg-defmath ,@body))
    `(with-raw-defmath ,@body)))

(defmacro with-raw-defmath-import (vlist &rest body)
  "Similar to \\[with-raw-defmath], but allows in VLIST the importation of symbols which will refer to the same value inside the defmath as outside (sort of \"globals\")."
  (declare (indent 1))
  `(let ,(mapcar (lambda (v) 
		   (list (intern (concat "var-" (symbol-name v)))
			 (list 'math-read-number (list 'prin1-to-string v))))
		 vlist)
     (with-raw-defmath ,@body)))

(defmacro with-alg-defmath-import (vlist &rest body)
  "Similar to \\[with-alg-defmath], but allows in VLIST the importation of symbols which will refer to the same value inside the defmath as outside (sort of \"globals\")."
  (declare (indent 1))
  `(let ,(mapcar (lambda (v) 
		   (list (intern (concat "var-" (symbol-name v)))
			 (list 'math-read-number (list 'prin1-to-string v))))
		 vlist)
     (with-alg-defmath ,@body)))

(defmacro with-defmath-import (vlist &rest body)
  "Similar to \\[with-defmath], but allows in VLIST the importation of symbols which will refer to the same value inside the defmath as outside (sort of \"global\")."
  (declare (indent 1))
  `(let ,(mapcar (lambda (v) 
		   (list (intern (concat "var-" (symbol-name v))) 
			 (list 'math-read-number (list 'prin1-to-string v))))
		 vlist)
     (with-defmath ,@body)))


;une opération à la noix pour lancer le shmilblick :
(calc-eval (apply (defmath dummy () (/ 1 2)) ()))



;; ============================================== Util.:

(defmath max-value (v)        ;; v vector or list, possibly nested 
  (let ((m nil))
    (for (i 1 (length v))
	 (setq tmp (elt v (- i 1)))
	 (if (and (not (realp tmp))
		  (listp tmp))
	     (setq tmp (max-value tmp)))
	 (if (and (realp tmp)
		  (or (null m) (> tmp m)))
	     (setq m tmp)))
    m))


(defmath min-value (v)       
  (let ((m nil))
    (for (i 1 (length v))
	 (setq tmp (elt v (- i 1)))
	 (if (and (not (realp tmp))
		  (listp tmp))
	     (setq tmp (min-value tmp)))
	 (if (and (realp tmp)
		  (or (null m) (< tmp m)))
	     (setq m tmp)))
    m))


;le concat de calc n'accepte que 2 arguments, donc:

(defmath m-concat (&rest args)
  (let ((res ""))
    (while args
      (if (car args)
	  (setq res (concat res (car args))))
      (setq args (cdr args)))
    res))


;; prettify keyboard

(require 'calc-keypd)
(defun calc-keypad-redraw ()
  (set-buffer calc-keypad-buffer)
  (setq buffer-read-only t)
  (setq calc-keypad-full-layout (append (symbol-value (nth calc-keypad-menu
							   calc-keypad-menus))
					calc-keypad-layout))
  (let ((buffer-read-only nil)
	(row calc-keypad-full-layout)
	(y 0))
    (erase-buffer)
    (insert "\n")
    (while row
      (let ((col (car row)))
	(while col
	  (let* ((key (car col))
		 (cwid (if (>= y 4)
			   5
			 (if (and (= y 3) (eq col (car row)))
			     (progn (setq col (cdr col)) 9)
			   4)))
		 (name (if (and calc-standalone-flag
				(eq (nth 1 key) 'calc-keypad-off))
			   "EXIT"
			 (if (> (length (car key)) cwid)
			     (substring (car key) 0 cwid)
			   (car key))))
		 (wid (length name))
		 (pad (- cwid (/ wid 2))))
	    (insert
	     (propertize (concat
			  (make-string (/ (- cwid wid) 2) 32)
			  name
			  (make-string (/ (- cwid wid -1) 2) 32))
			 'font-lock-face 'custom-button-face
			 'mouse-face 'custom-button-mouse
			 )
	     (if (equal name "MENU")
		 (int-to-string (1+ calc-keypad-menu))
	       " ")))
	  (or (setq col (cdr col))
	      (delete-char -1) (insert "\n")))
	(insert ?\n)
	(setq y (1+ y)
	      row (cdr row)))))
  (setq calc-keypad-prev-input t)
  (calc-keypad-show-input)
  (goto-char (point-min)))

;(calc-keypad-redraw )

(defun calc-keypad-show-input ()
  (or (equal calc-keypad-input calc-keypad-prev-input)
      (let ((buffer-read-only nil))
	(save-excursion
	  (goto-char (point-min))
	  (forward-line 1)
	  (delete-region (point-min) (point))
	  (if calc-keypad-input
	      (insert "Calc: " calc-keypad-input "\n")
	    (insert "Calc ("
		    (int-to-string (1+ calc-keypad-menu))
		    ")\n")))))
  (setq calc-keypad-prev-input calc-keypad-input))

