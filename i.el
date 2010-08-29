;;; -*- auto-recompile: t -*-

;;; i.el --- a library for generating csound scores

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; i.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; i.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;; last modified April 17, 2009

(require 'cl)
(require 'csound-eel)

;; ===================================================================
;;                   i.el ---  score-generating utilities
;; ===================================================================

(defvar pfstream-pf-attributes 
  '((1 (:instr 1)) (2 (:start 0)) (3 (:duration 0)))
  "Keys associated to the different p-fields, and their default values
Format: '\(\(NP \(KEY DEFAULT)) ...) where DEFAULT is either a number, 
a string or a form.
Changes to this variable are local to the enclosing `with-pfields-stream'
form the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-pf-attributes)

(defun pfstream-pf-at-key (key)
  "p-field number associated to KEY in `pfstream-pf-attributes'"
  (loop for spec in pfstream-pf-attributes
        when (equal (caadr spec) key)
        return (car spec)))

(defun pfstream-key-at (np)
  "key associated to p-field number NP in `pfstream-pf-attributes'"
  (caadr (assoc np pfstream-pf-attributes)))

(defun pfstream-pf-value (p-field)
  "Return the current value of P-FIELD, a number or its associated key
note that when the current value of a p-field is a quoted lisp form, 
it is returned as such.
use `current-pfield' if you want the actual pfield value"
  (let ((n (if (numberp p-field) p-field (pfstream-pf-at-key p-field))))
    (loop for spec in pfstream-pf-attributes
          when (equal (car spec) n)
          return (cseel-lambdaify (cadadr spec)))))

(defun cseel-lambdaify (form)  
  (if (or (null (listp form))
	  (equal (car form) 'lambda))          
      form
    (if (equal (car form) 'quote)
	(list 'lambda nil (cadr form))
      (list 'lambda nil form))))

(defvar pfstream-custom-pf-keys '()
  "Register custom extensions to the key syntax, allowing to operate on 
several keys at once through lambda expressions.
Format: '((:customkey (:key1 lambda1 :key2 lambda2 ...)) ...)
Changes to this variable are local to the enclosing `with-pfields-stream'
form the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-custom-pf-keys)

(defvar pfstream-time-markers nil
  "Repository of time markers.
Format: '((:key time) ...)
Changes to this variable are local to the enclosing `with-pfields-stream'
form the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-time-markers)

(defvar pfstream-last-p2 0 
"Buffer value for p2
Changes to this variable are local to the enclosing `with-pfields-stream'
form the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-last-p2)

(defvar pfstream-last-p3 0
 "Buffer value for p3
Changes to this variable are local to the enclosing `with-pfields-stream'
form the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-last-p3)

(defvar pfstream-end-time 0
 "Maximum value reached so far by p2+p3
changes to this variable are local to the enclosing `with-pfields-stream' form
the top-level value is buffer local")

(make-variable-buffer-local 'pfstream-end-time)


(defgroup csound-i nil
  "Generating i-statements in Emacs Lisp"
  :group 'csound-x
  :prefix "pfstream-")

(defcustom pfstream-float-format "%.4f"
  "Spec used by `format' when inserting a float with i, |i and +i"
  :type 'string
  :group 'csound-i)

(defcustom pfstream-separator "\t"
  "P-fields separator used by i, |i and +i"
  :type 'string
  :group 'csound-i)

;; ===================================================================
;;                    basic usage (examples & tests)
;; ===================================================================

(defun i-TEST1 ()
  (with-temp-buffer-to-string
    (let ((pfstream-separator " "))
      (with-pfields-stream '((1 (:instr 1))
			     (2 (:start 150.0))
			     (3 (:duration 1.5))
			     (4 (:pitch 8.01))
			     (5 (:volume 1000)))
	(loop repeat 3 do
	      (+i :pitch 7.11)
	      (+i :pitch 8.02 :volume ">"))
	(end-note "now is bingo")
	(now-> :bingo)
	(+i :duration 0.8)
	(+i :duration 0.5)
	(note "jumping around:")
	(t- 5) (|i :volume 1500)
	(t+ 35) (|i)
	(note "back in time to bingo")
	(now-be :bingo) (|i)
	(end-of-section)))))
  
;TEST (i-TEST1) => "i 1 150.0000 1.5000 7.1100 1000\ni 1 151.5000 1.5000 8.0200 >\ni 1 153.0000 1.5000 7.1100 >\ni 1 154.5000 1.5000 8.0200 >\ni 1 156.0000 1.5000 7.1100 >\ni 1 157.5000 1.5000 8.0200 >	; now is bingo\ni 1 159.0000 0.8000 8.0200 >\ni 1 159.8000 0.5000 8.0200 >\n; jumping around:\ni 1 154.8000 0.5000 8.0200 1500\ni 1 189.8000 0.5000 8.0200 1500\n; back in time to bingo\ni 1 157.5000 0.5000 8.0200 1500\ne\n\n"

(defun i-TEST2 ()
  (with-temp-buffer-to-string
    (let ((pfstream-separator ""))
      (save-stream
	(i-be 1 0 1)
	(+i)
	(then-> :t0)
	(+i)
	(save-stream
	  (+i)(+i))
	(+i)(+i)
	(save-stream-insert
	  (+i)(+i :p3 2))
	(+i)
	(now-be :t0)
	(+i :p3 3)
	(+i)
	(now-be :t0)
	(|i)
	(+i)))))

;TEST (i-TEST2) => "i 101\ni 111\ni 121\ni 131\ni 121\ni 131\ni 141\ni 152\ni 171\ni 113\ni 143\ni 113\ni 143\n"

(defun i-TEST3 ()
  (with-temp-buffer
    (let (ss1 ssi2 ss2)
      (save-stream
	(i-be 1 0 1)
	(setq ss1
	      (save-stream
		(+i :p3 250)
		(setq ssi2
		      (save-stream-insert
			(+i) 'yo))
		(end-time)))
	(+i)
	(setq ss2
	      (save-stream
		(+i)
		(end-time)))
	(list ss1 ssi2 ss2 (now) (then) (end-time))))))

;TEST (i-TEST3) => '(500 yo 2 0 1 500)

(defun i-TEST4 ()
  (with-temp-buffer-to-string
    (let ((pfstream-separator ""))
      (save-stream
	(i-be 1 0 1)
	(+i)
	(save-stream-insert
	  (save-stream
	    (+i :p3 5))
	  (save-stream
	    (+i :p3 2))
	  (save-stream
	    (+i :p3 3)))
      (+i)))))

;TEST (i-TEST4) => "i 101\ni 115\ni 112\ni 113\ni 161\n"


;; ===================================================================
;;                          p-fields streams
;; ===================================================================

(defvar pfstream-depth 0
  "How many embedded `with-pfields-stream' constructs are we in ?")

(defvar pfstream-letf-spec ()
  "Specification list for the `letf' call wrapping the code within
a `with-pfields-stream'; transparently built by `pfstream-defun'.")

(defmacro pfstream-defun (name args &rest body)
  "Define a new function with a short NAME valid only within
a `with-pfields-stream' body, and full name pfstream:NAME "
  (declare (indent defun))
  (unless (zerop pfstream-depth)
    (error "pfstream-defun should not be used within a pfield-stream"))
  (let ((full-name (intern (concat "pfstream:" (symbol-name name)))))
  `(progn
     (defun ,full-name ,args ,@body)
     (add-to-list 'pfstream-letf-spec
		  '((symbol-function ',name)
		     (symbol-function ',full-name)) t))))

(defun pfstream-defalias (short-name full-name)
  "Define an alias for function FULL-NAME, valid only within
the current a pfield-stream"
  (unless (zerop pfstream-depth)
    (error "pfstream-defalias should not be used within a pfield-stream"))
  (add-to-list 'pfstream-letf-spec
	       `((symbol-function ',short-name)
		 (symbol-function ',full-name)) t))

(defun cseel-make-standard-custom-pkeys ()
  "Define a :pN custom keyword for each pfield except if that keyword
 is the one used in `pfstream-pf-attributes'"
  (dolist (pfa pfstream-pf-attributes)
    (let* ((n (car pfa))
	   (ksym (intern (format ":p%d" n)))
	   (current-sym (pfstream-key-at n)))
      (unless (eq ksym current-sym)
	(add-custom-key `(,ksym (,current-sym 'identity)))))))

(defmacro with-pfields-stream (p-fields &rest body)
  "Defines a set of local variables used to maintain a stream of p-fields,
then process BODY within that stream. 
P-FIELDS is the local value of `pfstream-pf-attributes'.
When incomplete it inherits its missing attributes from the upper scope.
All functions defined via `pfstream-defun' can be used in BODY.

See Info node `(csound-x)The i library' for extensive documentation."
  (declare (indent 1))
  `(let ((with-pfields-stream:::parent-pfstream-end-time pfstream-end-time)
	 with-pfields-stream:::return-value)
     (let* ((pfstream-custom-pf-keys (copy-sequence pfstream-custom-pf-keys))
	    (pfstream-pf-attributes (pfstream-inherit-with ,p-fields)) 
	    (pfstream-depth (1+ pfstream-depth))
	    (pfstream-time-markers (copy-sequence pfstream-time-markers))
	    (pfstream-last-p2 (current-pfield 2))
	    (pfstream-last-p3 pfstream-last-p3)
	    (pfstream-end-time 0))
       (cseel-make-standard-custom-pkeys)
       (eval 
	(list 'letf* pfstream-letf-spec 
	      '(progn 
		 (setq with-pfields-stream:::return-value
		       (progn ,@body))
		 (setq with-pfields-stream:::parent-pfstream-end-time
		       (max with-pfields-stream:::parent-pfstream-end-time 
			    pfstream-end-time))))))
     (setq pfstream-end-time 
	   with-pfields-stream:::parent-pfstream-end-time)
     with-pfields-stream:::return-value))

(defmacro with-pfields-stream-sized (n-pfields &rest body)  
  "Extends the current stream size to N-PFIELDS
Do nothing if the stream is already wide enough.
Generated keywords for accessing the new p-fields have form :pN"
  (declare (indent 1))
  `(with-pfields-stream 
       (quote ,(let ((n n-pfields) map)
		 (while (not (pfstream-key-at n))
		   (setq map 
			 (append map `((,n (,(intern (format ":p%d" n)) 0)))))
		   (decf n))
		 map))
     ,@body))

(defmacro save-stream (&rest body)
  "Create a new p-field stream with all p-fields values, time 
markers and custom keys inherited from the current stream. 
Once BODY is evaluated, everything gets restored.
Equivalent to `with-pfields-stream' with a nil P-FIELDS argument"
  (declare (indent 0))
  `(with-pfields-stream () ,@body))

(defun pfstream-inherit-with (pfschild)
  (dolist (pf pfschild)
    (when (assoc (car pf) pfstream-custom-pf-keys)
      (setq pfstream-custom-pf-keys
            (assq-delete-all (car pf) pfstream-custom-pf-keys))))
  (let ((npfs (copy-sequence pfschild)))
    (dolist (pf pfstream-pf-attributes)
      (unless (assoc (car pf) pfschild)
	(setq npfs (append (list pf) npfs ))))
    npfs))

(defun cseel-getval (val)
  (if (and val (listp val))
      (funcall (cseel-lambdaify val))
    val))

(defun current-pfield (p-field)
  "Return the current value of P-FIELD, a number or its associated key
Note that when the current value of a p-field is a quoted lisp form, it 
is not returned as such: intead, its evaluation as the body of a lambda 
expression is returned
Use `pfstream-pf-value' to get the form itself "
 (cseel-getval (pfstream-pf-value p-field)))

(defun set-pfield (p-field val &optional autocreate)
  "Set the current value of P-FIELD, a number or its associated key, 
to VAL, a number or a string or a quoted form
This function does not accept a custom key: use `set-in-stream' instead
P-FIELD can be arbitrarily large; when out of range it is silently ignored
except if AUTOCREATE is not nil in which case a corresponding :pN key is 
defined on the fly"
  (let* ((n (if (numberp p-field) p-field (pfstream-pf-at-key p-field)))
         (key (pfstream-key-at n)))
    (setq pfstream-pf-attributes (assq-delete-all n pfstream-pf-attributes))
    (when (and (null key) autocreate)
      (setq key (intern (format ":p%s" n))))  
    (add-to-list 'pfstream-pf-attributes 
                   `(,n (,key ,(cseel-lambdaify val))) t))) 

(defun set-in-stream (&rest key-val)
  "Set the current values of KEYs to VALs, expanding a KEY if it is
a custom key."
  (let ((kv-list (cseel-expand-keys-in key-val)))
    (while kv-list
      (set-pfield (pop kv-list) (pop kv-list)))))

(defun i-be (&rest p-fields)
  "Set all p-fields in the stream; do not insert an i-statement."
  (if (find-if 'keywordp p-fields)
      (apply 'i-set (cseel-all-pfields-with p-fields))
    (loop for pf in p-fields
          for i = 1 then (1+ i)
          do (set-pfield i pf t))
    (setq pfstream-last-p2 (current-pfield 2)
	  pfstream-last-p3 0)))

(defun i (&rest p-fields)
  "Insert an i-statement at point
If P-FIELDS includes a keyword, act exactly as function |i
else set all p-fields in the stream as a side effect"
  (if (find-if 'keywordp p-fields)
      (eval (append '(|i) (expand-i-statement p-fields)))
    (loop for pf in p-fields
          for i = 1 then (1+ i)
          do (set-pfield i pf t))
    (apply 'i-primitive p-fields)))


;TEST (with-temp-buffer-to-string (i 1 0 5)) => "i 1\t0\t5\n"
;TEST (with-temp-buffer-to-string (i 1 0 5 200)) => "i 1\t0\t5\t200\n"

(defun expand-i-statement (ilist)
  (let (xpilist (n 0))
    (while ilist
      (incf n)
      (if (keywordp (car ilist))
	  (setq xpilist (append xpilist ilist)
		ilist nil)
	(setq xpilist (append xpilist (list (pfstream-key-at n) (car ilist)))
	      ilist (cdr ilist))))
    xpilist))
      
;TEST (expand-i-statement '(1 0 5)) => '(:instr 1 :start 0 :duration 5)
;TEST (expand-i-statement '(1 0 5 :vol (+ 5 6))) => '(:instr 1 :start 0 :duration 5 :vol (+ 5 6))

(defun i-primitive (&rest p-fields)
  (insert "i "
	  (mapconcat (lambda (pf) 
		       (if (stringp pf) pf
			 (if (integerp pf) (format "%d" pf)
			   (format pfstream-float-format pf))))
		     p-fields pfstream-separator)
	  "\n")
  (setq pfstream-last-p2 (current-pfield 2)  
	pfstream-last-p3 (current-pfield 3)
	pfstream-end-time (max pfstream-end-time 
			       (+ pfstream-last-p2 (abs pfstream-last-p3)))))

(defun |i (&rest pf)
  "Insert an i-statement at point, using keys to identify the changing p-fields
The other p-fields values are inherited from the local stream"
  (setq pf (cseel-expand-keys-in pf))
  (apply 'i-primitive (cseel-all-pfields-with pf)))

(defun cseel-all-pfields-with (pf)
  ""
  (loop for cp from 1 upto (length pfstream-pf-attributes)
	for pfval = (plist-get pf (pfstream-key-at cp))
	if pfval do (set-pfield cp (cseel-getval pfval))
	collect (current-pfield cp)))

(defun +i (&rest pf)          
  "Same as |i plus increment the current p2 by the current (abs p3)" 
  (eval `(|i ,@pf ,(pfstream-key-at 2) 
	     ,(+ pfstream-last-p2 (abs pfstream-last-p3)))))
 

;; ===================================================================
;;                           custom keys          
;; ===================================================================

(defun cseel-expand-keys-in (pf)
  "Expand all keys in property-list PF that are registered in `pfstream-custom-pf-keys'"
  (let (realk-list realk-val expanded-pf)
    (dolist (key (remove-if-not 'keywordp pf) expanded-pf)
      (setq realk-list (cadr (assoc key pfstream-custom-pf-keys)))
      (when (functionp realk-list)
        (setq realk-list (funcall realk-list (plist-get pf key))))
      (if realk-list
	  (dolist (realkey (remove-if-not 'keywordp realk-list))
	    (setq realk-val (plist-get realk-list realkey))
	    (setq expanded-pf (append expanded-pf
				      (if (assoc realkey pfstream-custom-pf-keys)
					  (cseel-expand-keys-in (list realkey realk-val))
					(list realkey
					      (if (functionp (eval realk-val))
						  (funcall (eval realk-val)
							   (plist-get pf key))
						realk-val))))))
	(setq expanded-pf (append expanded-pf (list key
						    (plist-get pf key))))))))

(defun add-custom-key (new-key-def) 
  "Register a new custom key for setting pfields in the stream
NEW-KEY-DEF is a list \(KEY VAL) where VAL is a plist or a function of one variable 
returning a plist: KEY is a keyword which will be expanded into that plist.
when VAL is a function its single argument will be the value associated to KEY when 
it is invoked; this argument is not restricted to any type"
  (let ((key (car new-key-def)))
  (when (pfstream-pf-at-key key)
    (error "%s already defined in 'pfstream-pf-attributes" key))
  (when (assoc key pfstream-custom-pf-keys)
    (setq pfstream-custom-pf-keys (assq-delete-all key pfstream-custom-pf-keys)))
  (add-to-list 'pfstream-custom-pf-keys new-key-def)))

(defun add-custom-keys (new-key-def-list)
  "Register at once a list of new custom keys"
  (dolist (kd new-key-def-list)
    (add-custom-key kd)))

;TEST (progn (setq pfstream-custom-pf-keys '((:boum (:start (lambda (v) (+ 10 v)) :duration (lambda (v) (* 10 v)))))) (cseel-expand-keys-in '(:boum 25))) => '(:start 35 :duration 250)

;TEST (progn (add-custom-key '(:arouh (:start 2000 :duration 0.01))) (cseel-expand-keys-in '(:instr 12 :arouh 0))) => '(:instr 12 :start 2000 :duration 0.01)

;TEST (progn (add-custom-key '(:arouh2 (:volume 500 :arouh 0))) (cseel-expand-keys-in '(:instr 12 :arouh2 0))) => '(:instr 12 :volume 500 :start 2000 :duration 0.01)

;TEST (progn (add-custom-key '(:loud (:duration 'identity :amp 'sqrt))) (cseel-expand-keys-in '(:loud 16))) => '(:duration 16 :amp 4)

;TEST (progn (add-custom-key '(:string (:duration 'length :amp 'string-to-number))) (cseel-expand-keys-in '(:string "1548"))) => '(:duration 4 :amp 1548)

;TEST (progn (add-custom-key '(:plist (:duration (lambda (pl) (plist-get pl :d)) :amp (lambda (pl) (plist-get pl :a))))) (cseel-expand-keys-in '(:plist (:a 2000 :d 5)))) => '(:duration 5 :amp 2000)

;TEST (progn (add-custom-key '(:xx (lambda (x) `(:duration ,(* 10 x) :amp ,(* 1111 x))))) (cseel-expand-keys-in '(:xx 5))) => '(:duration 50 :amp 5555)


;; ===================================================================
;; function querying and setting time, either through direct values
;; or through keys in p-list pfstream-time-markers
;; ===================================================================

(pfstream-defun now ()
 "Return the current p2 value
\(used for a `|i' note inserted immediately)"
 (current-pfield 2))

(pfstream-defun then ()
 "Return the current value of the last p2+p3
\(used for a `+i' note inserted immediately)"
 (+ pfstream-last-p2 pfstream-last-p3))

(pfstream-defun end-time ()
 "Return the maximum value reached by p2+p3 in the current stream"
 pfstream-end-time)

(pfstream-defun end-time-be (time)
 "Define the duration of the current stream, independently of the
actual maximum value reached by p2+p3"
 (setq pfstream-end-time (cseel-actual-time time)))

(pfstream-defun rest (duration)
 "Set the current p2 to p2+p3+DURATION"
  (now-be (+ (then) duration)))

(pfstream-defun t+ (time)
  "Change the current p2 by adding TIME, 
a number or a key in `pfstream-time-markers'"
  (now-be (+ (current-pfield 2) (cseel-actual-time time)))) 

(pfstream-defun t- (time)
  "Change the current p2 by substracting TIME, 
a number or a key in `pfstream-time-markers'"
  (now-be (- (current-pfield 2) (cseel-actual-time time)))) 

(pfstream-defun now-> (key)
  "Associate KEY to current p2 in `pfstream-time-markers'"
  (setq pfstream-time-markers (plist-put pfstream-time-markers key (current-pfield 2))))

(pfstream-defun then-> (key)
  "Associate KEY to current p2+p3 in `pfstream-time-markers'"
  (setq pfstream-time-markers (plist-put pfstream-time-markers key (+ (current-pfield 2) (current-pfield 3)))))

(pfstream-defun now-be (time)
  "Set current p2 to TIME, a number or a key in `pfstream-time-markers'"
  (let ((ct (cseel-actual-time time)))
    (set-pfield 2 ct)
    (setq pfstream-last-p3 0 ;; so that +i behaves properly
	  pfstream-last-p2 ct)))

(defun cseel-actual-time (time)
  (if (numberp time)
      time
    (plist-get pfstream-time-markers time)))

(defmacro save-stream-insert (&rest body)
  "Create a new p-field stream with all p-fields values, time 
markers and custom keys inherited from the current stream. 
Once BODY is evaluated, everything gets restored except the current 
time which is set to the end time of the exited stream."
  (declare (indent 0))
  `(let (save-stream-insert:::return-value)
     (now-be
      (with-pfields-stream () 
	(setq save-stream-insert:::return-value
	      (progn ,@body))
	(end-time)))
     save-stream-insert:::return-value))

;; deprecated:
(pfstream-defalias 'now-is 'pfstream:now->)
(pfstream-defalias 'then-is 'pfstream:then->)
(pfstream-defalias 't-set 'pfstream:now-be)


;; ===================================================================
;;                    misc. utilities
;; ===================================================================

(pfstream-defun // (n d &optional root)
  "Return the ratio ROOT*N/D as a float.
When ROOT is nil, it defaults to 1"
  (* (or root 1) (/ (float n) d)))

(pfstream-defun section ()
  "Insert a section statement"
  (insert "s\n"))

(pfstream-defun end-of-section ()
  "Insert an end of section statement"
  (insert "e\n"))

(pfstream-defun defcycler (list)
  "Returns a function iterating over LIST elements
in a cyclic manner."
  (lexical-let ((list list)
		(index -1))
    (lambda ()
      (setq index 
	    (mod (+ 1 index) 
		 (length list)))
      (nth index list))))

;; ===================================================================
;;                    commenting
;; ===================================================================

(pfstream-defun end-note (&rest str)
  "Insert STR as a comment at the end of the previous line"
  (backward-char 1)
  (insert "\t; " (format-for-note str) "\n")) 

(pfstream-defun note (&rest str)
  "Insert STR as a comment line"
 (insert "; " (format-for-note str) "\n"))

(pfstream-defun NOTE (&rest str)
  "Insert /* STR */ with empty lines around it"
  (insert "\n/* " (format-for-note str) " */\n\n"))

(defun format-for-note (str)
  (if (stringp (car str))
      (apply 'format str)
    (mapconcat 'prin1-to-string str nil)))


;; ===================================================================
;;                          scores and score lines
;; ===================================================================
;; a "score line" is a list or symbol which, when evaluated, insert an
;; i-statement at point.
;;
;; a "score" is a list of score lines
;;
;; example of score:
;;  '((i 1 0 5) 
;;    (+i :amp 200)
;;    a
;;    ,bo2d25v90
;;    (|i :vol 63)
;;    (c+ :time (seconds-to-clicks 2.5))
;;   )

(defmacro along-score (spec &rest body)      
  "SPEC is a list \(VAR-SYM SCORE [INDEX-SYM] [SCORE-SYM])
evaluate BODY with VAR taking in turn all note values in SCORE \(a list)
the macro optionaly binds one or two symbols in the scope of BODY: 
INDEX-SYM is the position of VAR in SCORE \(a number starting at 0)
SCORE-SYM is bound to SCORE
thus VAR is always equal to \(nth <INDEX-SYM> <SCORE-SYM>)"
  (declare (indent 1))
  (let ((index-sym (make-symbol "index"))
	(score-sym (make-symbol "score")))
    `(let ((,index-sym 0)
	   (,score-sym ,(cadr spec)))
       (when (quote ,(cadddr spec))
	 (setq ,(cadddr spec) ,score-sym))
       (dolist (,(car spec) ,score-sym)
	 (if (symbolp ,(car spec))
	     (if (fboundp ,(car spec))
		 (setq ,(car spec) (list ,(car spec)))
	       (when (require 'k nil t)
		 (setq ,(car spec) 
		       (normalize-keykit-note (symbol-name ,(car spec)))))))
	 (when (quote ,(caddr spec))
	   (setq ,(caddr spec) ,index-sym))
	 ,@body
	 (incf ,index-sym)))))

(defalias 'i> 'score-line)

(defmacro score-line (event &rest plist)
  "Modify EVENT \(a score-line) with PLIST, then evaluate the result.
Thus \(score-line '\(+i amp: 200) :dur 1) is like \(+i :amp 200 :dur 1)
Note that \(score-line 'a :vol 127) or \(score-line '(i 1 0 2 500))
are also legal syntaxes"
  (let ((eevent (make-symbol "n-")))
    `(let ((,eevent (cseel-expand-generic-statement ,event)))
      (eval (append (list (car ,eevent)) (quote ,plist) (cdr ,eevent))))))

(defun cseel-expand-generic-statement (ilist)
  (if (eq 'i (car ilist))
      (append '(|i) (expand-i-statement (cdr ilist)))
    (if (and (not (fboundp (car ilist)))
	     (require 'k nil t))
	(normalize-keykit-note (symbol-name (car ilist)))
      ilist)))

;TEST (cseel-expand-generic-statement '(|i :blouerg (+ 27 15))) => '(|i :blouerg (+ 27 15))
;TEST (cseel-expand-generic-statement '(i 1 0 5)) => '(|i :instr 1 :start 0 :duration 5)
;TEST (cseel-expand-generic-statement '(ao5d50)) => '(a :dur 50)

(defun score-line-pfield (event pf)  
  "Return the value for pfield PF \(a number or a key) as associated 
in EVENT \(an i-statement)
Return nil if PF is not explicitely defined in EVENT
Custom keywords are not expanded; PF may be a custom keyword"
  (when (listp event)
    (plist-get (cdr (cseel-expand-generic-statement event))
	       (if (symbolp pf) pf (pfstream-key-at pf)))))

;TEST  (score-line-pfield '(i 1 0 5 200) 4)       => 200
;TEST  (save-stream (score-line-pfield '(i 1 0 :p3 5) :p3))       => 5
;TEST  (score-line-pfield '(+i :amp 25) :amp)     => 25
;TEST  (score-line-pfield '(+i :start 5) 2)       => 5
;TEST  (score-line-pfield '(+i :start (+ 2 2)) 2) => '(+ 2 2)
;TEST  (score-line-pfield '(+i :env '(0 1 1 1 0)) :env) => '(quote (0 1 1 1 0))


(defun pfield (event pf) 
  "Return the value for pfield PF \(a number or a key) in EVENT 
\(an i-statement)
If PF is not explicitely defined, check if it appears when 
expanding the custom keys
If PF can still not be found in EVENT, return its current value 
in the stream 
PF must not be a custom keyword"
  (or 
   ;; pf is an actual key or pfield in event
   (eval (score-line-pfield event pf))       
   ;; or we get pf when expanding the keys
   (when (listp event)
     (setq event (cseel-expand-generic-statement event))
     (eval (score-line-pfield (cons (car event)
				    (cseel-expand-keys-in (cdr event)))
			      pf)))
   ;; else fetch the current value for pf in the stream
   (current-pfield pf)))

;TEST (pfield '(i 1 0 5 200) :start)       => 0
;TEST (save-stream (pfield '(i 1 0 :p3 5) :duration))       => 5
;TEST (pfield '(i 1 0 5 :vol 200) :vol)       =>  200
;TEST (pfield '(+i :amp 25) :amp)     => 25
;TEST (pfield '(+i :start 5) 2)       => 5
;TEST (pfield '(+i :start (+ 2 2)) 2) => 4


(defun score-line-remove (event tag)    
 ;;;; a revoir pour accepter n'importe quel event !?
  (when (listp event)
    (cl-do-remf (cdr event) tag)))


;; ===================================================================
;;                    reading parts of the actual score
;; ===================================================================

;; private:

(defun scomx-read-line (&optional with-following-lines)
  (save-excursion
    (beginning-of-line)
    (let ((raw (delete-if (lambda (pf) (member pf '(";" "i" "/*")))
			  (split-string
			   (buffer-substring-no-properties 
			    (point)
			    (re-search-forward "\\(;\\|$\\|\\/\\*\\)"))))))
      (if (and raw
	       (string-match "^i" (car raw)))
	  (setf (car raw) (substring (car raw) 1)))
      (if (null with-following-lines)
	  raw
	(forward-line 1)
	(while (and (null (scomx-i-statement-p))
		    (null (looking-at "[^ \t0-9.-]")))
	  (setq raw (append raw (scomx-read-line)))
	  (forward-line 1))
	raw))))

(defun scomx-filter-raw-line (raw)
  (append '(i) (mapcar (lambda (pf) (if (string-match "[0-9.-]" pf) (string-to-number pf) pf)) raw)))


;; public:

(defun scomx-read-line-at-point ()
  "Read the score line at point"
  (scomx-filter-raw-line (scomx-read-line t)))

(defun scomx-read-region (beg end)
  "Read the score lines around region"
  (save-excursion
    (goto-char beg)
    (let ((m nil))
      (while (< (point) end)
	(if (scomx-i-statement-p)
	    (setq m (append m (list (scomx-read-line-at-point)))))
	(forward-line 1))
      m)))

(defun scomx-read-at-row (row &optional sid)
  "Read the score line at ROW in selection SID"
  (save-excursion
    (scomx-goto-selection-row row (or sid ""))
    (scomx-read-line-at-point)))

(defun scomx-read-around-matrix (&optional sid)
  "Read the score lines around selection SID"
  (setq sid (or sid ""))
  (let ((m nil))
    (dotimes (row (scomx-count-i-statements (scomx-selection-beginning sid)
					    (scomx-selection-end sid)))
      (setq m (append m (list (scomx-read-at-row (1+ row) sid)))))
;      (nconc m (list (scomx-read-line (1+ row) sid))))                   ;???
    m))


;; === this is it.
(provide 'i)

;; i.el ends here




