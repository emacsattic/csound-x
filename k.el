;;; -*- auto-recompile: t -*-

;;; k.el --- extension of i.el for Keykit integration

;; Keywords: csound

;; This file is not part of GNU Emacs. 
;; 
;; k.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; k.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;; last modified March 14, 2008

(require 'cl)
(require 'i nil t)
(require 'csound-key nil t)
(require 'keykit-mode nil t)


;; ===================================================================
;;                k.el       keykit streams for i.el
;; ===================================================================

(defvar keykit-pitch-symbols
  '(("a" 69) ("b-" 70) ("b" 71) ("c" 60) ("c+" 61) ("d" 62) ("e-" 63) ("e" 64) ("f" 65) ("f+" 66) ("g" 67) ("a-" 68))
  "The symbols used by Keykit to represent the pitches at octave 3, and their corresponding MIDI numbers")

(defvar extended-keykit-pitch-symbols
  (let ((ekps keykit-pitch-symbols))
    (dolist (symbpair keykit-pitch-symbols ekps)
      (dotimes (octave 12)
	(let ((o (- octave 3)))
	  (setq ekps (append ekps (list (list (concat (car symbpair) (format "o%d" o))
					      (+ (cadr symbpair) (* 12 (- o 3)))
					      (* 12 (- o 3))))))))))
  "A development of `keykit-pitch-symbols' covering any possible MIDI number from 0 to 127")

;; defining all pitch-symbols functions (268 of them !):

(dolist (symbpair extended-keykit-pitch-symbols)
  (when (= 3 (length symbpair))
    (setf (symbol-function (intern (concat "," (car symbpair))))
	  `(lambda (&rest args)
	     (apply '+i (append args '(:pit ,(cadr symbpair))))
	     (setq kstream-current-octave ,(caddr symbpair))))
    (setf (symbol-function (intern (car symbpair)))
	  `(lambda (&rest args)
	     (apply '|i (append args '(:pit ,(cadr symbpair))))
	     (setq kstream-current-octave ,(caddr symbpair)))))
  (when (= 2 (length symbpair))
    (setf (symbol-function (intern (concat "," (car symbpair))))
	  `(lambda (&rest args)
	     (apply '+i (append args (list :pit (+ ,(cadr symbpair) kstream-current-octave))))))
    (setf (symbol-function (intern (car symbpair)))
	  `(lambda (&rest args)
	     (apply '|i (append args (list :pit (+ ,(cadr symbpair) kstream-current-octave))))))))


(dotimes (midi 128)
  (setf (symbol-function (intern (format ",p%d" midi)))
	`(lambda (&rest args)
	   (apply '+i (append args (list :pit ,midi))))) 
  (setf (symbol-function (intern (format "p%d" midi)))
	`(lambda (&rest args)
	   (apply '|i (append args (list :pit ,midi)))))) 


;; ===================================================================
;;                              internal (private)
;; ===================================================================

(defun cskey-attribute (lett str)
  (if (string-match lett str)
      (string-to-number (car (last (split-string str lett t))))))

(defun normalize-keykit-note (note)
  "Transform NOTE, a string, into its canonic form, to be evaluated within a keykit stream"
  (when (string-match "^p" note) (setq note (concat "|i" note)))
  (when (string-match "^,p" note) (setq note (concat "+i" note)))
  (let* ((no-error (string-match "^\\([+|,]*.[+-]*\\)\\(.*\\)" note))
	 (lnote (list (intern (match-string 1 note))))
	 (attributes (match-string 2 note)))
    (dolist (lk '(("p" :pit) ("c" :ch) ("v" :vol) ("t" :time) ("d" :dur)) lnote)
      (when (string-match (concat (car lk) "\\([0-9]+\\)") attributes)
	(setq lnote
	      (append lnote
		   (list (cadr lk) (string-to-number (match-string 1 attributes)))))))))

;TEST (normalize-keykit-note "f+v100t56") => '(f+ :vol 100 :time 56)
;TEST (normalize-keykit-note ",f+v100t56") => '(,f+ :vol 100 :time 56)
;TEST (normalize-keykit-note ",p28v100t56") => '(+i :pit 28 :vol 100 :time 56)
;TEST (normalize-keykit-note "p28v100t56") => '(|i :pit 28 :vol 100 :time 56)


;; ===================================================================
;;                       fundamental macros
;; ===================================================================

(defvar kdef:pit '(:p4 'midi-to-frequency))   
(defvar kdef:vol '(:p5 (lambda (v) (midi-to-range v 0 10000))))
(defmacro with-keykit-stream (&rest body) 
  "Define a pfield-stream adapted to the transcription of Keykit phrases into a score"
  (declare (indent 0))
  `(let ((kstream-current-octave 0))
     (with-pfields-stream-sized 5             
      (add-custom-keys '(;(:pit ,kdef:pit) 
			 ;(:vol ,kdef:vol)
			 (:ch (:instr 'identity))
			 (:time (:start 'clicks-to-seconds))
			 (:dur (:duration 'clicks-to-seconds))))
      (set-in-stream :ch 1 :time 0 :dur 96)    
      (kstream:pit '(,kdef:pit))  
      (kstream:vol '(,kdef:vol))
      ,@body)))

(defun kstream:vol (sexp)
  (add-custom-key (list :vol sexp))
  (set-in-stream :vol 63))

(defun kstream:pit (sexp)
  (add-custom-key (list :pit sexp))
  (set-in-stream :pit 69))

;; ===================================================================
;;                              utilities
;; ===================================================================

(defun midi-to-oct (midi)
  (let ((octave (+ (/ midi 12) 3)))
    (+ octave
       (/ (- midi (* 12 (- octave 3))) 12.0))))

(defun midi-to-pch (midi)
  (let ((octave (+ (/ midi 12) 3)))
    (+ octave
       (/ (- midi (* 12 (- octave 3))) 100.0))))

(defun midi-to-frequency (midi)
  (* 440 (exp (/ (* (log 2) (- midi 69)) 12.0))))

(defun midi-to-range (midi min max)
  (+ min (* (/ midi 127.0) (- max min))))

(defun clicks-to-seconds (clicks)
  (/ clicks 192.0))

(defun seconds-to-clicks (secs)
  (round (* secs 192)))


;; ===================================================================
;;                  high-level internal functions
;; ===================================================================

(defun KeyPhrase-to-kscore (keyphrase-name)
  (keyphrase-to-kscore (cscsd-get-KeyPhrase keyphrase-name)))

(defun keyphrase-to-kscore (str)
  (let (phrase
	(kstream-current-octave 3))
    (dolist (notes (delete-if (lambda (s) (string-match "^l" s))
			      (split-string str "[',]" t))
		   phrase)
      (let ((comma t))
	(dolist (s (split-string notes " " t))
	  (let* ((pitch (progn (string-match "^.[+-]*" s) 
			       (match-string 0 s)))
		 (attributes (substring s 1))
		 (octave (or (cskey-attribute "o" attributes)
			     kstream-current-octave))
		 (duration (cskey-attribute "d" attributes))
		 (volume (cskey-attribute "v" attributes))
		 (channel (cskey-attribute "c" attributes))
		 (time (cskey-attribute "t" attributes)) 
		 (midipitch (+ (* 12 (- octave 3))
			       (cadr (assoc pitch keykit-pitch-symbols))))
		 (note (append (list (if comma '+i '|i) :pit midipitch)
			       (when duration (list :dur duration))
			       (when volume (list :vol volume))
			       (when channel (list :ch channel))
			       (when time (list :time time)))))
	    (setq phrase (append phrase (list note)))
	    (setq kstream-current-octave octave)
	    (setq comma nil)))))))

(defun lowkey-exp-to-string (str)
  (delete ?\n (lowkey-eval (format "print(%s)" str))))

(defun lowkey-exp-to-kscore (str)
  (keyphrase-to-kscore (lowkey-exp-to-string str)))


;; ===================================================================
;;                  high-level public functions & macros
;; ===================================================================

(defmacro along-keyphrase (spec &rest body)
  "SPEC is a list \(VAR KEYPHRASE [INDEX-SYM] [SCORE-SYM])
evaluate BODY with VAR taking in turn all note values in KEYPHRASE \(a Keykit expression)
the macro optionaly binds one or two symbols in the scope of BODY: 
'INDEX-SYM is the order position of VAR in KEYPHRASE (starting at 0)
'SCORE-SYM is the computed list of all notes in KEYPHRASE
thus VAR is always equal to \(nth <INDEX-SYM> <SCORE-SYM>)"
  (declare (indent 1))
  (let ((keyphrase (make-symbol "keyphrase"))
	(score (make-symbol "score")))
  `(let* ((,keyphrase ,(cadr spec))
	  (,score (if (lowkey-one-phrase-code-p ,keyphrase)
		     (keyphrase-to-kscore ,keyphrase)
		   (lowkey-exp-to-kscore ,keyphrase))))
     (along-score (,(car spec) ,score ,(caddr spec) ,(cadddr spec))
		  ,@body))))

;(along-keyphrase (event "'a,b,c'") (eval event))

(defmacro along-KeyPhrase (spec &rest body)
  (declare (indent 1))
  `(along-keyphrase 
       (,(car spec) (quote ,(cscsd-get-KeyPhrase (eval (cadr spec)))) ,(caddr spec) ,(cadddr spec))
     ,@body))

(defmacro along-MIDIfile (spec &rest body)
  (declare (indent 1))
  `(along-keyphrase
       (,(car spec) (quote ,(cscsd-get-MIDIfile (eval (cadr spec)))) ,(caddr spec) ,(cadddr spec))
     ,@body))

;(along-MIDIfile (event) (eval event))

(defmacro along-mphrase (spec &rest body)
  (declare (indent 1))
  `(along-keyphrase 
       (,(car spec) (quote ,(cscsd-get-mphrase (eval (cadr spec)))) ,(caddr spec) ,(cadddr spec))
     ,@body))


;; ===================================================================
;;                           tests & examples
;; ===================================================================

; à ranger et exploiter correctement !!

(defun kel-example ()
  (insert ?\n)
  (with-keykit-stream
    (along-score (e '(a ec10 (c+ :time 1000)  ao5t1920))
      (i> e :dur 200))))

(defun kel-example-2 ()
  (with-keykit-stream
    (along-score (n '((i 1 0 5)
		      (+i :amp 200)
		      a
		      ,bd25
		      (|i :vol 63)
		      (c+ :time (seconds-to-clicks 2.5))
		      ))
      (i> n)
      (end-note n))))


(defun kel-simple-example ()
  (insert ?\n)
  (with-keykit-stream
    (note pfstream-pf-attributes)
    (note pfstream-custom-pf-keys)
    (|i :p4 500)
    (ao5)
    (f+)
    (,a- :ch 5 :dur 150)
    (p69 :vol 45)
    (,p70)))

(defun kel-less-simple-example () 
  (insert ?\n)
  (let ((kdef:pit '(:p5 'midi-to-oct))
	(kdef:vol '(:p4 (lambda (v) (midi-to-range v 0 1)))))
    (with-keykit-stream
      (add-custom-keys '((:vol '(:p4 (lambda (v) (midi-to-range v 1000 3000))))
			 (:ch (:instr '1+))))
      (NOTE "part 1")
      (along-score (n '(a ec10 ,cc3v127 (|i :dur 200) (a :vol 80) (c+ :time 1000) ao5t1920))
	(score-line n :p4 (+ (pfield n :p4) 10)))
      (NOTE "part 2")
      (along-keyphrase (n "'f+ d ,g'")
	(score-line n))
      (NOTE "part 3")
      (with-keykit-stream 
	(along-keyphrase (n "fractal('f+ d ,g')")
	  (score-line n)))

)))


; (with-temp-buffer-to-string (kel-simple-example))

; (with-temp-buffer-to-string (kel-less-simple-example))



;; === this is it.
(provide 'k)

;; k.el ends here




