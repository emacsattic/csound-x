;;; -*- auto-recompile: t -*-

;;; csound-lsp.el --- Csound composition in Emacs Lisp

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-lsp.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-lsp.el is distributed in the hope that it will be useful,
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
;;; Installation:
;;                  
;; ==========================================================
;; this file should be installed through the csound-x package
;; ==========================================================
;;

;; last modified June 25, 2009

;;; Code:

(require 'cl)
(require 'csound-csd)
(require 'csound-orc)

(defgroup csound-lsp nil
  "Emacs Lisp for Csound composition"
  :group 'csound-x
  :prefix "csl-")

(defcustom csl-sr 44100
  "Default sample rate"
  :type 'integer
  :group 'csound-lsp)

(defcustom csl-kr 4410
  "Default control rate"
  :type 'integer
  :group 'csound-lsp)

(defcustom csl-nchnls 1
  "Default number of ouput channels"
  :type 'integer
  :group 'csound-lsp)

(defcustom csl-variable-output-opcodes '("soundin" "diskin" "diskin2")
  "Opcodes known to have a variable number of outputs"
  ;; find a way to generate this list automatically ! 
  ;; see csound-doc.el
  :type '(repeat string)
  :group 'csound-lsp)


;; ===================================================================
;;                   formatting utilities
;; ===================================================================

(defmacro with-csd-template (&rest body)
  (declare (indent 0))
  `(progn
     (insert "<CsoundSynthesizer>\n")
     ,@body
     (insert "\n</CsoundSynthesizer>\n")))

(defmacro with-csl-options-template (&rest body)
  (declare (indent 0))
  `(progn
     (insert "\n<CsOptions>\n")
     ,@body
     (insert "\n</CsOptions>\n")))

(defmacro with-csl-score-template (&rest body)
  (declare (indent 0))
  `(progn
     (insert "\n<CsScore>\n")
     ,@body
     (insert "\n</CsScore>\n")))

(defmacro with-csl-orchestra-template (&rest body)
  (declare (indent 0))
  `(progn
     (setq csl-varname-counter 1000) 
     (insert (format "\n<CsInstruments>
sr     = %s
kr     = %s
ksmps  = %s
nchnls = %s

" csl-sr csl-kr (/ csl-sr csl-kr) csl-nchnls))
     ,@body
     (insert "\n</CsInstruments>\n")))

(defvar csl-varname-counter 1000)
(make-variable-buffer-local 'csl-varname-counter)

(defun csl-new-varname (&optional opcode)
  (format "%s%s" (if opcode (substring (concat opcode "vv") 0 3)
                   "var")
          (incf csl-varname-counter)))

;TEST (let ((csl-varname-counter 0)) (csl-new-varname "linen")) => "lin1"
;TEST (let ((csl-varname-counter 0)) (csl-new-varname "oscili")) => "osc1"
;TEST (let ((csl-varname-counter 0)) (csl-new-varname "in")) => "inv1"


;; ===================================================================
;;               orchestra code transcription engine
;; ===================================================================

(defun csl-transcribe-to-code (sexp)
  "Transcribe the specification SEXP \(a list or vector) into orchestra
statements in the usual Csound syntax.

If SEXP is a vector, it is converted to a list. 
SEXP valid formats are as follow:

\(OPCODE [:global] [:xrate] ARGS)
where OPCODE is a Csound opcode and ARGS its arguments;
any argument can itself be a specification SEXP.
the opcode output variables are automatically generated.
when :global is present, the output variables are global.
:xrate is either :arate, :krate or :irate and gives the type
for the output variables.

\(OPCODE \(:output OUTVARS) ARGS)
in which case the opcode output variables are direcly given
ex: \(oscili \(:output aosc) p4 1/p3 p7)

\(:code STRING) 
for litteral inclusion of STRING as a code line

\(::MACRO BODY) 
where BODY structure depends on ::MACRO
this is documented in the associated function csl-expand::MACRO
for example if ::MACRO is ::add or ::cond, the corresponding
functions are `csl-expand::add' and `csl-expand::cond'
"
  (setq sexp (append sexp ())) ;; vector->list
  ;; ::macros expansion
  (when (and (keywordp (first sexp))
             (string-match "^::" (symbol-name (first sexp))))
    (setq sexp 
          (funcall 
           (symbol-function (intern (format "csl-expand%s" (first sexp))))
           sexp)))
  ;;
  (if (listp (first sexp)) ;; jump to sub-level if required
      (loop for s in sexp
            for o = (csl-transcribe-to-code s)
            append (cadr o) into code-lines
            finally return (list (car o) code-lines))
    ;;
    (let* ((opcode (symbol-name (first sexp)))
           output opargs code-lines globalp)
      ;;
      ;; litteral inclusion
      (if (eq :code (first sexp))
          (setq code-lines (list (apply 'concat (cdr sexp))))
	;;
	;; regular parsing
	(dolist (arg (cdr sexp))
	  (cond
	   ;; single keyword 
	   ((keywordp arg)
	    (cond
	     ((eq arg :global) (setq globalp t))
	     ((memq arg '(:arate :krate :irate))
	      (setq output (csl-output-for opcode arg nil globalp)))
	     (t (error "unrecognized keyword: %s" arg))))
	   ;; list
	   ((listp arg)
	    (case (first arg)
	      ;; output spec (:output k1 a2 ...)
	      (:output (setq output (mapcar 'symbol-name (cdr arg))))
	      ;; else another sexp to expand
	      (t (let ((rec (csl-transcribe-to-code arg)))
		   (setq opargs (append opargs (car rec))
			 code-lines (append code-lines (cadr rec)))))))
	     ;; anything else is considered an opcode argument
	   (t (add-to-list 'opargs (format "%S" arg) t))))
        (setq output (or output
                         (csl-output-for opcode nil opargs globalp)))
        (add-to-list
	 'code-lines
	 (if (member opcode csdoc-functions)
	     (format "%s = %s" 
		     (car output)
		     (if (null (cdr opargs))
			 (concat opcode "(" (first opargs) ")")
		       (mapconcat 'identity opargs opcode)))
	   (format "%s %s %s"
		   (mapconcat 'identity output ", ") 
		   opcode
		   ;; to do: handle possible mismatch in input format:	      
		   (mapconcat 'identity
			      (butlast opargs 
				       (- (length opargs)
					  (length (csdoc-opcode :in opcode))))
			      ", "))) 
	 t))
      (list output code-lines))))

(defun csl-debug-ttc (sexp)
  (let ((csl-varname-counter 0)) 
    (csl-transcribe-to-code sexp)))

;TEST (csl-debug-ttc '(out (oscili :arate (linen :krate p4 p6 p3 p7) p5 p8)))) => '(nil ("klin2 linen p4, p6, p3, p7" "aosc1 oscili klin2, p5, p8" " out aosc1"))

(defun csl-output-for (opcode &optional rate opargs globalp)
  (cond
   ((member opcode csl-variable-output-opcodes)
    (error "unspecified outputs for %s" opcode))
   ((not (member opcode csdoc-opcodes))
    (if (yes-or-no-p (format "Unknown opcode %s. Proceed with warning ? "
			     opcode))
        (list (format ";WARNING: variables missing (unknown opcode %s)\n"
		      opcode))
      (error "Unknown opcode %s" opcode)))
   ((member opcode csdoc-functions)
    (list (concat
           (when globalp "g")
           (if (member opcode '("a" "k" "i"))
               opcode
             (if (null (cdr opargs))
                 (substring (car opargs) 0 1)
               (let ((spec (mapcar (lambda (v) (elt (remove ?g v) 0)) opargs)))
                 (cond ((member ?a spec) "a")
                       ((member ?k spec) "k")
                       (t "i")))))
           (csl-new-varname))))
   ((member opcode csdoc-0-opcodes)
    ())
   (t (let ((spec (substitute 
		   ?k ?x ;; should the default be ?a or ?k ?
		   (car (split-string (csdoc-opcode :spec opcode)))))
            (output))
        (dotimes (o (length spec))
          (add-to-list 'output
                       (concat (when globalp "g")
                               (string (if rate
                                           (elt (symbol-name rate) 1)
                                         (elt spec o)))
                               (csl-new-varname opcode))
                       t))
        output))))

;TEST (elt (car (csl-output-for "linen" :arate)) 0) => 97
;TEST (elt (car (csl-output-for "linen" :krate)) 0) => 107
;TEST (elt (car (csl-output-for "linen")) 0) => 107
;TEST (elt (car (csl-output-for "+" nil '("ahoui" "kbof"))) 0) => 97
;TEST (elt (car (csl-output-for "+" nil '("gahoui" "kbof"))) 0) => 97
;TEST (elt (car (csl-output-for "+" nil '("khoui" "kjhu" "kbof"))) 0) => 107
;TEST (elt (car (csl-output-for "+" nil '("khoui" "kjhu" "abof"))) 0) => 97
;TEST (elt (car (csl-output-for "a" nil '("khoui" "kjhu" "ibof"))) 0) => 97


;; ===================================================================
;;                 orchestra code transcription macros
;; ===================================================================

(defun csl-expand::cond (arg)
  "Usage: \(::cond \(TEST SEXP [SEXP ...]) [\(TEST SEXP [SEXP ...]) ...])
"
  (loop for s in (nthcdr 1 arg)
        with exps = nil
        if (null exps)
        collect `(:code ,(format "if %s then" (car s))) into exps
        else
        collect `(:code ,(format "elseif %s then" (car s))) into exps
        append `(,@(cdr s)) into exps
        finally return (append exps '((:code "endif")))))

;TEST (csl-debug-ttc '(::cond ((p4 < 20) (oscili (:output aosc1) p4 1/p3 p7) (oscili (:output aosc) p4 1/p3 p7)) ((5 == 2) (oscili (:output kbof) 1 2 3)))))  => '(nil ("if (p4 < 20) then" "aosc1 oscili p4, 1/p3, p7" "aosc oscili p4, 1/p3, p7" "elseif (5 == 2) then" "kbof oscili 1, 2, 3" "endif"))

;TEST (csl-debug-ttc [::cond ((p4 < 20) [oscili (:output aosc1) p4 1/p3 p7] [oscili (:output aosc) p4 1/p3 p7]) ((5 == 2) [oscili (:output kbof) 1 2 3])]))  => '(nil ("if (p4 < 20) then" "aosc1 oscili p4, 1/p3, p7" "aosc oscili p4, 1/p3, p7" "elseif (5 == 2) then" "kbof oscili 1, 2, 3" "endif"))

(defun csl-expand::if (arg)
  "Usage: \(::if TEST SEXP [SEXP ...])
"
  (append `((:code ,(format "if %s then" (second arg))))
          (nthcdr 2 arg)
          `((:code "endif"))))

;TEST (csl-debug-ttc '((::if (p4 < 2000) (oscili (:output aosc1) p4 1/p3 p7) (oscili (:output aosc) p4 1/p3 p7)) (out aosc))) => '(nil ("if (p4 < 2000) then" "aosc1 oscili p4, 1/p3, p7" "aosc oscili p4, 1/p3, p7" "endif" " out aosc"))

(defun csl-expand::loop (arg)
  "Usage: \(::loop LOOP-CLAUSES SEXP-TEMPLATE)
where SEXP-TEMPLATE is a quoted sexp or vector.
...see `loop' for valid loop clauses
"
  (mapcar
   (lambda (s) (append s ()))
   (eval `(loop ,@(subseq arg 1 -1)
               collect ,@(last arg)))))

;TEST (csl-debug-ttc '(::loop for i upto 5 `(init (:output ,(intern (format "ga%s" i))) 0))) => '(("ga5") ("ga0 init 0" "ga1 init 0" "ga2 init 0" "ga3 init 0" "ga4 init 0" "ga5 init 0"))

;TEST (csl-debug-ttc [::loop for i upto 5 `[init (:output ,(intern (format "ga%s" i))) 0]]) => '(("ga5") ("ga0 init 0" "ga1 init 0" "ga2 init 0" "ga3 init 0" "ga4 init 0" "ga5 init 0"))

;TEST (csl-debug-ttc [::loop for i upto 5 `[init (:output ,(intern (format "ga%s" i))) ,(* 10 i)]]) => '(("ga5") ("ga0 init 0" "ga1 init 10" "ga2 init 20" "ga3 init 30" "ga4 init 40" "ga5 init 50"))

(defun csl-expand::add (arg)
  "Usage: \(::add LOOP-CLAUSES SEXP-TEMPLATE)
where SEXP-TEMPLATE is a quoted or backquoted sexp 
...see `loop' for valid loop clauses

example:

   \(::add for amp in '\(p4 2*p4 p4/2)
          `\(oscili :krate ,amp 1/p3 p7))

is equivalent to:

   \(+ \(oscili :krate p4 1/p3 p7)
      \(oscili :krate 2*p4 1/p3 p7)
      \(oscili :krate p4/2 1/p3 p7))
"
  (append (list '+)
	  (csl-expand::loop arg)))

;TEST (csl-debug-ttc '(::add for amp in '(p4 p4/2) for freq in `(1/p3 ,(/ 6 2)) `(oscili :krate ,amp ,freq p7))) => '(("kvar3") ("kosc1 oscili p4, 1/p3, p7" "kosc2 oscili p4/2, 3, p7" "kvar3 = kosc1+kosc2"))

;TEST (csl-debug-ttc '(::add for amp in '(p4 p4/2) for freq in `(1/p3 ,(/ 6 2)) `[oscili :krate ,amp ,freq p7])) => '(("kvar3") ("kosc1 oscili p4, 1/p3, p7" "kosc2 oscili p4/2, 3, p7" "kvar3 = kosc1+kosc2"))

;TEST (csl-debug-ttc [::add for amp in '(p4 p4/2) for freq in `(1/p3 ,(/ 6 2)) `[oscili :krate ,amp ,freq p7]]) => '(("kvar3") ("kosc1 oscili p4, 1/p3, p7" "kosc2 oscili p4/2, 3, p7" "kvar3 = kosc1+kosc2"))


;; ===================================================================
;;            the 'defcsound and 'csound-composition macros
;; ===================================================================


(defun csl-insert-code (quoted-sexp)
  (let ((code (csl-transcribe-to-code quoted-sexp)))
    (dolist (line (cadr code))
      (insert line "\n"))
    (car code)))

(defun csl-insert-instrument (name &rest body)
  (declare (indent 1))
  (insert (format "\ninstr %s\n" name))
  (dolist (code body)
    (csl-insert-code code))
  (insert "endin\n"))

(defun csl-insert-opcode (name &rest body)
  (declare (indent 1))
  (insert (concat "\nopcode " name "\n"))
  (dolist (code body)
    (csl-insert-code code))
  (insert "endop\n"))

(defmacro defCSD (name parameters &rest args)
  "Define a function NAME accepting PARAMETERS which, as a side-effect,
inserts at point a full CSD described by ARGS. This is the main macro for 
describing a Csound composition in Emacs Lisp.
See Info node `(csound-x)Csound Elisp'.

PARAMETERS allows full Common Lisp conventions, as in `defun*'.

ARGS structure is \(KEY BODY [KEY BODY] ...) where KEY is a keyword
while BODY does not include a keyword at top-level. A same KEY can appear 
any number of time; no KEY is mandatory.

In the following HEAD refers to the first form in BODY and TAIL
refers to the list of all other forms, nil if BODY is a single form.
So-called \"orchestra code specifications\" are either quoted lists or
vectors \(see `csl-transcribe-to-code' argument format for details).

Supported KEYs are:

:sr, :kr:, :ksmps, :nchnls 
  set the corresponding global to HEAD. this is not mandatory.
  \(defaut values come from variables `csl-sr', `csl-kr' and `csl-nchnls')

:globals
  insert initialisation statements in orchestra, before instruments and 
opcode definitions. 
  BODY forms are orchestra code specifications.

:ftables
  use all forms in BODY as ftable specifications. 
  see `csl-insert-ftable' for details.

:instr 
  insert an instrument definition in the orchestra
  HEAD is the instrument number, TAIL are orchestra code specifications.

:opcode 
  insert an opcode definition in the orchestra
  HEAD is the opcode specification \(a string), TAIL are orchestra code specifications.

:orchestra
  evaluate BODY within the orchestra section of the CSD, among instruments
and opcodes definitions. BODY forms are plain lisp code.

:score 
  similar to :orchestra, only evaluate BODY within the CSD score section.  

:i-stream 
  same as :score but BODY is wrapped in `with-pfields-stream'.

:options
 set the CSD options section \(i.e. csound command line flags)
 BODY forms are converted to string and concatenated."
  (declare (indent defun))
  (let (options comment
        cscompo-orc-globals
        cscompo-orc-code
        cscompo-sco-header
        cscompo-sco-statements
        key body head tail)
    (dolist (chunk 
             (loop for form in args
                   for fplace upfrom 1
                   if (keywordp form)
                   collect `(,form ,@(loop for f in (nthcdr fplace args)
                                           until (keywordp f) 
                                           collect f))))
      (setq key (car chunk)
	    body (cdr chunk)
            head (car body)
            tail (cdr body))
      (case key
	(:about
	 (setq comment head))
        (:ftables     
	 (dolist (fspec body)
	   (push `(csl-insert-ftable ,@fspec)
		 cscompo-sco-header)))
        (:ksmps
         (push `(csound-set-ksmps ,head) cscompo-orc-code))
        (:kr
         (push `(csound-set-kr ,head) cscompo-orc-code ))
        (:sr
         (push `(csound-set-sr ,head) cscompo-orc-code ))
        (:nchnls
         (push `(csound-set-nchnls ,head) cscompo-orc-code ))
        (:instr
         (push `(csl-insert-instrument ,head ,@tail) cscompo-orc-code ))
        (:opcode 
	 (push `(csl-insert-opcode ,head ,@tail) cscompo-orc-code ))
        (:orchestra
	 (dolist (code body)
	   (push code cscompo-orc-code)))
        (:score
	 (dolist (code body)
	   (push code cscompo-sco-statements)))
        (:i-stream       
	 (push `(with-pfields-stream nil ,@body) cscompo-sco-statements ))
        (:options 
	 (setq options (mapconcat (lambda (f) (format "%s" f)) body " ")))
        (:globals        
           (dolist (code body)
             (push `(csl-insert-code ,code) cscompo-orc-globals)))))
    `(defun* ,name ,parameters
       ,(or comment "Insert a CSD at point (no more details).")
       (atomic-change-group
	 (with-csd-template
	  ,@(when options
	    `((with-csl-options-template
	       (insert ,options))))
	  (with-csl-orchestra-template
	   ,@(nreverse cscompo-orc-globals)
	   ,@(nreverse cscompo-orc-code))
	  (with-csl-score-template
	   ,@(nreverse cscompo-sco-header)
	   ,@(nreverse cscompo-sco-statements)))))))

(defmacro csound-composition (&rest body)
  "Perform an anonymous `defCSD' with no parameters, return the lambda.

See Info node `(csound-x)Csound Elisp' for documentation."
  `(symbol-function (defCSD ,(gensym) () ,@body)))

(defun csl-edit-composition (composition &rest parameters)
  "Edit COMPOSITION in a new CSD buffer.
COMPOSITION is either a lambda form returned by `csound-composition' or a
symbol fbound via `defCSD', in which case it possibly accepts PARAMETERS."
  (switch-to-buffer-other-frame ;; give a choice here ?
   (generate-new-buffer 
    (if (symbolp composition)
	(format "CSD %.30s" (append (list composition) parameters))
      "Anonymous Csound composition")))
  (apply composition parameters)
  (goto-char (point-min))
  (csound-csd-mode))

(defun csl-play-composition (composition &rest parameters)
  "Play COMPOSITION to DAC.
COMPOSITION is either a lambda form returned by `csound-composition' or a
symbol fbound via `defCSD', in which case it possibly accepts PARAMETERS."
  (with-csound-to-DAC 
      (apply composition parameters)))

(defun csl-render-composition (wavfile composition &rest parameters)
  "Render COMPOSITION into WAVFILE, a file name.
COMPOSITION is either a lambda form returned by `csound-composition' or a
symbol fbound via `defCSD', in which case it possibly accepts PARAMETERS."
  (with-csound-to-WAV wavfile (apply composition parameters)))

(defun csl-render-edit-composition (wavfile composition &rest parameters)
  "Render COMPOSITION into WAVFILE, a file name, then open that file via
function `cscsd-edit-audio'.
COMPOSITION is either a lambda form returned by `csound-composition' or a
symbol fbound via `defCSD', in which case it possibly accepts PARAMETERS."
  (with-csound-to-audio-editor wavfile (apply composition parameters)))

;; useful ?
(defun csl-insert-composition (composition)
  "Insert the lisp code for COMPOSITION at point.
COMPOSITION is either a lambda form returned by `csound-composition' or a
symbol fbound via `defCSD'"
  (insert (pp-to-string (if (symbolp composition)
			    (symbol-function composition)
			  composition))))


;; ===================================================================
;;                   f-tables utilities
;; ===================================================================

(defcustom csl-tables '(:sin "f %s 0 %s 10 1")
  "Templates for GEN tables
First replacement slot is for table number, second for table size"
  :type 'plist
  :group 'csound-lsp)

(defun csl-insert-ftable (num type size)
  (insert (format (plist-get csl-tables type)
		  num (expt 2 (round (log size 2)))) ?\n))


;; ===================================================================
;;                  examples & testing (to be cleaned up...)
;; ===================================================================



(defCSD example-compo (n sz vol)
  :about "test composition. use N, SZ and VOL as parameters"
  :options "-dummy --no joke"
  :sr (* 4 12 1000)
  :globals [::loop for i upto 5
		   `(= (:output ,(intern (format "ga%s" i))) ,n)]
  :ftables (1 :sin sz)
  :opcode "dummy,i,ii"
  [out 0]
  :instr 1
  [out (oscili :arate (oscili :krate p4 1/p3 p7) p5 p6)]
  :score
  (insert "f 2 0 513 5 1 12 1024 500 1" ?\n)
  (i 1 0 4 vol 440 1 2)
  (loop for h in '(2200 600 215 1852 990)
	for d in '(2 2.5 2 1.5 4)
	do (+i :p5 h :p3 d)))

;(csl-edit-composition 'example-compo 56 16001 8003)


'(examples 
  ::add
  (csl-edit-composition
   (csound-composition
    :instr 1
    [out (::add for amp in '(p4 2*p4 p4/2)
		for freq in `(1/p3 ,(/ 1 10.0) ,(+ .01 0.05))
		`(oscili :krate ,amp ,freq p7))]))

  (csl-edit-composition
   (csound-composition
    :instr 1
    [out (::add for amp in '(p4 2*p4 p4/2)
		for freq in `(1/p3 ,(/ 1 10.0) ,(+ .01 0.05))
		`[oscili :krate ,amp ,freq p7])]))

  ::if 
  (csl-edit-composition
           (csound-composition
            :instr 1
            [::if (p4 < 2000)
                    (oscili (:output aosc1) p4 1/p3 p7)
                    (oscili (:output aosc) p4 1/p3 p7)]
	    [out aosc]))

  (csl-edit-composition
           (csound-composition
            :instr 1
            [::if (p4 < 2000)
                    [oscili (:output aosc1) p4 1/p3 p7]
                    [oscili (:output aosc) p4 1/p3 p7]]
	    [out aosc]))

  (csl-edit-composition
           (csound-composition
            :instr 1
            `[::if (p4 < 2000)
                    [oscili (:output aosc1) p4 ,(+ 3 7) p7]
                    [oscili (:output aosc) p4 1/p3 p7]]
	    [out aosc]))

  ::cond 
  (csl-edit-composition
           (csound-composition
            :instr 1
            [(::cond ((p4 < 2000)
                       (oscili (:output aosc1) p4 1/p3 p7)
                       (oscili (:output aosc) p4 1/p3 p7))
                      ((5 == 2)
                       (oscili (:output kbof) 1 2 3)))
	     (out aosc)]))

  (csl-edit-composition
           (csound-composition
            :instr 1
            [::cond ((p4 < 2000)
		     [oscili (:output aosc1) p4 1/p3 p7]
		     [oscili (:output aosc) p4 1/p3 p7])
		    ((5 == 2)
		     [oscili (:output kbof) 1 2 3])]
	    [out aosc]))

  ::loop
  (csl-edit-composition
           (csound-composition
            :globals
            [::loop for i upto 5
		    `(init (:output ,(intern (format "ga%s" i))) 
			   ,(* 10 i))]))

  (csl-edit-composition
           (csound-composition
            :globals
            [::loop for i upto 5
		    `[init (:output ,(intern (format "ga%s" i))) 
			   ,(* 10 i)]]))

end examples)


(defvar csl-example-composition 
  (csound-composition
   :instr 1108
   '(out (oscili :arate (linen :krate p4 p6 p3 p7) p5 p8))
   :instr 1109
   '(out (* (:output aout) 
            (linen (:output kline) p4 p6 p3 p7)
            (+ (oscili (:output aosc1) 1 p5 p9)
               (oscili (:output aosc2) 1 p8 p9))))
   :ftables (1 :sin 8192)
   :score
   (i 1108 0 2 15000 400 .3 .3 1)
   (i 1108 3 2 15000 10 .3 .3 1)
   (i 1109 6 2 25000 400 .3 .3 10 1)
   ;; (s)
   (insert "s\n")
   (i 1108 1 2 15000 400 .3 .3 1)
   (i 1108 4 2 15000 170 .3 .3 1)
   (i 1109 7 2 25000 400 .3 .3 170 1)  
   ;; (s)
   (insert "s\n")
   (i 1108 1 2 15000 400 .3 .3 1)
   (i 1108 4 2 15000 385 .3 .3 1)
   (i 1109 7 2 25000 400 .3 .3 385 1)))

(defun csl-test-dac ()
  (interactive)
  (csl-play-composition csl-example-composition))

(defun csl-test-wav ()
  (interactive)
  (csl-render-composition csl-example-composition "test.wav"))

(defun csl-test-edit ()
  (interactive)
  (csl-edit-composition csl-example-composition))


;; === this is it.
(provide 'csound-lsp)

;; csound-lsp.el ends here

