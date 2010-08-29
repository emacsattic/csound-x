;;; -*- auto-recompile: t -*-

;;; csound-fx.el --- functions for processing audio files

;; Keywords: csound, convenience, csd

;; This file is not part of GNU Emacs.
;; 
;; csound-fx.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-fx.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;; last modified January 21, 2009

;;;=====================================================================
;;; Code:

(require 'csound-csd)

(defgroup csound-fx nil
  "Functions for processing audio files with Csound"
  :group 'csound-x
  :prefix "csfx-")

(defvar csfx-library 
  '((:name "freeverb"
	   :docstring "the freeverb opcode, a stereo reverb unit" 
	   :parameters ((:name "Mix" :default 0.5 :min 0 :max 1)
			(:name "RoomSize" :default 0.7 :min 0 :max 1)
			(:name "HFDamp" :default 0.35 :min 0 :max 1)
			(:name "DecayTime" :default 2 :min 0))
    ))
  "A list of specification for DSP effects implemented as CSD files.
The CSD file name for effect NAME must be csfx-NAME.csd and it must be 
located in `cscsd-INCDIR'.
See functions `csfx-define-fx' and `csfx-install-library' for usage.")

(defun csfx-process-wav (csd-file wav-file &optional out-file macros)  
  "Applies the process implemented in CSD-FILE to WAV-FILE, and
write the result to OUT-FILE. MACROS provide orchestra macros definitions.
See function `cscsd-interpret-command' for MACROS format.
CSD-FILE must be located in `cscsd-INCDIR'. See there for examples.
OUT-FILE can be 'dac in which case the output is played."
  (let ((csd-file (expand-file-name csd-file (cscsd-INCDIR)))
	(out-file 
	 (or out-file 
	     (let ((temporary-file-directory (cscsd-SFDIR)))
	       (make-temp-file "tmp" nil ".wav")))))
    (add-to-list
     'macros
     (cons "WavFile"
	   (format "\"%s\""
		   (if (eq system-type 'windows-nt)
		       (w32-long-file-name wav-file)
		     wav-file))))
    (if (eq 'dac out-file)
	(cscsd-call-csound-to-string 
	 "$csound %s -dodac" csd-file nil nil macros)
      (cscsd-call-csound-to-string
       (concat "$csound %s -dWo \""
	       (if (eq system-type 'windows-nt)
		   (w32-long-file-name out-file)
		 out-file)
	       "\"") csd-file nil nil macros))
    out-file))

(defun csfx-define-fx (spec)
  "SPEC is an element in variable `csfx-library' describing effect NAME.
This function defines a function called csfx-NAME used to invoke this effect,
with arguments the input file, the output file and all parameters \(defaulting 
to their default values in SPEC).
It also define another function called csfx-NAME-csd of no argument, which can
be used to visit the CSD file implementing the effect."
  (let ((parameters (mapcar 
		     (lambda (p)
		       (intern (plist-get p :name)))
		     (plist-get spec :parameters)))
	(csd-file (concat "csfx-" (plist-get spec :name) ".csd")))
    (eval `(defun ,(intern (concat "csfx-" (plist-get spec :name)))
	     (wav-file &optional out-file ,@parameters)
	     ,(concat "Process WAV-FILE via "
	       (plist-get spec :docstring))
	     (csfx-process-wav 
	      ,csd-file
	      wav-file
	      out-file
	      (list ,@(mapcar 
		       (lambda (p)
			 (list 'cons (plist-get p :name) 
			       (list 'or 
				     (intern (plist-get p :name))
				     (plist-get p :default))))
		       (plist-get spec :parameters))))))
    (eval `(defun ,(intern (concat "csfx-" (plist-get spec :name) "-csd")) ()
	     (interactive)
	     ,(concat "Visit the CSD file required by function `" 
		      (concat "csfx-" (plist-get spec :name))
		      "'.")
	     (find-file (expand-file-name ,csd-file (cscsd-INCDIR)))))))

(defun csfx-csd-files-exist-p ()
  "Check if all CSD files required by the definitions in variable
`csfx-library' actually exist."
  (loop for fx in csfx-library 
	always (file-exists-p 
		(expand-file-name 
		 (concat "csfx-" (plist-get fx :name) ".csd")
		 (cscsd-INCDIR)))))

;TEST (csfx-csd-files-exist-p) => t

(defun csfx-install-library ()
  "Define interface functions for all DSP effects in `csfx-library'"
  (interactive)
  (dolist (fx csfx-library)
    (csfx-define-fx fx)))

(csfx-install-library)

;;======================================================================
;; this is it

(provide 'csound-fx)

;; csound-fx.el ends here




