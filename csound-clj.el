;;; csound-clj.el --- csound API with Clojure

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-clj.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-clj.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

(require 'clojure-mode)

(defvar clojure-home (substitute-in-file-name "$SURMULOTDIR/clojure")) 


(add-to-list 'Info-additional-directory-list clojure-home) ;; SLIME doc


;; ------- settings to be improved 

(defvar csclj-csound-path 
  (if (eq system-type 'gnu/linux) 
      ""
    (substitute-in-file-name "$SURMULOTDIR\\csound\\bin")))

(defvar csclj-csnd-jar 
 (if (eq system-type 'gnu/linux) 
      "/usr/share/java/csnd-5.08.0.jar"  ;;; make this dynamic or customizable
   (expand-file-name "csnd.jar" csclj-csound-path)))

(defvar csclj-csnd-library-path 
  (if (eq system-type 'gnu/linux) 
      "/usr/lib/jni/")) ;"/usr/lib/jni/lib_jcsound.so"))

;; --------


(defvar csclj-init-file 
  (substitute-in-file-name "$SURMULOTDIR/clojure/surmulot.clj"))

(add-to-list 'special-display-buffer-names "*slime-repl clojure*")

(defun surmulot-start-clojure (&optional init-form)
  (interactive)
  (require 'swank-clojure-autoload)
  ;;
  ;; ugly, make this dynamic and versatile (how ?)
  (unless (eq system-type 'gnu/linux)
    (setenv "PATH" (concat csclj-csound-path ";" (getenv "PATH"))))
  ;;
  (swank-clojure-config
   (slime-setup '(slime-repl))
   (setq swank-clojure-jar-path (expand-file-name "clojure.jar" clojure-home ))
   (when csclj-csnd-library-path
     (add-to-list 'swank-clojure-library-paths csclj-csnd-library-path))
   (setq swank-clojure-extra-classpaths 
	 (list (expand-file-name "clojure-contrib.jar" clojure-home)
	       csclj-csnd-jar)))
  (require 'slime)
  (setq slime-repl-banner-function 
	(lambda () (insert ";--- Clojure interaction ---")))
  (save-excursion 
    (save-window-excursion (slime)))
  (while (not (slime-connected-p))
    (sit-for 1))
  (surmulot-clojure-eval `(load-file ,csclj-init-file)))

;;; --------------------------- experimental  -----------------------------

;; (defun surmulot-clojure-eval (form)
;;   (slime-eval-with-transcript 
;;    `(swank:interactive-eval ,(prin1-to-string form))))

(defun surmulot-clojure-eval (form)
  (process-send-string 
   "*inferior-lisp*" 
   (concat (prin1-to-string form) "\n")))

(defun surmulot-slider-demo ()
  (interactive)
  (unless (and (featurep 'slime) 
	       (slime-connected-p))
	(surmulot-start-clojure))
  (surmulot-clojure-eval '(def gui (slider-demo))))


;(surmulot-clojure-eval '(load-file "draft.clj"))
; (csound-play "../csound/library/examples/study.csd")
; (csound-play "d:/devel/surmulot/csound/library/examples/study.csd")


;; ================== this is it.
(provide 'csound-clj)

;; csound-clj.el ends here
