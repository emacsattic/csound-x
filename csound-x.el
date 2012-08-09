;;; csound-x.el --- extending csound-sco and csound-orc major modes
;;;                 this is the installation code

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-x.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-x.el is distributed in the hope that it will be useful,
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

;;; Installation:
;;---------------                   
;;
;; install Calc if necessary.
;;
;; add the following to your .emacs:
;;
;;   (setq load-path (cons "~/site-lisp/csound-x" load-path))
;;   (require 'csound-x)

;; last modified February 19, 2009

(require 'cl)

;;;======= calc (required)

;;; Commands added by calc-private-autoloads on Sat Sep 04 00:02:52 1999.
(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
(global-set-key "\e#" 'calc-dispatch)
;;; End of Calc autoloads.

;; mode algébrique par défaut:
(setq calc-algebraic-mode t)


;;;======== (optional) MMM mode support 

(when (require 'mmm-auto nil t)

  ;; bug fix in mmm-region.el:
  (defun mmm-submode-changes-in (start stop)
    "Return a list of all submode-change positions from START to STOP.
The list is sorted in order of increasing buffer position."
    (sort
     (remove-if (lambda (pos) (or (< pos start) (> pos stop)))
		(remove-duplicates
		 (list* start stop
			(mapcan #'(lambda (ovl)
				    `(,(overlay-start ovl)
				      ,(overlay-end ovl)))
				(mmm-overlays-overlapping start stop)))))
     #'<))

  (setq mmm-global-mode 'maybe
	mmm-submode-decoration-level 0)
  (add-to-list 'mmm-save-local-variables 'tab-stop-list)  
  (mmm-add-classes
   '((csound-orc
      :submode csound-orc-mode
      :front "<CsInstruments>"
      ;; :front-offset (end-of-line 1)
      :back "</CsInstruments>"
      :face mmm-code-submode-face)
     (csound-sco
      :submode csound-sco-mode
      :front "<CsScore>"
      ;; :front-offset (end-of-line 1)
      :back "</CsScore>"
      :face mmm-code-submode-face)
     (keykit
      :submode keykit-mode
      :front "<KeyPhrase[^>]*>"
      :back "</KeyPhrase>"
      :face mmm-code-submode-face)
     (embedded-elisp
      :submode emacs-lisp-mode
      :front "<ELISP>"
      :back "[;# \t]*</ELISP>"
      :face mmm-code-submode-face)
     (python
      :submode python-mode
      :front "{{"
      :back "}}"
      :face mmm-code-submode-face)))
  (dolist (class '(python csound-sco csound-orc keykit embedded-elisp))
    (add-to-list 'mmm-mode-ext-classes-alist `(csound-csd-mode nil ,class)))
  (defadvice mmm-mode-on-maybe (around csoundx-around-mmm)
    "Take care of preserving the buffer modification status"
    (let ((cscsd-modflag (buffer-modified-p)))
      ad-do-it
      (set-buffer-modified-p cscsd-modflag)))
  (ad-activate 'mmm-mode-on-maybe))


;; ================== csound-x proper ======================

(require 'thingatpt)

(setq auto-mode-alist 
      (cons '("\\.\\(orc\\|udo\\)$" . csound-orc-mode) auto-mode-alist))
(autoload 'csound-orc-mode "csound-orc" "Csound Orchestra major mode." t)

(setq auto-mode-alist (cons '("\\.sco$" . csound-sco-mode) auto-mode-alist))
(autoload 'csound-sco-mode "csound-sco" "Csound Score major mode." t)

(setq auto-mode-alist (cons '("\\.csd$" . csound-csd-mode) auto-mode-alist))
(autoload 'csound-csd-mode "csound-csd" "Csound csd minor mode." t)

(defgroup csound-x nil
  "Csound front-end for Emacs"
  :link '(info-link :tag "Csound-x documentation (Info)" "(csound-x)Top")
  :group 'languages
  :group 'spfa
  :prefix "csoundx-")

(defgroup csound-x-applications-paths nil
  "Variables defining a path or a command line"
  :group 'spfa-paths
  :group 'csound-x)

;; these extra libraries are required here for convenience:
;; you may comment out the following lines if you do not use them
(require 'i nil t)
(require 'k nil t)
(require 'csound-lsp nil t)

;; ================== key bindings ======================

(require 'csound-kbs)

;; ================== global menu ======================

(defcustom csound-x-force-menu-mode t
  "When not nil, always turn `menu-bar-mode' on when starting up.
This intrusive option is here to have people using Emacs without menus
realize that menus are an important part of csound-x user interface."
  :group 'csound-x
  :type 'boolean)

(when csound-x-force-menu-mode (menu-bar-mode 1))

(defcustom csound-x-show-global-menu nil
  "If not nil, a Csound menu is always present when visiting 
a non-csound buffer."
  :group 'csound-x
  :type 'boolean)

(defun csound-global-menu-visible-p ()
  (and csound-x-show-global-menu
       (not (member major-mode 
		    '(csound-csd-mode csound-orc-mode csound-sco-mode)))))

(easy-menu-define csound-menu global-map
  "global menu for csound-x"
  `("Csound"
    :visible (csound-global-menu-visible-p)
    ["New CSD template" csound-csd-new t]
    ["Import clipboard as new CSD" cscsd-import-from-clipboard t]
    "--"
    ,(csound-doc-submenu t)
    "--"
    ,(cscsd-orc-library-menu)
    "--"
    ["Help" (info "csound-x") t]
    ["Debug last Csound invocation" cscsd-debug-last-csound-call
     (not (string= "" cscsd-last-csound-invocation))]
    ["Customize csound-x" (customize-group 'csound-x) t]))


;; ================== convenience ======================

(add-to-list 'mouse-buffer-menu-mode-groups ' ("score" . "Csound Score") t)


;; to do ... 

;; ;(add-hook 'emacs-lisp-mode-hook     ...
;; ;(add-hook 'lisp-interaction-mode-hook  ...

;; (defconst csound-x-lisp-font-lock-keywords
;;   (append lisp-font-lock-keywords-2
;;     `((,(concat
;; 	"(" (regexp-opt
;; 	     '("save-stream" "save-stream-insert"
;; 	       "pfstream-defun" "pfstream-defalias") t)
;; 	"\\>")
;;       . 1))))

;; (setq lisp-font-lock-keywords-2 csound-x-lisp-font-lock-keywords)


;; ================== this is it.
(provide 'csound-x)

;; csound-x.el ends here
