;;; -*- auto-recompile: t -*-

;;; csound-csd.el --- editing and processing Csound CSD files

;; Keywords: csound, convenience, csd

;; This file is not part of GNU Emacs.
;; 
;; csound-csd.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-csd.el is distributed in the hope that it will be useful,
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

;;    full documentation and last version can be found at:
;;    http://www.zogotounga.net/comp/csoundx.html

;;
;;; Installation:
;;                  
;; ==========================================================
;; this file should be installed through the csound-x package
;; ==========================================================
;;
;; but, if you want to install it independently,
;; add the following to your .emacs:
;;
;;   (require 'csound-csd)
;;   (add-hook 'csound-orc-mode-hook 'csound-csd-install-orc)
;;   (add-hook 'csound-sco-mode-hook 'csound-csd-install-sco)

;; last modified August 12, 2009

;;;=====================================================================
;;; Code:

(require 'cl)
(require 'easymenu)
(require 'custom)
(require 'cus-edit)
(require 'query-sheet)


;;======================================================================
;;                          defvars, defcustoms
;;======================================================================

(defgroup csound-csd nil
  "Utilities for Csound csd files"
  :group 'csound-x
  :prefix "cscsd-")

(defun cscsd-open-to-environment (keep-list)
  "Revert all environment variables that csound-x may have overridden
to their previous value. 
The list of variables is kept in `cscsd-hidden-environment-variables'.
Variables in KEEP-LIST are restored to their previous value, appended with
their current value using ; \(giving the current value higher precedence)"
  (interactive)
  (dolist (var cscsd-hidden-environment-variables)
    (let ((val (symbol-value (intern (concat "cscsd-hidden-" var))))
	  (current (getenv var)))
      (when (member var keep-list)
	  (setq val (if (and val current)
			(concat val ";" current)
		      current)))
      (if val
	  (message "resetting env variable %s to \"%s\"" var val)
	(message "resetting env variable %s to nil (undefined)" var))
      (custom-set-default (intern (concat "cscsd-" var)) val)
      (setenv var val))))

(defvar cscsd-hidden-environment-variables ()
  "A list of environment variables possibly overriden by csound-x.
See function `'cscsd-open-to-evironment")

(defmacro cscsd-defenv (var &optional default)
  "Read via `getenv' a path-like environment variable named VAR,
store its value in variable 'cscsd-VAR and define a 'cscsd-VAR function 
which return the true path using `substitute-in-file-name'.
If no such environment variable is found, it is set to DEFAULT.
If VAR specify several paths, the function returns only the first one;
another function named 'cscsd-full-VAR is defined which return all paths.
If a customized value of cscsd-VAR exists, then it becomes VAR value:
`setenv' is invoked in order to override its current value in the environment,
while the older value is kept in variable 'cscsd-hidden-VAR.
VAR is added to the list `cscsd-hidden-environment-variables'
"
  `(let* ((str ,var)
	  (doc (concat str " environment variable"))
	  (varsymb (intern (concat "cscsd-" str)))
	  (varsymbhid (intern (concat "cscsd-hidden-" str)))
	  (varsymbfull (intern (concat "cscsd-full-" str)))
          (def ,default))
     (unless (member str cscsd-hidden-environment-variables)
       (eval `(progn
		(add-to-list 'cscsd-hidden-environment-variables ,str)
		(setq ,varsymbhid (getenv ,str))
		(defcustom ,varsymb (or ,varsymbhid ,def "~")
		  ,doc
		  :set 
		  `(lambda (p val)
		     (custom-set-default p val)
		     (setenv ,str
			     (expand-file-name
			      (substitute-in-file-name val))))
		  :type 'string
		  :group 'csound-csd
		  :group 'csound-x-applications-paths)
		(defun ,varsymb ()
		  (when ,varsymb
		    (substitute-in-file-name
		     (car (split-string ,varsymb ";")))))
		(defun ,varsymbfull ()
		  (when ,varsymb
		    (reverse (mapcar 'substitute-in-file-name
				     (split-string ,varsymb ";")))))
		(setenv ,str (mapconcat 
			      'substitute-in-file-name 
			      (split-string ,varsymb ";") ";")))))))

(dolist (var '("SSDIR" "SFDIR" "INCDIR" "MFDIR" "SADIR"
	       "OPCODEDIR" "OPCODEDIR64" "RAWWAVE_PATH"))
  (cscsd-defenv var "~"))

(cscsd-defenv "CSOUNDRC" "~/.csoundrc")

(defun cscsd-actual-sfdir ()
  (or (cscsd-SFDIR) default-directory))

(defun cscsd-actual-ssdir ()
  (or (cscsd-SSDIR) default-directory))

(defun cscsd-actual-csoundrc ()
  "Return the file name for the appropriate csound resource file
\(either the one set by environment variable CSOUNDRC or the .csoundrc file
in HOME or in the current directory) "
  (let ((loc (expand-file-name ".csoundrc" 
			       (buffer-file-name (current-buffer)))))
    (or (and (file-exists-p loc) loc)
	(cscsd-CSOUNDRC)
	(expand-file-name "~/.csoundrc"))))

(defun csound-find-directory ()
  "Tries to find the directory for a csound installation in the system.
Returns either that directory or nil."
  (when (eq system-type 'windows-nt) ;; Windows only at the moment
    (let ((csound-bin-path (find-if
			    (lambda (s) (string-match "csound.*bin" s))
			    (split-string (getenv "PATH") ";"))))
      (when (or csound-bin-path cscsd-hidden-CSOUNDRC)
	(file-name-directory (or csound-bin-path cscsd-hidden-CSOUNDRC))))))

;; the following may need 'csound-find-directory for initialisation:
(require 'csound-spdb)
(require 'csound-eel)  ; we need this anyway
(require 'csound-key nil t)
(require 'csound-ft)
(require 'csound-mid)
(require 'csound-doc)
(require 'comint)

(defun csound-find-binary ()
  "Tries to find a csound executable in the system."
  (let ((csound-path (csound-find-directory)))
    (if csound-path (expand-file-name "bin/csound.exe" csound-path)
      "csound")))

(defcustom cscsd-csound-binary (csound-find-binary)
  "The default csound binary"
  :type 'string
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defun cscsd-csound-binary ()
  (substitute-in-file-name cscsd-csound-binary))

(defcustom cscsd-process-file
  '(("csound <CsOptions> csd" 
     "$csound %s &")
    ("csound wav" 
     "$csound -dWo %s %s &")
    ("csound wav (sync.)"
     "$csound -dWo %s %s")
    ("csound dac"
     "$csound -dodac %s &")
    ("csound midi to dac"
     "$csound -dodac %s -TF %s &")
    ("csound wav + play"     
     (lambda (csd-file output-file)
       (cscsd-call-csound "$csound -dWo %s %s" csd-file output-file)
       (cscsd-play-audio)))
    ("csound wav + edit"     
     (lambda (csd-file output-file)
       (cscsd-call-csound "$csound -dWo %s %s" csd-file output-file)
       (cscsd-edit-audio)))
    ;("csoundAV" "CsoundAV_Con.exe %s &")
    )
  "A list of pairs \(NAME FUNCTION) or \(NAME SYSTEM-CALL) among which
you can choose which process is to be invoked by the \"process file\" and
\"process regions\" items from the \"Csd\" menu.

- when the process NAME is associated to a FUNCTION, it must accept one,
two or three arguments: the first argument is the file name associated to
the current csd buffer, the second argument is the file name of the audio
output, the third one is the file name for MIDI input.

  when the function action does create an audio file, it should set variable
`cscsd-latest-audio-file' to the file name, without a path: the file should
be located in SFDIR. 

  when the function action requires a MIDI file, it should set variable
`cscsd-latest-midi-file' to its full file name, with a path. 

- when the process NAME is associated to a SYSTEM-CALL, a string, it will be
used as the first argument for `cscsd-call-csound'"
  :type '(repeat (list (string :tag "processing label") 
		  (choice :tag "action" string function)))
  :group 'csound-csd)

(defcustom cscsd-current-processing ""
  "Label of the currently selected processing function, or \"\" if there
is only one function"
  :type 'string
  :group 'csound-csd)

(defcustom cscsd-process-with-modifications t
  "When not nil, a CSD buffer marked modified is processed by `cscsd-process'
via a temporary CSD file. When nil, the buffer must be saved else the current
version on file is processed."
  :type 'boolean
  :group 'csound-csd)

(defcustom cscsd-default-csd-directory "~"
  "Directory where automatically created CSD files are saved"
  :type 'directory
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defun cscsd-default-csd-directory ()
  (substitute-in-file-name cscsd-default-csd-directory))

(defcustom cscsd-temp-csd-file 
  (expand-file-name "tmp.csd" (cscsd-default-csd-directory))
  "Temporary CSD file used when processing the region"
  :type 'file
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defun cscsd-temp-csd-file ()
  (substitute-in-file-name cscsd-temp-csd-file))

(defcustom cscsd-shell-compile-sco "$csound -dWo %s %s %s &"
  "Shell command used to compile a sco buffer.
It should feature three occurences of %s, to be replaced repectively with:
the audio file name \(minus its extension\), the orchestra file name and the
score file name"
 :type 'string
 :group 'csound-csd
 :group 'csound-x-applications-paths)

(defcustom cscsd-shell-play-sco "$csound -dodac %s %s &"
  "Shell command used to compile a score buffer and send audio to DAC.
It should feature two occurences of %s, to be replaced with the orc file name
and the sco file name, respectively"
 :type 'string
 :group 'csound-csd
 :group 'csound-x-applications-paths)

(defcustom cscsd-shell-play-audio "wavesurfer -config default -play \"%s\" &" 
  "Shell command used to play an audio file.
It should feature a single %s occurrence, to be replaced with the audio file
name"
  :type 'string
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defcustom cscsd-shell-edit-audio "wavesurfer -config default \"%s\" &" 
  "Shell command used to edit an audio file.
It should feature a single %s occurrence, to be replaced with the audio file
name"
  :type 'string
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defcustom cscsd-associated-orc
  (lambda ()
    (when (and buffer-file-name (string-match "\\.sco$" buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (if (looking-at "^[ \t]*;[ \t]*\\(.*\\.orc\\)")
            (expand-file-name (match-string 1) 
			      (file-name-directory (buffer-file-name)))
          (let ((guessed (concat (file-name-sans-extension
				  (buffer-file-name)) ".orc")))
	    (when (file-exists-p guessed) guessed))))))
  "Function returning the file name of the orchestra associated to the current
score buffer or nil if such an orchestra can not be defined"
  :type 'function
  :group 'csound-csd)

(defcustom cscsd-associated-sco
  (lambda ()
    (when (and buffer-file-name (string-match "\\.orc$" buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (if (looking-at "^[ \t]*;[ \t]*\\(.*\\.sco\\)")
            (expand-file-name (match-string 1) 
			      (file-name-directory (buffer-file-name)))
          (let ((guessed (concat (file-name-sans-extension 
				  (buffer-file-name)) ".sco")))
	  (when (file-exists-p guessed) guessed))))))
  "Function returning the file name of the score associated to the current
orchestra buffer or nil if such a score can not be defined"
  :type 'function
  :group 'csound-csd)

(defcustom cscsd-default-orc-header 
  "sr = 44100\nkr = 4410\nksmps = 10\nnchnls = 2\n\n"
  "Headers to be inserted at the beginning of the <CsInstruments> section
when creating a new csd template"
  :type 'string
  :group 'csound-csd)

(defcustom cscsd-default-options ""
  "Text to be inserted in the <CsOptions> section when creating a new
CSD template"
  :type 'string
  :group 'csound-csd)

(defcustom cscsd-csr-files '()
  "List of .csr files to import.
\(this format is used by the CsEdit software to store chunks of code)
The contents of the files will go under the `Insert' submenu"
  :type '(repeat string)
  :group 'csound-csd)

(defcustom cscsd-current-csr-file (expand-file-name "csoundx.csr" "~")
  "Current csr file for editing"
  :type ' string
  :group 'csound-csd
  :group 'csound-x-applications-paths)

(defun cscsd-current-csr-file ()
  (substitute-in-file-name cscsd-current-csr-file))

(defcustom cscsd-csr-author "?"
  "Value of the Author= field in the current *.csr file"
  :type ' string
  :group 'csound-csd)

(defvar cscsd-orc-overlay nil
  "Overlay used to control invisibility of the CsInstruments section")

(defvar cscsd-sco-overlay nil
  "Overlay used to control invisibility of the CsScore section")

(defcustom cscsd-use-MMM (featurep 'mmm-auto) ; makes sense: see csound-x.el
  "If t, toggle MMM minor mode by default"
  :type 'boolean
  :group 'csound-csd)

(defcustom cscsd-use-always-indirect-buffers nil
  "If t, always edit CSD files through indirect buffers to score and orchestra"
  :type 'boolean
  :group 'csound-csd)

(defcustom cscsd-use-indirect-buffers-if-no-MMM t
  "If t, edit CSD files through indirect buffers to score and orchestra when 
MMM minor mode is not turned on"
  :type 'boolean
  :group 'csound-csd)

(defcustom cscsd-use-EEL t
   "If t, toggle embedded-elisp minor mode by default for CSD buffers."
   :type 'boolean
   :group 'csound-csd)

(defcustom cscsd-no-confirmation-for-association nil
   "If t, no confirmation prompt allows changing the file \(orc/sco)
associated to the current buffer when such a file is found"
   :type 'boolean
   :group 'csound-csd)

(defcustom cscsd-no-confirmations nil
   "If t, operations never ask for confirmation before proceeding
this is mainly intended for batch usage, as it may be dangerous for
interactive editing"
   :type 'boolean
   :group 'csound-csd)

(defcustom cscsd-import-clipboard-with-new-frame t
   "If t, `cscsd-import-from-clipboard' does create a new frame,
else it reuses the current window"
   :type 'boolean
   :group 'csound-csd)

(defcustom csound-x-disable-smart-semicolon nil
  "if non-nil, the `;' key is no longer electric in csound-sco
and csound-orc modes"
  :type 'boolean
  :set (lambda (p val)
	 (custom-set-default p val)
	 (when (featurep 'csound-orc)
	   (csound-orc-mode-populate-keymap))
	 (when (featurep 'csound-sco)
	   (csound-sco-mode-populate-keymap)))
  :group 'csound-csd)

(defcustom csound-x-disable-smart-colon nil
  "if non-nil, the `:' key is no longer electric in csound-orc-mode"
  :type 'boolean
  :set (lambda (p val)
	 (custom-set-default p val)
	 (when (featurep 'csound-orc)
	   (csound-orc-mode-populate-keymap)))
  :group 'csound-csd)

(defcustom csound-x-break-column 80
  "The column where line breaks are to happen."
  :type 'integer
  :group 'csound-csd)

(defmacro cscsd-usually-ask-p (&rest body)
  `(or cscsd-no-confirmations
       (y-or-n-p ,@body)))


;; hooks


(defcustom csound-csd-mode-hook nil
  "Hook run when csound-csd-mode is started."
  :type 'hook
  :group 'csound-csd)

(defcustom csound-csd-load-hook nil
  "Hook run after csound-csd has been loaded."
  :type 'hook
  :group 'csound-csd)

(defcustom csound-csd-prepare-process-hook nil
  "Hook run before processing a CSD buffer. 
At this stage we may be in an indirect buffer, and value of variable
`cscsd-current-processing' has not be taken into account yet.
It is followed by `csound-csd-before-process-hook'."
  :type 'hook
  :group 'csound-csd)

(defcustom csound-csd-before-process-hook nil
  "Hook run right before processing a CSD buffer.
At this stage we are in the base CSD buffer, even if processing was
started from an indirect buffer; value of variable `cscsd-current-processing'
has already be taken into account.
See also `csound-csd-prepare-process-hook'."
  :type 'hook
  :group 'csound-csd)

(defcustom csound-csd-after-process-hook nil
  "Hook run right after processing a CSD buffer."
  :type 'hook
  :group 'csound-csd)



;;======================================================================
;;                     major mode definition, menu
;;======================================================================

(defvar cscsd-menu nil
  "Menu for csound-csd-mode.")

(defvar cscsd-csr-was-changed nil
  "Flag used to enable the \"refresh menu\" item in the \"Code repository\"
sub-menus")

(define-derived-mode csound-csd-mode text-mode "Csound CSD"
  "Major mode for editing CSD documents
The value '\(csd . t) is added to `buffer-invisibility-spec'"
  (require 'csound-orc)
  (require 'csound-sco)
  ;;
  (cscsd-build-menus)
  (cscsd-define-tool-bar)
  (set (make-local-variable 'cscsd-orc-overlay) (make-overlay 0 1))
  (set (make-local-variable 'cscsd-sco-overlay) (make-overlay 0 1))
  (add-to-invisibility-spec '(csd . t))
  (add-to-invisibility-spec 'csd-link)
  (overlay-put cscsd-sco-overlay 'invisible nil)
  (overlay-put cscsd-orc-overlay 'invisible nil)
  ;;
  (set (make-local-variable 'eldoc-documentation-function) 
       (lambda nil (csdoc-opcode :template (thing-at-point 'word))))
  ;;
  (require 'embedded-elisp-library nil t)
  (setq embedded-elisp-eval-do-button-function 
	'cseel-eval-do-button)
  (when cscsd-use-EEL
    (embedded-elisp-mode t))
  ;;
  (if (and (featurep 'csound-ses) cscsd-ses-areas-invisible)
      (cscsd-hide-ses-areas))
  (let ((modifp (buffer-modified-p)))
    (cscsd-hide-storage-areas)
    (cscsd-activate-org-bracket-links)
    (unless modifp
      (set-buffer-modified-p nil)))
  ;;
  (add-hook 'activate-menubar-hook 'cscsd-maybe-refresh-menu nil t)
  (run-hooks 'csound-csd-mode-hook)
  ;; 
  (when (cscsd-use-indirect-buffers-p)
    (cscsd-edit-indirectly))
  ;;
  (csound-define-CSD-keys))

(defmacro with-base-buffer (&rest body)
  (declare (indent 0))
  `(save-excursion
     (set-buffer (or (buffer-base-buffer (current-buffer))
		     (current-buffer)))
     ,@body))

(defun cscsd-activate-org-bracket-links ()
  "Run through the buffer and add overlays to Org bracketed links."
  (interactive)
  (when (require 'org nil t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
	(let* ((ip (list 'invisible 'csd-link 
			  'rear-nonsticky t
			  'keymap org-mouse-map))
	       (vp (list 'rear-nonsticky t
			 'keymap org-mouse-map 
			 'face 'link
			 'font-lock-face 'link
			 'mouse-face 'link)))
	  (if (match-end 3)
	      (progn
		(add-text-properties (match-beginning 0) (match-beginning 3) ip)
		(add-text-properties (match-beginning 3) (match-end 3) vp)
		(add-text-properties (match-end 3) (match-end 0) ip))
	  (add-text-properties (match-beginning 0) (match-beginning 1) ip)
	  (add-text-properties (match-beginning 1) (match-end 1) vp)
	  (add-text-properties (match-end 1) (match-end 0) ip)))))))

(defun cscsd-apply-everywhere ()
  "Set the major mode to csound-csd for all buffer editing a CSD file"
  (save-excursion
    (dolist (b (buffer-list))
      (set-buffer b)
      (if (and (buffer-file-name b)
	       (string-match "\\.csd$" (buffer-file-name b))
	       (or (eq major-mode 'csound-orc-mode) 
		   (eq major-mode 'emacs-lisp-mode)
		   (eq major-mode 'csound-sco-mode)))
	  (csound-csd-mode)))))

(add-hook 'desktop-after-read-hook 'cscsd-apply-everywhere)

(defun cscsd-use-indirect-buffers-p ()
  "Tells weither `csound-csd-mode should' should display score and orchestra
as indirect buffers by default"
  (or cscsd-use-always-indirect-buffers
      (and (not cscsd-use-MMM)
	   cscsd-use-indirect-buffers-if-no-MMM)))

(defvar cscsd-repository-menu-cache ()
  "This part of the 'Csd' menu is not dynamically recomputed,
for performance reasons")

(defun cscsd-make-repository-menu ()
  (setq cscsd-repository-menu-cache
  `("Code repository"
    ["Register region" cscsd-csr-add-region t]
    ("Insert" ,@(cscsd-read-csr-files))
    ["Edit csr file" cscsd-edit-csr t]
    ["Refresh menu" cscsd-refresh-repository-menu cscsd-csr-was-changed])))

;; dynamic elements may be missing from menu, replace them:
(unless (featurep 'csound-key)
  (defun csound-csd-keyphrases-menu ()
    ["KeyPhrases" 'ignore nil]))

(defun cscsd-maybe-refresh-menu ()
  "User activated the menu-bar: check if we want to update the Csd menu"
  (cscsd-build-csd-menu) 
  ;; we should cache the menu (buffer-local)... to do
)

(defun cscsd-build-csd-menu ()
  (require 'csound-orc)
  (require 'csound-sco)
  (easy-menu-define cscsd-menu csound-csd-mode-map
    "Menu provided by csound-csd-mode"
    (cscsd-menu-template))
  (easy-menu-define cscsd-menu csound-orc-mode-map
    "Menu provided by csound-csd-mode"
    (cscsd-menu-template))
  (easy-menu-define cscsd-menu csound-sco-mode-map
    "Menu provided by csound-csd-mode"
    (cscsd-menu-template)))

(defun cscsd-menu-template ()
  (save-excursion
  (let ((csdp (cscsd-buffer-is-a-csd-p)))
    (delete-if 'null
    `("CSD"
      ,@(when (and csdp (featurep 'surmulot))
          '(("Surmulot"
	     ["Edit score in widget"  muo-edit-csd-as-widget t]
	     ["Edit score in muO"  muo-edit-csd t]
	     ["Insert instrument format templates" muo-insert-itemplates t]
	     "--"
	     ["Send to muO"  muo-get-csd t])
            "--"))
      ,@(when csdp (csound-csd-keyphrases-menu)) ;;; from csound-key.el
      ,@(when csdp (cseel-menu)) ;;; from csound-eel.el
      "--"
      ,@(when csdp
	  '(("Structure"
	     ["Show structure" cscsd-show-structure (cscsd-non-indirect-buffer-p)]
            ["Display Speedbar" cscsd-speedbar (cscsd-non-indirect-buffer-p)]
	     "--"
             ["Hide/Show Instruments" cscsd-hs-instruments 
	      (cscsd-non-indirect-buffer-p)]
             ["Hide/Show Score" cscsd-hs-score (cscsd-non-indirect-buffer-p)]
             ["Go to Instruments" cscsd-goto-orc (cscsd-non-indirect-buffer-p)]
             ["Go to Score" cscsd-goto-sco (cscsd-non-indirect-buffer-p)]
             "--"
             ["Edit sco & orc indirectly" cscsd-edit-indirectly
	      (cscsd-non-indirect-buffer-p)]
             ["Edit orc by itself" cscsd-orc-alone
	      (cscsd-non-indirect-buffer-p)]
             ["Edit sco by itself" cscsd-sco-alone
	      (cscsd-non-indirect-buffer-p)]
             ["See full CSD" (switch-to-buffer-other-frame
				    (get-buffer (buffer-base-buffer)))
	      (cscsd-indirect-buffer-p)]
             ["Split in orc/sco" cscsd-split-buffer 
	      (cscsd-non-indirect-buffer-p)]
             "--"
	     ["Build a true csd" cscsd-auto-import
	      (cscsd-non-indirect-buffer-p)]
	     ["Build a csd from clipboard" cscsd-import-from-clipboard t])))
      ["Remove line breaks" (cscsd-remove-breaks (point-min) (point-max)) t]
      "--"
      ,(csound-doc-submenu)
;      "--"
      ,(cscsd-orc-library-menu)
      "--"
      ,@(when csdp 
          `(("MIDI settings"
             ,(format "MIDI on: %s" cscsd-latest-midi-file)
             ,@(csmid-midifiles-submenu 
		'latest-midifile "... select a MIDI file:"
		'cscsd-build-csd-menu 
		(lambda (f) (setq cscsd-latest-midi-file f))))
            ["Write MIDI <Options>" cscsd-insert-MIDI-options 
	     (cscsd-non-indirect-buffer-p)]
            ("Insert file/data"            
             ["Arbitrary file" cscsd-insert-file (cscsd-non-indirect-buffer-p)]
             ,@(csmid-midifiles-submenu 
		'keyphrase "MIDI file" 'cscsd-build-csd-menu 
		(lambda (f) (cscsd-insert-file f)))
             ["KeyPhrase" cscsd-insert-KeyPhrase 
	      (and (featurep 'csound-key)
		   (featurep 'keykit-mode)
		   (cscsd-non-indirect-buffer-p))])))
      ;; the repository submenu is only computed upon request:
      ,(or cscsd-repository-menu-cache (cscsd-make-repository-menu))
      ["Macroify region" cscsd-macroify t]
      "--"  
      ["Edit CSOUNDRC file" cscsd-edit-csoundrc t]
      ,(cscsd-processing-menu)
      ,@(if csdp 
            '(["Process file" cscsd-process  t]
              ["Process region" cscsd-process-region 
	       (cscsd-non-indirect-buffer-p)])
          '(["Process source CSD" 
	     (cscsd-process (buffer-base-buffer (current-buffer)))
	     (cscsd-indirect-buffer-p)]
            ["Process via temp CSD" cscsd-process-scorc-via-temp-csd 
	     (cscsd-non-indirect-buffer-p)]))
      ["Kill Csound process" cscsd-kill-csound t] 
      ["Edit file" cscsd-edit-audio (cscsd-audio-available-p)]
      [,(concat " ... " (if cscsd-latest-audio-file 
			   (file-name-nondirectory cscsd-latest-audio-file)
			 "")) 
       cscsd-edit-audio (cscsd-audio-available-p)]
      [" ... choose in SFDIR" cscsd-browse-SFDIR t])))))

(defcustom cscsd-libraries ()
  "A list of directories of format \(\(LABEL PATH) ...)
that will be made directly accessible from the library submenu."
  :type '(repeat (list (string :tag "label") 
		       (string :tag "location")))
  :group 'csound-csd)

(defun cscsd-orc-library-menu ()
  (append
   '("Orchestras library"
     ["Search for a string" cscsd-csmode-search-query t]
     ["Search for a regexp" cscsd-csmode-search-regexp-query t]
     ["Find all user-defined opcodes"
      (cscsd-csmode-search-regexp-query "^[ \t]*opcode[ \t]+[^ \t,]*") t])
   '( "--"
     ["Set path" (customize-option 'cscsd-orchestras-path) t]
     ["Set recursive path"
      (customize-option 'cscsd-orchestras-recursive-path) t])
   (when cscsd-libraries
     (append '("--")
	     (loop for spec in cscsd-libraries
		   collect 
		   (vector (car spec)
			   `(dired ,(substitute-in-file-name (cadr spec)))
			   t))))))

(defun cscsd-build-menus ()
  (interactive)
  (cscsd-make-repository-menu)
  (require 'csound-orc) ; ORC menu
  (csound-install-orc-menu)
  (require 'csound-sco) ; SCO menu
  (csound-install-sco-menu)
  (cscsd-build-csd-menu) ; CSD menu
  ;; mark menus as "fresh":
  (setq cscsd-csr-was-changed nil)
  ;; hook
  (run-hooks 'csound-csd-refresh-menu-hook))

(defalias 'cscsd-refresh-repository-menu 'cscsd-build-menus)

(defun cscsd-define-tool-bar ()
  (when (boundp 'tool-bar-map)
    (defvar cscsd-tool-bar-map (copy-sequence tool-bar-map))
    (let ((tool-bar-map cscsd-tool-bar-map))
      (tool-bar-add-item-from-menu 
       'cscsd-split-buffer "csd-scorc" csound-csd-mode-map
       :enable '(cscsd-non-indirect-buffer-p))
      (tool-bar-add-item-from-menu 
       'cscsd-process "csd-process" csound-csd-mode-map)
      (tool-bar-add-item-from-menu 
       'cscsd-kill-csound "csd-kill" csound-csd-mode-map)
      (tool-bar-add-item-from-menu 
       'cscsd-play-audio "csd-play" csound-csd-mode-map
       :enable '(cscsd-audio-available-p)))
    (set (make-local-variable 'tool-bar-map) cscsd-tool-bar-map)))

(defvar cscsd-structure-regexp
  "^[ \t]*\\(<Cs\\(Instruments>\\|Options>\\|Score>\\)\\|instr\\|opcode\\)"
  "Regexp matching notable structural lines in a CSD, for easy navigation")
; (concat "^[ \t]*" (make-regexp '("instr" "opcode" "<CsOptions>" "<CsInstruments>" "<CsScore>") t))

(defun cscsd-show-structure ()
  (interactive)
  (when (cscsd-buffer-is-a-csd-p)
    (occur cscsd-structure-regexp)))

(defun cscsd-indirect-buffer-p ()
  (buffer-base-buffer (current-buffer)))

(defun cscsd-non-indirect-buffer-p ()
  (null (cscsd-indirect-buffer-p)))

(defun cscsd-set-current-processing (val)
  (custom-set-variables `(cscsd-current-processing ,val t))
  (custom-save-all))


;;======================================================================
;;                        *.csr management 
;;======================================================================

(defun cscsd-read-csr-files ()
  (let ((vectors ()))
    (dolist (csr-file cscsd-csr-files)
      (setq csr-file (substitute-in-file-name csr-file))
      (if (file-readable-p csr-file)
	  (with-temp-buffer
	    (insert-file-contents csr-file)
	    (goto-char (point-min))
	    (while (re-search-forward "^\\[\\(.*\\)\\][ \t]*$" nil t)
	      (let ((item-id (match-string 1))
		    item-code 
		    (item-cat "General")
		    (item-help "")
		    item-end)
		(save-excursion
		  (setq item-end (or (re-search-forward "^[ \t]*$" nil t)
				     (point-max))))
		(save-excursion
		  (re-search-forward "^Code=\\(.*\\)$")
		  (setq item-code 
			(cscsd-replace-tabs-and-nls (match-string 1))))
		(save-excursion                    
		  (when (re-search-forward "^Category=\\(.*\\)$" item-end t)
		      (setq item-cat (match-string 1))))
		(save-excursion                    
		  (when (re-search-forward "^Comments=\\(.*\\)$" item-end t)
		      (setq item-help (match-string 1))))

		(let ((new-category t))
		  (block check-loop
		    (dolist (v vectors)
		      (if (equal (car v) item-cat)
			  (progn
			    (setq vectors (delete v vectors))
			    (setf v
				  (append v
					  (list (vector item-id
							`(insert ,item-code)
							:help item-help)))
				  new-category nil)
			    (setq vectors (cons v vectors))
			    (return-from check-loop)))))
		  (when new-category
		    (setq vectors (cons (list
					 item-cat 
					 (vector item-id
						 `(insert ,item-code)
						 :help item-help))
					vectors)))))))))
    (or vectors
	'(["(nothing here...)"
	   (message "You need to customize cscsd-csr-files:
 it is either void or not correct for your installation") 
	   ""]))))

(defun cscsd-replace-tabs-and-nls (str)
  (while (string-match "\\\\n" str)
    (setq str (replace-match "\n" t t str)))
  (while  (string-match "\\\\t" str)
    (setq str (replace-match "\t" t t str)))
  str)

(defun cscsd-encode-tabs-and-nls (str)
  (while (string-match "\n" str)
    (setq str (replace-match "\\n" t t str)))
  (while  (string-match "\t" str)
    (setq str (replace-match "\\t" t t str)))
  str)

(defun cscsd-csr-add-region (&optional beg end)
  (interactive "r")
  (if (and beg end)
      (let ((reg (cscsd-encode-tabs-and-nls (buffer-substring beg end))))
	(with-temp-file (cscsd-current-csr-file)
	  (if (file-readable-p (cscsd-current-csr-file))
	      (insert-file-contents (cscsd-current-csr-file)))
	  (goto-char (point-max))
	  (let* ((ref (read-from-minibuffer "Reference: ")))
	    (unless (string= ref "")
	      (let* ((cat (read-from-minibuffer "Category="))
		     (com (read-from-minibuffer "Comments=")))
		(if (string= cat "") (setq cat "General"))
		(insert "\n"
			"[" ref "]\n"
			"Author=" cscsd-csr-author "\n"
			"Code=" reg "\n"
			"Category=" cat "\n"
			"Comments=" com "\n")))))
	(cscsd-refresh-repository-menu))))


(defun cscsd-edit-csr ()
  (interactive)
  (setq cscsd-csr-was-changed t)
  (find-file (cscsd-current-csr-file)))



;;======================================================================
;;                        compilation/processing 
;;======================================================================

(defvar cscsd-latest-audio-file nil
  "Record the name of the latest compiled audio file.
It is buffer-local: an audio file can thus be associated to each CSD or
score buffer.") 

(make-variable-buffer-local 'cscsd-latest-audio-file)

(defvar cscsd-latest-midi-file nil
  "Record the name of the latest input MIDI file.
It is buffer-local: a MIDI file can thus be associated to each CSD") 

(make-variable-buffer-local 'cscsd-latest-midi-file)

(defun cscsd-csound-command (command &optional omacros smacros)
  "Perform appropriate replacements in COMMAND and append orchestra OMACROS
and score SMACROS options when required"
  (let ((rcommand
	 (replace-regexp-in-string
	  "\"\"" "\""
	  (replace-regexp-in-string "$CSOUND" 
				    (format "\"%s\"" (cscsd-csound-binary)) 
				    command))))
    (if (string-match "&$" command)
	(concat (car (split-string rcommand "&"))
		(cscsd-command-macros omacros smacros) " &")
      (concat rcommand (cscsd-command-macros omacros smacros)))))

;TEST (cscsd-csound-command "\"\"bof.bat\"\" -blip") => "\"bof.bat\" -blip"

(defun cscsd-browse-SFDIR ()
  (interactive)
  (setq cscsd-latest-audio-file 
	(read-file-name "Choose associated file: " (cscsd-actual-sfdir) "" t)))

(defcustom cscsd-csound-log-buffer-name "*Csound Log*"
  "Name for the Csound log buffer."
  :type 'string
  :group 'csound-csd)  

(defun cscsd-kill-csound ()
  (interactive)
  (kill-process cscsd-csound-log-buffer-name))

(defcustom cscsd-csound-log-pops-up t
  "If non-nil, the Csound log buffer appears in a pop-up frame
if nil, you may have trouble killing a runnning csound process !"
  :type 'boolean
  :group 'csound-csd)  

(when cscsd-csound-log-pops-up
  (add-to-list 'special-display-buffer-names cscsd-csound-log-buffer-name))

(defun cscsd-maybe-save-buffer (confirmation-prompt)
  (when (and (buffer-modified-p)
             (cscsd-usually-ask-p 
              (format "The buffer has been modified. %s " confirmation-prompt)))
    (save-buffer)))

(defun cscsd-confirm-associated-orc ()
  (cscsd-confirm-file "orchestra:" (funcall cscsd-associated-orc)))

(defun cscsd-confirm-associated-sco ()
  (cscsd-confirm-file "score:" (funcall cscsd-associated-sco)))

(defun cscsd-compile-score (&optional orc-file audio-file)
  "Compile the score in the current buffer, 
with ORC-FILE or with the orchestra file returned by `cscsd-associated-orc'
if AUDIO-FILE is not provided, the audio file name is built from the current
buffer file name"
  (interactive)
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-process)
    (cscsd-maybe-save-buffer "Update score file ?")
    (cscsd-compile-scorc (or audio-file 
			     (file-name-nondirectory (buffer-file-name)))
			 (or orc-file (cscsd-confirm-associated-orc))
                         (buffer-file-name))))

(defun cscsd-compile-orchestra (&optional sco-file audio-file)
  "Compile the orchestra in the current buffer, 
with SCO-FILE or with the score file returned by `cscsd-associated-sco'
if AUDIO-FILE is not provided, the audio file name is built from the current
buffer file name"
  (interactive)
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-process)
    (cscsd-maybe-save-buffer "Update orchestra file ?")
    (cscsd-compile-scorc (or audio-file 
			     (file-name-nondirectory (buffer-file-name)))
                         (buffer-file-name)
			 (or sco-file (cscsd-confirm-associated-sco)))))

(defun cscsd-play-score (&optional orc-file)
  "Compile the score in the current buffer, 
with ORC-FILE or with the orchestra file returned by `cscsd-associated-orc'
send audio to DAC"
  (interactive)
  (unless (cscsd-buffer-is-a-csd-p)
    (cscsd-maybe-save-buffer "Update score file ?")
    (cscsd-play-scorc (or orc-file (cscsd-confirm-associated-orc))
                      (buffer-file-name))))

(defun cscsd-play-orchestra (&optional sco-file)
  "Compile the orchestra in the current buffer, 
with SCO-FILE or with the score file returned by `cscsd-associated-sco'
send audio to DAC"
  (interactive)
  (unless (cscsd-buffer-is-a-csd-p)
    (cscsd-maybe-save-buffer "Update orchestra file ?")
    (cscsd-play-scorc (buffer-file-name)
		      (or sco-file (cscsd-confirm-associated-sco)))))

(defun cscsd-confirm-file (prompt file-name)
  (if (and file-name (or cscsd-no-confirmations 
			 cscsd-no-confirmation-for-association))
      file-name
    (let ((use-dialog-box nil)
	  (dir (if file-name
		   (file-name-directory file-name))))
      (expand-file-name 
       (read-file-name prompt dir nil t 
		       (if file-name 
			   (file-name-nondirectory file-name)
			 ""))
       dir))))

(defun cscsd-init-log-window (dir-or-file)
  (set-buffer (get-buffer-create cscsd-csound-log-buffer-name))
  (erase-buffer)
  (comint-preinput-scroll-to-bottom)
  (cd (file-name-directory dir-or-file)))

(defun cscsd-compile-scorc (audio-file orc-file sco-file)
  (setq audio-file 
	(concat (file-name-sans-extension audio-file)
		(cscsd-guess-audio-extension cscsd-shell-compile-sco)))
  (save-window-excursion
    (cscsd-init-log-window orc-file)
    (shell-command (format (cscsd-csound-command cscsd-shell-compile-sco) 
			   (shell-quote-argument audio-file)
			   (shell-quote-argument orc-file)
			   (shell-quote-argument sco-file))
		   cscsd-csound-log-buffer-name))
  (setq cscsd-latest-audio-file audio-file))

(defun cscsd-play-scorc (orc-file sco-file)
  (save-window-excursion
    (cscsd-init-log-window orc-file)
    (shell-command (format (cscsd-csound-command cscsd-shell-play-sco) 
			   (shell-quote-argument orc-file)
			   (shell-quote-argument sco-file)) 
		   cscsd-csound-log-buffer-name)))

(defun cscsd-command-macros (omacros &optional smacros)
"Convert OMACROS and SMACROS, both lists of the form 
'\((\"MacroName\" . MacroValue) ..) into a string defining
orchestra and score macros options"
  (loop for macros in (list omacros smacros)
	for prefix in '("o" "s")
	with flags = ""
	do (loop for macro in macros
		 do (setq flags
			  (concat flags
				  (format
				   " --%smacro:%s=%s"
				   prefix
				   (car macro)
				   (cdr macro)))))
	finally return flags))

;TEST (cscsd-command-macros nil) => ""
;TEST (cscsd-command-macros '(("SomeName" . 1)("Yo" . "Hop"))) => " --omacro:SomeName=1 --omacro:Yo=Hop"
;TEST (cscsd-command-macros '(("SomeName" . 1)) '(("Yo" . "Hop"))) => " --omacro:SomeName=1 --smacro:Yo=Hop"

(defun cscsd-interpret-command 
  (syscall csd-file &optional audio-file midi-file omacros smacros)
 "Interpret SYSCALL as a shell command to be used in order 
to compile CSD-FILE into AUDIO-FILE, using midi input from MIDI-FILE.

SYSCALL must be a string containing up to three occurences of %s, 
for example: \"$csound -dHWo %s %s\"

if there is only one %s, it will stand for CSD-FILE.

if there are two of them, the first occurence of %s will be replaced 
by AUDIO-FILE, and the second %s by CSD-FILE, except if the string \"DAC\" 
is detected among the options, in which case the first %s is replaced 
with CSD-FILE and the second one with MIDI-FILE

if three occurences of %s appear, they stand for AUDIO-FILE, CSD-FILE 
and MIDI-FILE, respectively.  

OMACROS is a list of cons cells of form '\((\"MacroName\" . MacroValue) ..)
defining orchestra macros, which are appended to SYSCALL as --omacro: options.
SMACROS is a similar list defining --smacros: options, for score.

when AUDIO-FILE is nil (and required), it is built from CSD-FILE with 
an extension depending on the detected flags in the SYSCALL: .wav if a 
-W is detected, .aif if a -A, no extension otherwise.
if no option flag is provided the <CsOptions> area is analysed and the 
filename can be detected there.

when MIDI-FILE is nil (and required), the user is interactively prompted 
for a file name. 
 
SYSCALL may include the substring \"$csound\" which is then replaced by 
the returned value of function `cscsd-csound-binary'.

this function has the side effect of setting buffer-local variables 
`cscsd-latest-audio-file' and `cscsd-latest-midi-file', respectively to 
AUDIO-FILE and to the appropriate MIDI file, if any."
  (let* ((syscall (cscsd-csound-command syscall omacros smacros))
	 (buff (current-buffer))
         (out-file-candidate (cscsd-out-file-from-options))
         (out-file (or audio-file
                      (if (and (not (string-match " -" syscall)) 
			       out-file-candidate)
                          out-file-candidate
                        (concat (file-name-sans-extension 
				 (file-name-nondirectory csd-file))
                                (cscsd-guess-audio-extension syscall))))))
    (setq cscsd-latest-audio-file out-file)
    (if (= (count ?% syscall) 3)
	(format syscall
		(shell-quote-argument out-file) 
		(shell-quote-argument csd-file)
		(shell-quote-argument 
		 (cscsd-get-midi-file midi-file buff)))
      (if (= (count ?% syscall) 2)
	  (if (string-match "o[ \t]*dac" syscall) 
	      ;; audio to DAC: second %s is for MIDI-FILE
	      (format syscall
		      (shell-quote-argument csd-file)
		      (shell-quote-argument 
		       (cscsd-get-midi-file midi-file buff)))
	    (format syscall
		    (shell-quote-argument out-file) 
		    (shell-quote-argument csd-file)))
	(format syscall
		(shell-quote-argument csd-file))))))

(defun cscsd-call-csound 
  (syscall csd-file &optional audio-file midi-file omacros smacros)
  "Compile CSD-FILE into AUDIO-FILE, with midi input from MIDI-FILE.
SYSCALL is interpreted as a shell command by function `cscsd-interpret-command'.
The command output appears in a buffer named `cscsd-csound-log-buffer-name'."
  (save-window-excursion
    (setq cscsd-last-csound-invocation
	  (cscsd-interpret-command 
	   syscall csd-file audio-file midi-file omacros smacros)
	  cscsd-last-csound-output
	  (concat " ... see buffer \"" cscsd-csound-log-buffer-name "\""))
    (cscsd-init-log-window csd-file)
    (shell-command 
     (cscsd-interpret-command 
      syscall csd-file audio-file midi-file omacros smacros)
     cscsd-csound-log-buffer-name)))

(defvar cscsd-last-csound-invocation "")
(defvar cscsd-last-csound-output "") 

(defun cscsd-call-csound-to-string 
  (syscall csd-file &optional audio-file midi-file omacros smacros)
  "Compile CSD-FILE into AUDIO-FILE, with midi input from MIDI-FILE.
SYSCALL is interpreted as a shell command by function `cscsd-interpret-command'.
Return the command output"
  (setq cscsd-last-csound-invocation
	(cscsd-interpret-command 
	 syscall csd-file audio-file midi-file omacros smacros)
	cscsd-last-csound-output
	(shell-command-to-string cscsd-last-csound-invocation)))

(defun cscsd-debug-last-csound-call ()
  (interactive)
  (with-output-to-temp-buffer "*last csound call*"
    (princ cscsd-last-csound-invocation) 
    (princ "\n\n ==> \n\n")
    (princ cscsd-last-csound-output)
    t))

(defun cscsd-get-midi-file (&optional midi-file buffer)
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (if (and cscsd-latest-midi-file
	     (cscsd-usually-ask-p (format "input MIDI from %s ?"
					  cscsd-latest-midi-file)))
	cscsd-latest-midi-file
      (setq cscsd-latest-midi-file
	    (or midi-file
		(read-file-name "MIDI file:" 
				(file-name-directory 
				 (or cscsd-latest-midi-file
				     (buffer-file-name)))))))))

(defun cscsd-guess-audio-extension (syscall)
  (let ((case-fold-search nil))
    (cond ((string-match "-[^ ]*W" syscall)  
	   ".wav")
	  ((string-match "-[^ ]*A" syscall)
	   ".aif")
	  (t ""))))

;TEST (cscsd-guess-audio-extension "csound -dWo %s %s -TF %s &") => ".wav"
;TEST (cscsd-guess-audio-extension "csound -dodac -Wo %s %s -TF %s &") => ".wav"

(defun cscsd-out-file-from-options ()
  (let ((case-fold-search nil)
        (options (cscsd-Options)))
    (when options
      (if (string-match "-[^ ]*o *\"?\\([^ ]*\\)\"?" options)
        (match-string 1 options)))))

(defun cscsd-processing-menu ()
  "the menu allowing for the choice of a compilation mode"
  (if (null (listp cscsd-process-file))
    (progn
      (setq cscsd-current-processing "")
      "(single process)")
    (append
     (if (local-variable-p 'cscsd-current-processing)
       '("Select CSD processing (local)")
       '("Select CSD processing"))
     (let ((v nil))
       (unless (member cscsd-current-processing
		       (mapcar 'car cscsd-process-file))
         (setq cscsd-current-processing (caar cscsd-process-file)))
       (dolist (ap (reverse cscsd-process-file) v)
         (setq v (append 
		  (list (vector
			 (car ap)
			 (if (local-variable-p 'cscsd-current-processing)
			     `(cscsd-make-processing-local ,(car ap)) 
			   `(cscsd-set-current-processing ,(car ap)))
			 :style 'toggle
			 :selected `(equal cscsd-current-processing ,(car ap))))
		  v))))
     '("--")
     (if (local-variable-p 'cscsd-current-processing)
	 '(["Erase local value"
	    (progn
	      (kill-local-variable 'cscsd-current-processing)
	      (cscsd-build-menus)) t])
       (list (append 
	      '("Make it file-local")
	      (let (v)
		(dolist (ap (reverse cscsd-process-file) v)
		  (setq v (append
			   (list (vector
				  (car ap)
				  `(cscsd-make-processing-local ,(car ap))
				  t))
			   v))))))))))

(defun cscsd-current-processing ()
  (if (listp cscsd-process-file)
      (let (p)
	(dolist (pp cscsd-process-file p)
	  (setq p (cadr pp))
	  (if (equal (car pp) cscsd-current-processing)
	      (return p))))
    cscsd-process-file))

(defun cscsd-process (&optional csd-file audio-file midi-file processing)
  "Process CSD-FILE, or the current CSD buffer if CSD-FILE is nil.
What \"process\" exactly means here depends on the function or command 
associated to `cscsd-current-processing' in `cscsd-process-file'

If AUDIO-FILE is not provided, the audio file name is built from 
the current buffer file name or fetched from the <CsOptions> area.

When variable `cscsd-process-with-modifications' is not nil, and
when CSD-FILE is nil and the current CSD buffer has unsaved modifications,
process a temporary file created in the same directory as the edited CSD, 
leaving the current buffer untouched.

Similarly process a temporary file when CSD-FILE is nil and the current
CSD buffer has no file name."
  (interactive)
  (save-excursion
    (when (bufferp csd-file)
      (set-buffer csd-file)
      (setq csd-file (buffer-file-name csd-file)))
    (run-hooks 'csound-csd-prepare-process-hook)
    (let ((action (or processing (cscsd-current-processing))))
      (set-buffer (or (buffer-base-buffer (current-buffer))
		      (current-buffer)))
      (run-hooks 'csound-csd-before-process-hook)
      (setq csd-file
	    (or csd-file
		(if (buffer-modified-p)
		    (if cscsd-process-with-modifications
			(cscsd-make-temp-buffer-for-processing)
		      (when (y-or-n-p  
			     (format "The buffer has been modified. Update %s ?"
				     (buffer-file-name)))
			(save-buffer)
			(buffer-file-name))))
		(buffer-file-name)
		(cscsd-make-temp-buffer-for-processing)))
      (if (functionp action)
	  (condition-case nil
	      (funcall action csd-file audio-file midi-file)
	      ;; on accepte aussi les formats avec 2 voire 1 seul argument...:
	      (error (condition-case nil
			 (funcall action csd-file audio-file)
		       (error (funcall action csd-file)))))
	(when (stringp action)
	  (cscsd-call-csound action csd-file audio-file midi-file)))
      (run-hooks 'csound-csd-after-process-hook)
      (cscsd-build-menus))))

(defun cscsd-make-temp-buffer-for-processing (&optional score append)
  "Copy the current CSD buffer contents in a temporary file,
return that file name. Have this file be automatically deleted
after the next csound invocation.
If SCORE is not nil, have it replace the current score section
in the temporary buffer, or have it APPENDed to it."
  (let ((tempfile
	 (if (buffer-file-name)
	     (expand-file-name 
	      "_tmp_.csd"
	      (file-name-directory (buffer-file-name)))
	   (cscsd-temp-csd-file))))
    (set-cscsd-post-csound-log-action
     `(lambda () (delete-file ,tempfile)))
    (save-restriction
      (widen)
      (if (null score)
	  (write-region (point-min) (point-max) tempfile)
	(write-region (point-min) (if append
				      (cscsd-sco-end)
				    (cscsd-sco-beginning)) tempfile)
	(with-temp-buffer
	  (insert score)
	  (write-region (point-min) (point-max) tempfile t))
	(write-region (cscsd-sco-end) (point-max) tempfile t))
      tempfile)))

(defun cscsd-process-with-score (score &optional append)
  "Process a variation of the current CSD buffer where the score
section is replaced with SCORE \(a string).
If APPEND is not nil, SCORE is appended to the current score and 
does not replace it."
  (cscsd-process (cscsd-make-temp-buffer-for-processing score append)))

;============

(defun cscsd-clean-up-files (&rest files)
  (set-cscsd-post-csound-log-action `(lambda () 
				       (mapcar (lambda (f)
						 (when (file-exists-p f)
						   (delete-file f)))
					       ',files))))

(defun csound-process-csd (csd-file &optional omacros smacros)
  "Process CSD-FILE according to its own <Options>
If no path is specified in CSD-FILE the file is looked for
in the current directory, then in `cscsd-default-csd-directory'
OMACROS is a list of cons cells of form '\((\"MacroName\" . MacroValue) ..)
defining orchestra macros, SMACRO a similar list for score macros."
  (let ((file (expand-file-name csd-file)))
    (unless (file-exists-p file)
      (setq file (expand-file-name csd-file (cscsd-default-csd-directory))))
    (if (file-exists-p file)
	(cscsd-process file nil nil 
		       (cscsd-csound-command "$csound %s &" omacros smacros))
      (error "file not found"))))

(defmacro with-cscsd-processing (audiofile midifile processing &rest body)
  ""
  (declare (indent 2))
  `(let* ((rootfile (make-temp-file "cscsd-"))
          (csdfile (concat rootfile ".csd")))
     (with-temp-file csdfile
       ,@body)
     (cscsd-clean-up-files csdfile rootfile)
     (cscsd-process csdfile ,audiofile ,midifile ,processing)))

(defmacro with-csound-to-DAC (&rest body)
  (declare (indent 1))
  `(with-cscsd-processing nil nil "$csound -dodac %s &" ,@body))
  
(defmacro with-csound-to-WAV (wavfile &rest body)
  (declare (indent 2))
  `(with-cscsd-processing ,wavfile nil "$csound -dWo %s %s &" ,@body))

(defmacro with-csound-to-audio-editor (wavfile &rest body)
  (declare (indent 2))
  `(with-cscsd-processing ,wavfile nil 
     (lambda (csd-file output-file)
       (cscsd-call-csound "$csound -dWo %s %s" csd-file output-file)
       (cscsd-edit-audio))
     ,@body))

;============

(defun cscsd-process-region (&optional beg end)
  "Process the region as a full csd, with `cscsd-process'"
  (interactive "r")
  (let ((this-buf (current-buffer))
	(sco-deb (save-excursion (cscsd-goto-sco)
				 (re-search-forward "^i")
				 (beginning-of-line) (point)))
	(sco-fin (cscsd-sco-end)))
    (if (and (>= beg sco-deb) (<= end sco-fin))
	(progn
	  (with-temp-file (cscsd-temp-csd-file)
	    (insert-buffer-substring this-buf 1 sco-deb)
	    (insert "\n")
	    (save-excursion
	      (insert-buffer-substring this-buf beg end)
	      (insert "\ne\n")
	      (insert-buffer-substring this-buf sco-fin))
	    ;; time offset:
	    (if (re-search-forward "^i" nil t)
		(progn
		  (scomx-goto-column 2)
		  (scomx-select-column "" t)
		  (let ((var-m (scomx-grab-matrix ""))
			(var-toff 0))
		    (unless 
			(equal 0 (setq var-toff 
				       (read-calc-expr-from-minibuffer
					"time offset : "
					(number-to-string
					 (* -1 (with-defmath (min-value m)))))))
		      (scomx-yank-matrix (with-raw-defmath (+ m toff))
					 ""))))))	    
	  (cscsd-process (cscsd-temp-csd-file)))
      (message "Incorrect region: should be a score part"))))

(defun cscsd-process-scorc-via-temp-csd ()
  (interactive)
  (save-window-excursion
    (cscsd-wrap-buffer)
    (cscsd-process)
    (kill-buffer (current-buffer))))

(defun cscsd-play-audio (&optional audio-file)
  "Play AUDIO-FILE or the file stored in variable `cscsd-latest-audio-file', 
using the command in `cscsd-shell-play-audio'"
  (interactive)
  (if (or audio-file (cscsd-audio-available-p))	     
    (save-window-excursion
      (shell-command
       (format (substitute-in-file-name cscsd-shell-play-audio)
               (expand-file-name (or audio-file
                                     cscsd-latest-audio-file) 
                                 (cscsd-actual-sfdir)))))
    (message "don't know what file to play !")))

(defun cscsd-edit-audio (&optional audio-file)
  "Edit AUDIO-FILE or the file stored in variable `cscsd-latest-audio-file', 
using the command in `cscsd-shell-edit-audio'"
  (interactive)
  (if (or audio-file (cscsd-audio-available-p))	     
    (save-window-excursion
      (shell-command
       (format (substitute-in-file-name cscsd-shell-edit-audio)
               (expand-file-name (or audio-file
                                     cscsd-latest-audio-file) 
                                 (cscsd-actual-sfdir)))))
    (message "don't know what file to edit !")))

(defun cscsd-audio-available-p ()
  "Tests weither `cscsd-latest-audio-file' is a valid audio file name
in `cscsd-SFDIR', or else if there is in `cscsd-SFDIR' an audio file with
the same root name as the current buffer file name.
This decides weither the 'Play/Edit' items are available from the menus"
  (or (and (stringp cscsd-latest-audio-file)
	   (file-exists-p 
	    (expand-file-name cscsd-latest-audio-file (cscsd-actual-sfdir))))
      (and (null (stringp cscsd-latest-audio-file))
           (buffer-file-name)
	   (file-exists-p (cscsd-actual-sfdir))
	   (setq cscsd-latest-audio-file 
		 (car (directory-files (cscsd-actual-sfdir) nil
                        (format "^%s\\(\\.wav\\|\\.aif\\|$\\)"
                                (file-name-sans-extension 
				 (file-name-nondirectory 
				  (buffer-file-name))))))))))
				   

;;======================================================================
;;                   post processing actions, debugging
;;======================================================================

(defvar cscsd-post-csound-log-action ()
  "form to be evaluated right after the next invocation of csound")

(defun set-cscsd-post-csound-log-action (action)
  "Have ACTION be evaluated right after the next invocation of csound
via `cscsd-call-csound'"
  (when (and cscsd-post-csound-log-action
             (get-process cscsd-csound-log-buffer-name))
     (error "Csound is already at work"))
  (setq cscsd-post-csound-log-action action))

(defadvice shell-command-sentinel (after cscsd-scan-log (process signal))
  "Allows Csound post-processing actions"
  (when (string= cscsd-csound-log-buffer-name 
		 (buffer-name (process-buffer process)))
    (cscsd-scan-log-buffer)
    (when cscsd-post-csound-log-action 
      (funcall cscsd-post-csound-log-action)
      (setq cscsd-post-csound-log-action nil))))

(ad-activate 'shell-command-sentinel)

(defun cscsd-scan-log-buffer ()
  "Activate a few pointers in the Csound log buffer for easier debugging"
  (save-excursion
    (set-buffer cscsd-csound-log-buffer-name)
    (goto-char (point-min))
    (let* ((csd (save-excursion 
		  (when (re-search-forward "UnifiedCSD:[ ]*\\([^ \n\r]*\\)"
					   nil t)
		    (match-string-no-properties 1))))
	   (orc (save-excursion 
		  (when (re-search-forward "orchname:[ ]*\\([^ \n\r]*\\)" nil t)
		    (match-string-no-properties 1))))
           (filename (or (and csd (file-exists-p csd) (expand-file-name csd))
                         (and orc (file-exists-p orc) (expand-file-name orc))))
           (source (when filename
		     (loop for buffer in (buffer-list)
			   when (and (buffer-file-name buffer)
				     (string= filename
					      (buffer-file-name buffer)))
			   return buffer))))
      (when (or source filename)
	(while (re-search-forward "^error:.*\\(line \\(.*\\)\\):" nil t)
	  (setq source (or source
			   (save-match-data
			     (setq source (find-file-noselect filename)))))
	  (add-text-properties (match-beginning 1) (match-end 1)
	   `(mouse-face highlight
	     local-map ,(cscsd-debug-line-keymap 
			 (read (match-string 2))
			 source)))
	  (forward-line 1)
	  (add-text-properties (point) (point-at-eol)
	   `(mouse-face highlight
	     local-map ,(cscsd-debug-keymap 
			 (substring (thing-at-point 'line) 0 -1)
			 source))))))))

(defun cscsd-debug-line-keymap (number source)
  ""
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(lambda () 
                                 (interactive)
                                 (switch-to-buffer-other-window ,source)
                                 (goto-line ,number)))
    map))

(defun cscsd-debug-keymap (string source)
  ""
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(lambda () 
                                 (interactive)
                                 (switch-to-buffer-other-window ,source)
                                 (goto-char (point-max))
                                 (search-backward ,string)))
    map))

;;======================================================================
;;                             batch usage
;;======================================================================

;; csound-csd-synth.el allows Emacs to become a generic  MIDI syntheziser,
;; through this kind of shell command (Windows example here):
;;
;;   emacs.exe -batch --load=csound-csd-synth.el %1 %2 %3 %4 %5 %6 %7 %8 %9
;;
;; where: %1 is the CSD file name
;;        %2 is the rendered WAV file name
;;        %3 is the MIDI file name
;;        %4, %5, ... are optional arguments (depends on the CSD)
;;
;; see the comments in csound-csd-synth.el for further details

;;======================================================================
;;                        hide/show instruments, opcodes 
;;======================================================================

(defun cscsd-hide-instrument (name)
  (interactive)
  (cscsd-put-instrument-property name 'invisible '(csd . t)))

(defun cscsd-show-instrument (name)
  (interactive)
  (cscsd-put-instrument-property name 'invisible nil))

(defun cscsd-put-instrument-property (name prop val)
  (save-excursion
    (goto-char (point-min))
    (put-text-property 
     (re-search-forward 
      (concat "^[ \t]*\\(instr\\|opcode\\)[ \t]+" name "\\>"))
     (progn (re-search-forward "^[ \t]*\\(endin\\|endop\\)")
            (forward-line -1)
            (point-at-eol))
     prop
     val)))

(defun cscsd-instrument-invisible-p (name)
  (save-excursion
    (goto-char (point-min))
    (get-text-property 
     (re-search-forward 
      (concat "^[ \t]*\\(instr\\|opcode\\)[ \t]+" name "\\>"))
     'invisible)))

(defun cscsd-beginning-of-instrument (name)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^[ \t]*\\(instr\\|opcode\\)[ \t]+" name "\\>"))
    (point-at-bol)))

(defun cscsd-end-of-instrument (name)
  (save-excursion
    (goto-char (cscsd-beginning-of-instrument name))
    (re-search-forward "^[ \t]*\\(endin\\|endop\\)")
    (point-at-eol)))

(defun cscsd-hideshow-instrument (&optional name)
  (interactive)
  (setq name (or name (cscsd-this-instrument)))
  (when name
    (if (cscsd-instrument-invisible-p name)
      (cscsd-show-instrument name)
      (cscsd-hide-instrument name))))

;(define-key csound-orc-mode-map [home] 'cscsd-hideshow-instrument)
;(define-key csound-orc-mode-map [f2] 'cscsd-hideshow-instrument)


(defun cscsd-instruments (&optional up-to)
  (let (iando)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward 
	      "^[ \t]*\\(instr\\|opcode\\)[ \t]+\\([0-9a-zA-Z,$]*\\)" 
	      up-to t)
        (add-to-list 'iando (match-string-no-properties 2) t)))
    iando))

(defun cscsd-this-instrument ()
  (car (last (cscsd-instruments (point-at-eol)))))

(defun cscsd-hide-instruments ()
  (interactive)
  (dolist (i (cscsd-instruments))
    (cscsd-hide-instrument i)))
    
(defun cscsd-show-instruments ()
  (interactive)
  (dolist (i (cscsd-instruments))
    (cscsd-show-instrument i)))
    
(defun cscsd-hideshow-instruments ()
  (interactive)
  (dolist (i (cscsd-instruments))
    (cscsd-hideshow-instrument i)))

(defun cscsd-instr-alone ()
  "Create an indirect buffer narrowing to a single instrument
in the current orchestra"
  (interactive)
  (let* ((instrument (cscsd-this-instrument))
         (beg (cscsd-beginning-of-instrument instrument))
         (actual-beg (save-excursion (goto-char beg)
                                     (cscsd-skip-comments-backward)
                                     (point)))
         (end (cscsd-end-of-instrument instrument)))
    (switch-to-buffer
     (make-indirect-buffer 
      (current-buffer)
      (save-excursion
	(goto-char beg)
	(re-search-forward "\\(\\(instr\\|opcode\\)[ \t]+[^\n;/ \t]*\\)")
	(format "<%s> in %s" (match-string 1) (buffer-name))) t))
    (unless cscsd-use-MMM
      (csound-orc-mode))
    (when (or (> (point) end) (< (point) actual-beg))
      (goto-char (point-min)))
    (narrow-to-region actual-beg end)))

;;======================================================================
;;                        interactive functions 
;;======================================================================

(defun cscsd-edit-csoundrc ()
  (interactive)
  (find-file-other-window (cscsd-actual-csoundrc))
  (unless (file-exists-p (cscsd-actual-csoundrc))
    (let ((help (shell-command-to-string 
		 (format "\"%s\" -h" (cscsd-csound-binary)))))
      (when (string-match "^flag defaults.*" help)
	(insert ";; " (match-string 0 help))))))

(defun cscsd-make-processing-local (str)
  (interactive)
  (make-local-variable 'cscsd-current-processing)
  (setq cscsd-current-processing str)
  (when (and
	 (cscsd-non-indirect-buffer-p)
	 (y-or-n-p "Hard-code local value in buffer ?"))
    (save-excursion
      (goto-char (point-min))
      (insert ";; -*- cscsd-current-processing : \""
	      cscsd-current-processing "\" -*-\n")
      (message "hardcoded current processing value at top of buffer...")))
  (cscsd-build-menus))

(defcustom cscsd-MIDI-Options "-dWo %s.wav -TF %s"
  "Options for compiling a MIDI file 
these are inserted from the Csd menu
there should be two instances of %s, the first one refering to the root name
of the rendered audio file; the second one refering to the MIDI file name"
  :type ' string
  :group 'csound-csd)

(defun cscsd-insert-MIDI-options (&optional midi-file)
  (interactive)
  (setq midi-file (cscsd-get-midi-file))
  (save-excursion
    (cscsd-goto-Options)
    (kill-region (point)
		 (progn (search-forward "</CsOp") (backward-char 6) (point)))
    (insert "\n"
	    (format cscsd-MIDI-Options 
		    (file-name-nondirectory
		     (file-name-sans-extension midi-file)) midi-file)
	    "\n")))

(defcustom cscsd-hide-storage-areas t
  "When non-nil, the contents of <CsFileB> and <WIDGET> areas is hidden"
  :type 'boolean
  :group 'csound-csd)

(defun cscsd-hide-storage-areas ()
  (interactive)
  (dolist (area '("CsFileB" "WIDGET"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "^<%s.*$" area) nil t)
	(if (string= area "WIDGET") (forward-line 1))
	(put-text-property 
	 (point) 
	 (- (search-forward (format "</%s>" area)) (+ (length area) 3))
	 'invisible
	 (if cscsd-hide-storage-areas 'csd nil))))))

(defun cscsd-delete-area (area-name)
  (goto-char (point-min))
  (if (and (search-forward (format "<%s>" area-name) nil t)
	   (search-forward (format "</%s>" area-name) nil t))
      (progn
	(delete-region (save-excursion
			 (search-backward (format "<%s>" area-name)))
		       (point))
	t)
    nil))

(defun cscsd-untabify-comment ()
  (save-excursion
    (when (search-forward ";" (point-at-eol) t)
      (untabify (point) (point-at-eol)))))

(defun cscsd-remove-breaks (&optional start end)
  "Remove all \ endline separators from the region."
  (interactive "r")
  (setq start (or start (point-min))
	end (or end (point-max)))
  (save-excursion
    (goto-char end)
    (while (re-search-backward "\\\\[ \t]*$" start t)
      (replace-match "")
      (delete-char 1)
      (when (thing-at-point-looking-at "[ \t]+")
	(replace-match " ")))))

(defun cscsd-break-line (cont-tab &optional comment)
  (untabify (point-at-bol) (point-at-eol))
  (save-excursion
    (goto-char (point-at-bol))
    (while (re-search-forward "\\\\[ \t]*$" (1+ (point-at-eol)) t)
      (replace-match "")
      (delete-char 1)
      (forward-char 1)))
  (save-excursion
    (goto-char (point-at-bol))
    (when (and (not comment)
	       (save-excursion
		 (re-search-forward "\\(;.*\\)$" (point-at-eol) t)))
      (setq comment (match-string 1))
      (replace-match "" nil nil nil 1)
      (goto-char (point-at-bol)))
    (forward-char csound-x-break-column)
    (if (= (current-column) csound-x-break-column)
	(let ((dp (skip-chars-backward " \t")))       
	  (if (re-search-forward "[ \t]+$" (point-at-eol) t)
	      (progn 
		(replace-match "")
		(when comment (insert comment)))
	    (unless (looking-at "[ \t]")
	      (setq dp
		    (- dp
		       (- (point)
			  (re-search-backward "[ \t]" (point-at-bol))))))
	    (insert (make-string (abs dp) ? ) "\\\n")
	    (just-one-space cont-tab)
	    (cscsd-break-line cont-tab comment)))
      (forward-char (* -1 csound-x-break-column))
      (goto-char (point-at-eol))
      (when comment (insert " " comment)))))

(defun cscsd-insert-ftable (ft-line &optional with-backup)
  "Insert FT-LINE, a score ftable statement, into the current buffer.
If the buffer is a score insert at beginning, if it is a CSD insert at
the beginning of the score section, else insert at point.
If the f-table is already defined in score or CSD then either remove it or,
if WITH-BACKUP is not nil, comment it out."
  (let ((ft-number (car (split-string (substring ft-line 1)))))
    (if (or (cscsd-buffer-is-a-csd-p)
	    (cscsd-buffer-is-a-sco-p))
	(save-excursion
	  (goto-char (point-min))
	  (when (cscsd-buffer-is-a-csd-p)
	    (cscsd-goto-sco))
	  (if (re-search-forward (format "^f[ \t]*%s[ \t].*$" ft-number) nil t)
	      (replace-match 
	       (if with-backup
		   (format ";backup; %s\n%s" (match-string 0) ft-line)
		 ft-line))
	    (insert ?\n ft-line ?\n)))
      (insert ft-line))))

(defun cscsd-insert-file (&optional filename name)
  "Insert a B64 encoded file area
This create a <CsFileB filename=NAME>..</CsFileB> area,
where NAME defaults to FILENAME without its path if its nil
The variable `cscsd-hide-storage-areas' controls wheither this area is hidden"
  (interactive "fFile name:")
  (setq name (or name (file-name-nondirectory filename)))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward 
	 (format "<CsFileB[ \t]+filename=%s>" (regexp-quote name)) nil t)
	(progn
	  (delete-region (search-backward "<CsFileB") 
			 (search-forward "</CsFileB>"))
          (insert (format "<CsFileB filename=%s>\n" name)))
      (cscsd-go-after-score)
      (insert (format "\n\n<CsFileB filename=%s>\n" name)))
    (let ((deb (point)) 
	  (end (make-marker)))
      (insert "\n</CsFileB>")
      (goto-char deb)
      (move-marker end (1+ deb))
      (insert-file-contents-literally filename)
      (base64-encode-region deb (- end 1))))
  (cscsd-hide-storage-areas))

(defun cscsd-split-buffer (&optional orc-name &optional sco-name)
  "Create a score and an orchestra from the csd"
  (interactive)
  (setq orc-name
	(or orc-name 
	    (concat (file-name-sans-extension (buffer-name)) ".orc")))
  (setq sco-name
	(or sco-name 
	    (concat (file-name-sans-extension (buffer-name)) ".sco")))
  (if (or (not (get-buffer orc-name))
	  (cscsd-usually-ask-p
	   (concat "overwrite the existing " orc-name " ?")))
      (save-excursion
	(copy-to-buffer (get-buffer-create orc-name)
			(cscsd-orc-beginning)
			(cscsd-orc-end))
	(set-buffer orc-name)
	(csound-orc-mode)
	(message (concat "created " orc-name))
	(switch-to-buffer orc-name)))
  (if (or (not (get-buffer sco-name))
	  (cscsd-usually-ask-p 
	   (concat "overwrite the existing " sco-name " ?")))
      (save-excursion
	(copy-to-buffer (get-buffer-create sco-name)
			(cscsd-sco-beginning)
			(cscsd-sco-end))
	(set-buffer sco-name)
	(csound-sco-mode)
	(message (concat "created " sco-name))
	(switch-to-buffer-other-window sco-name))))

(defun cscsd-skip-comments-backward ()
  (forward-line -1)
  (beginning-of-line)
  (while (and (looking-at "^[ \t]*\\($\\|;\\|/\\*\\)")
	      (> (point) (point-min)))
    (forward-line -1))
  (forward-line 1))

(defun cscsd-orc-alone ()
  "Create an indirect buffer narrowing to the orchestra section"
  (interactive)
  (switch-to-buffer (make-indirect-buffer 
		     (current-buffer)
		     (concat "orchestra in " (buffer-name)) t))
  (unless cscsd-use-MMM
    (csound-orc-mode))
  (setq header-line-format nil)
  (narrow-to-region (cscsd-orc-beginning) 
		    (cscsd-orc-end)))

(defun cscsd-sco-alone ()
  "Create an indirect buffer narrowing to the score section"
  (interactive)
  (switch-to-buffer (make-indirect-buffer 
		     (current-buffer)
		     (concat "score in " (buffer-name)) t))
  (unless cscsd-use-MMM
    (csound-sco-mode))
  (setq header-line-format nil)
  (narrow-to-region (cscsd-sco-beginning) 
		    (cscsd-sco-end)))

(defun cscsd-edit-indirectly ()
  "Create indirect buffers for score and orchestra, and display them.
If an <ELISP> area is present, it also appears in an indirect buffer"
  (interactive)
  (save-excursion
    (cseel-edit-indirectly))
  (let ((sco-buffer (save-excursion (cscsd-sco-alone))))
    (cscsd-orc-alone)
    (switch-to-buffer-other-window sco-buffer)))

(defun cscsd-wrap-buffer ()
  "Associate the current buffer to a sco or orc file and create a csd;
if `next-window' displays a complementary buffer, it is proposed as default
otherwise we look for a complementary buffer with the same root name
or else we look for a complementary file in the same directory
... and if all of this fails we just ask.

do nothing if we already are in a csd buffer"
  (interactive)
  (unless (cscsd-buffer-is-a-csd-p)
    (let* ((comp-ext (if (cscsd-buffer-is-a-sco-p) ".orc" ".sco"))
	   (root-name (file-name-sans-extension (buffer-name)))
	   (root-file-name (file-name-sans-extension (buffer-file-name)))
	   found-buffer found-file tmp)
      ;; first look for a complementary buffer visible in the frame
      (if (and (null (one-window-p t))
	       (setq tmp (save-window-excursion 
			   (window-buffer (other-window 1))))
	       (or (and (cscsd-buffer-is-a-sco-p)
			(cscsd-buffer-is-an-orc-p tmp))
		   (and (cscsd-buffer-is-an-orc-p)
			(cscsd-buffer-is-a-sco-p tmp))))
	  (setq found-buffer tmp)
	;; .. then look for a complementary buffer of same root name
	(if (and root-name
		 (setq tmp (get-buffer (concat root-name comp-ext))))
	    (setq found-buffer tmp)))
      ;; else search for an appropriate file with same root name 
      (if (and root-file-name
	       (file-exists-p (setq tmp (concat root-file-name comp-ext))))
	  (setq found-file tmp))
      ;; now construct the CSD interactively
      (if (cscsd-buffer-is-a-sco-p)
	  (cscsd-wrap
           (if (and found-buffer 
                    (cscsd-usually-ask-p 
                     (format "Orchestra: use buffer %s ? " found-buffer)))
               found-buffer
             (if (or (and found-file
                          (cscsd-usually-ask-p 
                           (format "Orchestra: insert file %s ? " found-file)))
                     (and (y-or-n-p "Orchestra: insert a file ?")
                          (setq found-file 
                                (read-file-name 
				 "Orchestra file: "
				 (when (buffer-file-name)
				   (file-name-directory (buffer-file-name)))))))
                 found-file
               "no orchestra"))
           (current-buffer)
           root-name)
        (if (cscsd-buffer-is-an-orc-p)
            (cscsd-wrap 
             (current-buffer)
             (if (and found-buffer
                      (cscsd-usually-ask-p 
                       (format "Score: use buffer %s ? " found-buffer)))
                 found-buffer
               (if (or (and found-file 
                            (cscsd-usually-ask-p 
                             (format "Score: insert file %s ? " found-file)))
                       (and
			(y-or-n-p "Score: insert a file ?")
			(setq found-file 
			      (read-file-name
			       "Score file: "
			       (when (buffer-file-name)
				 (file-name-directory (buffer-file-name)))))))
                   found-file
                 "no score"))
             root-name))))))

(defun cscsd-goto-orc ()
  "Move point just after <CsInstruments>, return point"
  (interactive)
  (goto-char (point-min))
  (search-forward "<CsInstruments>" nil t)
  (point))

(defun cscsd-orc-beginning ()
  (save-excursion (cscsd-goto-orc)))

(defun cscsd-goto-sco ()
  "Move point just after <CsScore>, return point"
  (interactive)
  (goto-char (point-min))
  (search-forward "<CsScore>" nil t)
  (point))

(defun cscsd-sco-beginning ()
  (save-excursion (cscsd-goto-sco)))

(defun cscsd-go-after-cssynth ()
  "Move point just after </CsoundSynthesizer>, return point"
  (interactive)
  (goto-char (point-min))
  (search-forward "</CsoundSynthesizer>")
  (point))

(defun cscsd-go-after-score ()
  "Move point just after </CsScore>, return point"
  (interactive)
  (goto-char (point-min))
  (search-forward "</CsScore>")
  (point))

(defun cscsd-go-before-cssynth ()
  "Move point just before <CsoundSynthesizer>, return point"
  (interactive)
  (goto-char (point-max))
  (search-backward "<CsoundSynthesizer>")
  (point))

(defun cscsd-goto-Options ()
  "Move point just after <CsOptions>, return point.
If no <CsOptions> area exists, it is created"
  (interactive)
  (goto-char (point-min))
  (if (search-forward "<CsOptions>" nil t)
      (point)
    (goto-char (point-min))
    (search-forward "<CsoundSynthesizer>")
    (insert "\n<CsOptions></CsOptions>\n")
    (cscsd-goto-Options)))

(defun cscsd-goto-end-of-orc ()
  "Move point just before </CsInstruments>, return point"
  (interactive)
  (goto-char (point-max))
  (search-backward "</CsInstruments>" nil t)
  (point))

(defun cscsd-orc-end ()
  (save-excursion (cscsd-goto-end-of-orc)))

(defun cscsd-goto-end-of-sco ()
  "Move point just before </CsScore>, return point"
  (interactive)
  (goto-char (point-max))
  (search-backward "</CsScore>" nil t)
  (point))

(defun cscsd-sco-end ()
  (save-excursion (cscsd-goto-end-of-sco)))

(defun cscsd-current-section ()
  "Return 'orc, 'sco or 'out depending on where is the point"
  (cond ((and (>= (point) (cscsd-orc-beginning))
	      (<= (point) (cscsd-orc-end))) 'orc)
	((and (>= (point) (cscsd-sco-beginning))
	      (<= (point) (cscsd-sco-end))) 'sco)
	(t 'out)))

(defun cscsd-hs-instruments ()
  "Hide or show the orchestra section"
  (interactive)
  (let ((currently-invisible (overlay-get cscsd-orc-overlay 'invisible)))
    (if (not currently-invisible)
	(move-overlay cscsd-orc-overlay
		      (cscsd-orc-beginning)
		      (cscsd-orc-end)
		      (current-buffer)))
    (overlay-put cscsd-orc-overlay
		 'invisible (if currently-invisible nil 'csd))))

(defun cscsd-hs-score ()
  "Hide or show the score section"
  (interactive)
  (let ((currently-invisible (overlay-get cscsd-sco-overlay 'invisible)))
    (if (not currently-invisible)
	(move-overlay cscsd-sco-overlay
		      (cscsd-sco-beginning)
		      (cscsd-sco-end)
		      (current-buffer)))
    (overlay-put cscsd-sco-overlay
		 'invisible (if currently-invisible nil 'csd))))

(defun csound-csd-new (&optional name)
  "Switch to a newly created buffer with a CSD template 
in major mode `csound-csd-mode'.
NAME optionaly provides the buffer name"
  (interactive)
  (cscsd-make-template)
  (csound-csd-mode))

(defun cscsd-make-template (&optional name void)  
  (interactive)
  (if (and (get-buffer (setq name (or name "temp.csd")))
	   (cscsd-usually-ask-p
	    (concat "overwrite sections in the existing " name " ?")))
      (progn (switch-to-buffer name)
	     (kill-region (cscsd-orc-beginning) (cscsd-orc-end))
	     (kill-region (cscsd-sco-beginning) (cscsd-sco-end))
	     t)
    (unless (and (get-buffer name)
		 (null (cscsd-usually-ask-p 
                        (concat "insert within the existing "
				name " buffer ?"))))
      (switch-to-buffer (get-buffer-create name))
      (cscsd-insert-template void)
      t)))

(defcustom cscsd-sco-delimiter-regexps
  '("^[ \t]*[;/*]+[ \t]*sco\\(re\\)?[ \t]*:[ \t]*$"
    "^[ \t]*[;/*]+.*\\.sco"
    "^[ \t]*[;/*]+.*[ \t]+sco\\(re\\)?"
    "^[ \t]*f[ \t]*[0-9]+[ \t]"
    "^[ \t]*i[ \t]*[0-9]+[ \t]+[0-9]"
    "^[ \t]*i[ \t]*[0-9]+[ \t]+\\$")
  "List of regexps to be tried in order to guess where the score starts in
a plain text file known to contain a score and an orchestra with no CSD syntax
to wrap them.
This is used by `cscsd-auto-import', making it possible for example to convert
free-style text from a mail into a real CSD"
  :type '(repeat string)
  :group 'csound-csd)

(defcustom cscsd-orc-delimiter-regexps
  '("^[ \t]*[;/*]+[ \t]*orc\\(hestra\\)?[ \t]*:[ \t]*$"
    "^[ \t]*[;/*]+.*\\.orc"
    "^[ \t]*[;/*]+.*[ \t]+orc\\(hestra\\)?"
    "^[ \t]*sr[ \t]*="
    "^[ \t]*\\(instr\\|opcode\\)[ \t]")
  "List of regexps to be tried in order to guess where the orchestra starts
in a plain text file known to contain a score and an orchestra with no CSD 
syntax to wrap them.
This is used by `cscsd-auto-import', making it possible for example to convert
free-style text from a mail into a real CSD"
  :type '(repeat string)
  :group 'csound-csd)


(defun cscsd-remove-left-offset ()
  (save-excursion
    (goto-char (point-min))
    (let ((o (loop while (re-search-forward "^ +" (point-max) t)
                   minimize (length (match-string 0)) into offset
                   finally return offset)))
      (unless (or (null o) (zerop o))
        (goto-char (point-min))
        (while (re-search-forward (format "^ \\{%s\\}" o) nil t)
          (replace-match "")
          (forward-char 1))))))

(defun cscsd-import-from-clipboard ()
  "Create a csd from the content of the OS clipboard.
The function tries to guess where orchestra and score do start
by looking for lines matching the regexps listed in variables
`cscsd-orc-delimiter-regexps' and `cscsd-sco-delimiter-regexps'"
  (interactive)
  (when cscsd-import-clipboard-with-new-frame (select-frame (make-frame)))
  (switch-to-buffer
   (get-buffer-create (generate-new-buffer-name " *temp csd*")))
  (clipboard-yank)
  (cscsd-remove-left-offset)
  (goto-char (point-min))
  (cscsd-auto-import))

(defun cscsd-make-temp-name ()
  (format "%s.csd" (make-temp-name
                    (expand-file-name "auto"
                                      (cscsd-default-csd-directory)))))

(defun cscsd-auto-import (&optional csd-filename)  
  "Create a CSD from the contents of current buffer.
The function tries to guess where orchestra and score do start
by looking for lines matching the regexps listed in variables
`cscsd-orc-delimiter-regexps' and `cscsd-sco-delimiter-regexps'"
  (interactive)
  (unless (when-csd-sans-error 
	    (or csd-filename
		(set-visited-file-name (cscsd-make-temp-name))))
    (let ((raw-text (current-buffer))
          orc-beg sco-beg)
      (loop for (var regs syms)
	    in `((orc-beg ,cscsd-orc-delimiter-regexps 
			  ,cscsd-sco-delimiter-regexps)
		 (sco-beg ,cscsd-sco-delimiter-regexps 
			  ,cscsd-orc-delimiter-regexps))
            do (set var
		    (save-excursion 
		      (goto-char (point-min))
		      (loop for reg in regs
			    when (condition-case nil
				     (loop do (re-search-forward reg)
					   when (not 
						 (and
						  (loop for sreg in syms
							thereis
							(save-excursion
							  (re-search-forward 
							   sreg nil t)))
						  (loop for sreg in syms
							thereis 
							(save-excursion
							  (re-search-backward
							   sreg nil t)))))
					   return t)
				   (error nil))
			    return (point-at-bol)))))
      (if (not (and orc-beg sco-beg))
          (message "Could not figure out score from orchestra, sorry...")
        (cscsd-make-template (cscsd-make-temp-name) t)
        (cscsd-goto-orc)
        (with-current-buffer raw-text
          (copy-region-as-kill orc-beg
                               (if (< orc-beg sco-beg) sco-beg (point-max))))
        (insert ?\n)
        (yank)
        (cscsd-goto-sco)
        (with-current-buffer raw-text
          (copy-region-as-kill sco-beg
                               (if (> orc-beg sco-beg) orc-beg (point-max))))
        (insert ?\n)
        (yank)
        (goto-char (point-min))
        (with-csd-check-up
            (:dubious-score-contents (lambda (what)
                                       (cscsd-goto-sco) 
                                       (message "!! Score oddity:\n%s" what))
             :dubious-orc-contents (lambda (what)
                                     (cscsd-goto-orc)
                                     (message "!! Orchestra oddity:\n%s" what)))
          (csound-csd-mode))
        (cd (cscsd-default-csd-directory))
        (set-buffer-modified-p t)))))

(defun cscsd-at-point ()
  "Attempt to create a csd from the content of the current buffer around point"
  (interactive)
  (let (beg end)
    (save-excursion
      (loop while (re-search-backward "^[ \t]*<[^/]" nil t) 
            do (setq beg (point))
            while (and (re-search-backward "<[/a-zA-Z]+>" nil t)
                       (re-search-forward "^[ \t]*\\([a-zA-Z<0-9]\\)")
                       (string= (match-string 1) "<")
                       (= (point-at-bol) beg))
            finally return beg))
    (save-excursion
      (loop while (re-search-forward "^[ \t]*</[a-zA-Z]+>" nil t) 
            do (setq end (point))
            while (and (re-search-forward "<[/a-zA-Z]+>" nil t)
                       (goto-char (point-at-bol))
                       (re-search-backward "^[ \t]*\\([a-zA-Z<0-9]\\)")
                       (string= (match-string 1) "<")
                       (eq (search-forward ">" nil t) end))
            finally return end))
    (clipboard-kill-ring-save beg end)
    (cscsd-import-from-clipboard)))


;;======================================================================
;;                      checking-up the CSD 
;;======================================================================

(defun cscsd-check-up ()
  "Perform a few tests in order to see if we have a well-formed CSD
in the current buffer. Used by macro `with-csd-check-up'

Use \(catch 'bad-csd) to get the possible thrown error identifiers 
which are:
'(:missing-tag)
'(:dubious-score-contents match) where match is a string
'(:dubious-orc-contents match)

The function returns t if no error was found"
  (save-excursion
    ;; missing orc or sco tag
    (unless (and (save-excursion
                   (goto-char (point-min))
                   (and (search-forward "<CsInstruments>" nil t)
                        (search-forward "</CsInstruments>" nil t)))
                 (save-excursion
                   (goto-char (point-min))
                   (and (search-forward "<CsScore>" nil t)
                        (search-forward "</CsScore>" nil t))))
      (throw 'bad-csd (list :missing-tag)))
    ;; strange contents in sco section
    (cscsd-goto-sco)
    (if (or 
	 (re-search-forward
	  "^[ \t]*\\(instr\\|endin\\|opcode\\|endop\\)[ \t]+.*"
	  (cscsd-sco-end) t)
	 (re-search-forward 
	  "^[ \t]*\\(sr\\|kr\\|ksmps\\|nchnls\\)[ \t]*=.*"
	  (cscsd-sco-end) t))
      (throw 'bad-csd (list :dubious-score-contents (match-string 0))))  
    ;; strange contents in sco section
    (cscsd-goto-orc)
    (if (re-search-forward "^[ \t]*\\(f[ \t0-9]+.*\\|[es][ \t]*\\)$" 
			   (cscsd-orc-end) t)
      (throw 'bad-csd (list :dubious-orc-contents (match-string 0))))  
    ;; nothing to be signaled
    t))

(defvar cscsd-error-keywords '(:missing-tag)
  "List of keywords representing errors in the CSD structure
See `with-csd-check-up'.")

(defvar cscsd-warning-keywords '(:dubious-score-contents
                                 :dubious-orc-contents)
  "List of keywords representing warning about the current CSD
See `with-csd-check-up'.")

(defmacro with-csd-check-up (handlers &rest body)
  "Eval BODY if no problem is detected within the CSD structure
HANDLERS is a p-list defining what to do if an error is found

:any will match any keyword
:error will match any keyword in `cscsd-error-keywords'
:warning will match any keyword in `cscsd-warning-keywords'
else use the specific keywords thrown by `cscsd-check-up'

the property associated to a keyword may be either a function or some code. 
if it is a function, its arguments will be the cdr of the list returned
by `cscsd-check-up'

note that only one handler is either taken into account. 
:any has the lowest priority, then come :error and :warning, then
all other keywords.

the macro returns t if the CSD structure is correct, else nil"
  (declare (indent 1))
  (let ((condvar (make-symbol "cond"))
        (keyvar (make-symbol "key"))
        (handvar (make-symbol "handler"))
        (morevar (make-symbol "details")))
    `(let* ((,condvar (catch 'bad-csd (cscsd-check-up) ,@body nil))
            (,keyvar (car ,condvar))
            (,handvar (or (plist-get ',handlers ,keyvar)
                          (and (member ,keyvar cscsd-error-keywords)
                               (plist-get ',handlers :error))
                          (and (member ,keyvar cscsd-warning-keywords)
                               (plist-get ',handlers :warning))
                          (plist-get ',handlers :any)))
            (,morevar (cdr ,condvar)))
      (when (and ,condvar ,handvar)
        (if (functionp ,handvar)
            (apply ,handvar ,morevar)
          (eval ,handvar))
        nil)
      (unless (and ,condvar ,handvar) t))))

(defun test-csd-check-up ()
  (interactive)
  (with-csd-check-up 
      (
       :error 
       (message "Error detected")
       :warning 
       (message "Strange things detected")
       :dubious-score-contents
       (lambda (what) (message ">>> %s" what))
;       :missing-tag 
;       (message "Missing tag detected")
       :any 
       (message "Some problem detected")
       )
    (goto-char (point-max))
    (message "Went to the end no trouble")))

(defmacro when-csd-sans-error (&rest body)
  "Evaluate BODY only if `cscsd-check-up' did not find an error
Note that the macro returns t in that case, else nil"
  (declare (indent 0))
  `(with-csd-check-up (:error 'ignore)
    ,@body))


;;======================================================================
;;                      csound macros handling
;;======================================================================

(defun cscsd-macroify (beg end &optional name)
  "Transform region BEG to END into a macro NAME, move the point
in the macro definition. This works both in orchestra or score,
the #define being appended at the beginning of the corresponding section"
  (interactive "r")
  (setq name (or name (read-from-minibuffer "Macro name: ")))
  (goto-char beg)
  (kill-region beg end)
  (insert "$" name ".")
  (if (eq (cscsd-current-section) 'orc)
      (cscsd-goto-orc)
    (cscsd-goto-sco))
  (insert "\n#define " name " #")
  (save-excursion
    (yank)
    (insert "#")
    (unless (looking-at "$")
      (insert "\n\n"))))


(defun cscsd-set-macro-def (name value &optional section)
  "Set the VALUE, a string or a number, for the macro NAME
in case NAME is defined in both score and orchestra, use a third
argument SECTION to specify which one is to be affected: SECTION
can be either 'sco or 'orc.
The function returns nil if macro NAME is not found, t if it is found
You may use a nil VALUE, in which case the function simply query for
the existence of the macro."
  (save-excursion
    (cond ((eq section 'orc) (cscsd-goto-orc))
	  ((eq section 'sco) (cscsd-goto-sco))
	  (t (goto-char (point-min))))
    (unless (or (null value) (stringp value))
      (setq value (number-to-string value)))
    (when (re-search-forward
	   (concat "^[ \t]*#[ \t]*define[ \t]+" name "[ \t]+#\\([^#]*\\)#")
	   nil t)
      (when value
	(replace-match value nil t nil 1))
      t)))


(defun cscsd-define-macro (name section value)
  "#define macro NAME with value VALUE, a string or a number,
in SECTION, either 'orc or 'sco
Return the point after insertion"
  (unless (cscsd-set-macro-def name value section)
    (save-excursion
      (cond ((eq section 'orc) (cscsd-goto-orc))
	    ((eq section 'sco) (cscsd-goto-sco))
	    (t (goto-char (point-min))))
      (unless (bolp)
	(insert ?\n))
      (insert "#define " name " #" value "#\n")
      (point))))


;;======================================================================
;;                      looking for macro sources
;;======================================================================

(defun cscsd-find-include ()
  "Display in other-window the #include file referenced at point,
if it can be found"
  (interactive)
  (let ((i-file (if (thing-at-point-looking-at
		     "^[ \t]*#[ \t]*include[ \t]*\"\\(.*\\)\"")
		    (match-string 1) ""))
	fi-file)
    (if (or (file-exists-p 
	     (setq fi-file (expand-file-name 
			    i-file
			    (file-name-directory (buffer-file-name)))))
            (loop for incdir in (cscsd-full-INCDIR)
                  thereis (file-exists-p 
			   (setq fi-file (expand-file-name i-file incdir)))))
	(let ((mmode major-mode))
	  (save-excursion 
	    (find-file-other-window fi-file)
	    (when (fboundp mmode)
	      (funcall mmode)))))))

(defun cscsd-source-macro (&optional macro)
  "Display in other-window the code defining the macro at point,
if it can be found"
  (interactive)
  (if (setq macro (or macro
		  (if (thing-at-point-looking-at "[a-zA-Z0-9_]+")
		      (match-string 0) nil)))
      (let ((result (cscsd-search-macro-source macro (buffer-file-name) 
					       (when (cscsd-buffer-is-a-csd-p)
						 (cscsd-current-section)))))
	(if result
	    (progn
	      (find-file-other-window (car (last result)))
	      (goto-char (car result)))
	  (message (concat "Could not find the definition for "
			   macro ", sorry !"))))))

(defun cscsd-search-macro-source (macro in-file &optional section)
  (let (result s-file fs-file)
    (with-temp-buffer
      (insert-file-contents in-file) 
      (cond ((eq section 'orc) (cscsd-goto-orc))
	    ((eq section 'sco) (cscsd-goto-sco))
	    (t (goto-char (point-min))))  
      (if (re-search-forward (concat "#[ \t]*define[ \t]+"
				     macro "[ \t(]") nil t)
	  (setq result (list (save-excursion
			       (beginning-of-line)
			       (cscsd-skip-comments-backward)
			       (point))
			     (progn
			       (search-forward "#" nil t)
			       (search-forward "#" nil t))
			     in-file))
	(while (and (re-search-forward "#[ \t]*include[ \t]+\"\\(.*\\)\"" nil t)
		    (null result))
	  (setq s-file (match-string 1))
	  (or (file-exists-p (setq fs-file (expand-file-name 
					   s-file
					   (file-name-directory in-file))))
              (loop for incdir in (cscsd-full-INCDIR)
                    thereis
                    (file-exists-p
		     (setq fs-file (expand-file-name s-file incdir )))))
	  (setq result
		(cscsd-search-macro-source macro fs-file))))
      result)))


;;======================================================================
;;                      non-interactive functions 
;;======================================================================

(defun cscsd-buffer-is-a-csd-p (&optional buffer)
  "Test weither the current buffer is a csd, as opposed to a plain orc or sco"
  (or (save-match-data (string-match "\\.csd$" (buffer-name)))
      (save-excursion 
        (set-buffer (or buffer (current-buffer)))
        (goto-char (point-min))
        (eq major-mode 'csound-csd-mode))))

(defun cscsd-buffer-is-a-sco-p (&optional buffer)
  "Test weither the current buffer is a score"
  (or (string-match "\\.sco$" (buffer-name buffer))  
      (save-excursion
	(setq buffer (or buffer (current-buffer)))
	(eq major-mode 'csound-sco-mode))))

(defun cscsd-buffer-is-an-orc-p (&optional buffer)
  "Test weither the current buffer is a orchestra"
  (or (string-match "\\.orc$" (buffer-name buffer))
      (save-excursion
	(setq buffer (or buffer (current-buffer)))
	(eq major-mode 'csound-orc-mode))))

(defun cscsd-wrap (orc sco &optional name)  
  "Create a csd from an orc plus a sco"
  (when (cscsd-make-template (setq name (if name (concat name ".csd")
					  "temp.csd")))
    (kill-region (cscsd-orc-beginning) (cscsd-orc-end)) ;; suppress headers
    (cscsd-goto-orc)
    (insert ?\n ?\n)
    (forward-char -1)
    (if (get-buffer orc)
	(insert-buffer-substring orc)
      (if (and (not (string= orc ""))
	       (file-readable-p orc))     
	  (insert-file-contents orc)))
    (cscsd-goto-sco)
    (insert ?\n ?\n)
    (forward-char -1)
    (if (get-buffer sco)
	(insert-buffer-substring sco)
      (if (and (not (string= sco ""))
	       (file-readable-p sco))
	  (insert-file-contents sco)))
    (csound-csd-mode)
;    (set-buffer-modified-p t)
    (when (file-exists-p (cscsd-default-csd-directory))
      (set-visited-file-name 
       (expand-file-name name (cscsd-default-csd-directory)) t))))

(defun cscsd-insert-template (&optional void)
  (insert "<CsoundSynthesizer>\n<CsOptions>\n"
	  (if void "" cscsd-default-options)
	  "</CsOptions>\n\n<CsInstruments>\n"
	  (if void "" cscsd-default-orc-header)
	  "</CsInstruments>\n\n<CsScore>\n</CsScore>\n</CsoundSynthesizer>")
  (goto-char (point-min)))

(defun cscsd-set-Options (&rest bits)
  "Create or update the <Options> section
fill it with the concatenation of its arguments"
  (cscsd-goto-Options)
  (kill-region (point) (search-forward "<"))
  (insert (apply 'concat bits) "<"))

(defun cscsd-Options ()
  "Returns the current options for compilation
as defined in the <CsOptions> tag, or nil"
  (save-excursion
    (goto-char (point-min))
    (when (and (search-forward "<CsOptions>" nil t)
               (re-search-forward 
		"^\\([^;\n\r]+\\)$" 
		(save-excursion (search-forward "</CsOptions>" nil t))
		t))
      (match-string-no-properties 1))))


;;;===================================================================
;;;                      query-sheet support 
;;;===================================================================

(defvar csmode-search-query
  (copy-querymode qsheet-search-query))
(defvar csmode-search-word-query
  (copy-querymode qsheet-search-word-query))
(defvar csmode-search-exact-word-query
  (copy-querymode qsheet-search-exact-word-query))
(defvar csmode-search-regexp-query
  (copy-querymode qsheet-search-regexp-query))
(let ((l-display 
       (lambda (result)
         (save-excursion (set-buffer qsheet-source-buffer) 
			 ;; only keep one occurence per source
                         (erase-buffer))
         (if (< (length (plist-get result :in-line)) 80)
             (insert (plist-get result :in-line))
           (insert (substring (plist-get result :in-line) 0 80) "\n"))
         (let* ((orc-file (plist-get result :source))
                (sco-file (concat (file-name-sans-extension orc-file) ".sco")))
           (if (and 
		(string= (upcase (file-name-extension orc-file)) "ORC")
		(file-exists-p sco-file))
               (qsheet-insert-button
		   "play" `(lambda () (interactive)
			     (cscsd-play-scorc ,orc-file ,sco-file))
		   " ")
             (if (string= (upcase (file-name-extension orc-file)) "CSD")
               (qsheet-insert-button
		   "process" `(lambda () (interactive)
				(cscsd-process ,orc-file))
		   " "))))
         (insert " "  (plist-get result :found)
                 " --> "
                 (propertize (qsheet-name-source (plist-get result :source))
                             'mouse-face 'highlight
                             'local-map
                             (qsheet-action-keymap
                              `(lambda () (interactive)
                                (qsheet-goto-source ,(plist-get result :source)
                                 ,(plist-get result :line-number)))))
                 (format " (Line %s)\n" (plist-get result :line-number))))))
  (setf (querymode-display csmode-search-query) l-display)
  (setf (querymode-display csmode-search-word-query) l-display)
  (setf (querymode-display csmode-search-exact-word-query) l-display)
  (setf (querymode-display csmode-search-regexp-query) l-display))

(defvar cscsd-query-sheet
  (make-querysheet :buffer-name "*Csound Queries*"
		   :sources 'cscsd-search-domain
		   :modes '(csmode-search-query
			    csmode-search-word-query
			    csmode-search-exact-word-query
			    csmode-search-regexp-query))
  "Query sheet for csound-csd")

(qsheet-make-query-functions cscsd-query-sheet "cscsd-")

(defalias 'csound-search 'cscsd-csmode-search-query)
(defalias 'csound-search-word 'cscsd-csmode-search-word-query)
(defalias 'csound-search-exact-word 'cscsd-csmode-search-exact-word-query)
(defalias 'csound-search-regexp 'cscsd-csmode-search-regexp-query)

(defun cscsd-directory-orclike-files (dir)
  (directory-files dir t "\.orc$\\|\.csd$\\|.udo$"))

(defun cscsd-directory-orclike-files-recursive (dir)
  (let ((olist (cscsd-directory-orclike-files dir))
	(dirs (delete-if-not 'file-directory-p
			     (directory-files dir t "^[^.]"))))
    (dolist (dir dirs olist)
      (setq olist (append olist
			  (cscsd-directory-orclike-files-recursive dir))))))

(defun cscsd-search-domain ()
  "Return the list of all files in the orchestra library
Only the files with extensions .orc, .csd or .udo are considered"
  (let (doclist)
    (dolist (dir (cscsd-orchestras-path))
      (setq doclist
	    (append doclist (cscsd-directory-orclike-files dir))))
    (dolist (dir (cscsd-orchestras-recursive-path) doclist)
      (setq doclist
            (append doclist (cscsd-directory-orclike-files-recursive dir))))))

(defcustom cscsd-orchestras-path '()
  "List of directories to be scanned when querying through orc/csd files"
  :type '(repeat string)
  :group 'csound-x-applications-paths
  :group 'csound-csd)

(defun cscsd-orchestras-path ()
  (mapcar 'substitute-in-file-name cscsd-orchestras-path))

(defcustom cscsd-orchestras-recursive-path '()
  "List of directories to be scanned recursively
 when querying through orc/csd files"
  :type '(repeat string)
  :group 'csound-x-applications-paths
  :group 'csound-csd)

(defun cscsd-orchestras-recursive-path ()
  (mapcar 'substitute-in-file-name cscsd-orchestras-recursive-path))


;;======================================================================
;; this is it

(provide 'csound-csd)
(run-hooks 'csound-csd-load-hook)

;; csound-csd.el ends here




