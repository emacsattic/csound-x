;;; -*- auto-recompile: t -*-

;;; csound-spdb.el --- csound-csd speedbar support

;; Keywords: csound, convenience, csd

;; This file is not part of GNU Emacs.
;; 
;; csound-spdb.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-spdb.el is distributed in the hope that it will be useful,
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
;;    Speedbar support for CSD
;;
;;; Installation:
;;                  
;; ==========================================================
;; this file should be installed through the csound-x package
;; (it is required by csound-csd.el)
;; ==========================================================
;;

;; last modified March 30, 2006

;;;=====================================================================
;;; Code:


;;;===================================================================
;;;             Speedbar support (adapted from info.el)
;;;===================================================================

;; These functions allow speedbar to display the "tags" in the
;; current csd document.
(eval-when-compile (require 'speedbar))

(defun cscsd-speedbar ()
  (interactive)
  (speedbar)
  (speedbar-change-initial-expansion-list "csound-csd"))

(defvar csspdb-key-map nil
  "Keymap used when in the csound-csd- display mode.")

(defun csound-csd-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance csound-csd."
  (if csspdb-key-map
      nil
    (setq csspdb-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key csspdb-key-map "e" 'speedbar-edit-line)
    (define-key csspdb-key-map "\C-m" 'speedbar-edit-line)
    (define-key csspdb-key-map "+" 'speedbar-expand-line)
    (define-key csspdb-key-map "-" 'speedbar-contract-line))

  (speedbar-add-expansion-list '("csound-csd" csspdb-menu-items
				 csspdb-key-map
				 csspdb-hierarchy-buttons)))

(defvar csspdb-menu-items
   '(["Browse Node" speedbar-edit-line t]
     ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
 		     (looking-at "[0-9]+: *.\\+. "))]
     ["Contract Node" speedbar-contract-line
      (save-excursion (beginning-of-line)
 		     (looking-at "[0-9]+: *.-. "))])
   "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (csound-csd-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'csound-csd-install-speedbar-variables))

;;; csound-csd hierarchy display method
;;;###autoload
(defun csspdb-browser ()
  "Initialize speedbar to display a csound csd document browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into csound-csd mode on speedbar.
  (speedbar-change-initial-expansion-list "csound-csd"))

(defface cscsd-speedbar-main-section
  '((((class color) (background light)) (:foreground "medium blue" :bold t))
    (((class color) (background dark)) (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for Csound csd speedbar main sections"
  :group 'csound-csd)

(defface cscsd-speedbar-subsection
  '((((class color) (background light)) (:foreground "firebrick" :underline t))
    (((class color) (background dark)) (:foreground "orange" :underline t))
    (t (:underline t)))
  "Face for Csound csd speedbar subsections"
  :group 'csound-csd)

(defmacro csspdb-within-csd-buffer (&rest forms)
  "Execute FORMS in the attached frame's csd buffer."
  `(save-selected-window
     (if (fboundp 'speedbar-select-attached-frame) ;; Emacs 22
         (speedbar-select-attached-frame)
       (select-frame speedbar-attached-frame)) ;; Emacs 21
     ,@forms))

(defmacro csspdb-top-node (label expand-tag &optional action data)
  "private"
  `(speedbar-make-tag-line
    'bracket ?+ 'csspdb-expand-node ,expand-tag
    ,label ,(or action '()) ,(or data '())
    'cscsd-speedbar-main-section depth))

(defmacro csspdb-subnode (label expand-tag)
  "private"
  `(speedbar-make-tag-line
    'bracket ?+ 'csspdb-expand-node ,expand-tag
    ,label '() '()
    'cscsd-speedbar-subsection depth))

(defun csspdb-hierarchy-buttons (tag-type depth)
  "Display a Csound csd document tag hierarchy in speedbar.
TAG-TYPE is the type of tag to be looked at in the attached frame.
DEPTH is the current indentation depth."
  (when (stringp tag-type)
    ;;
    ;; top-level hierarchy
    ;; ---------------------
    (when (csspdb-csd-contains-p "<CsOptions")
      (speedbar-make-tag-line
       'bracket ?> 'ignore nil
       "Options" 'csspdb-goto-tag "<CsOptions>"
       'default depth))
     (csspdb-top-node "Orchestra" 'orc 'csspdb-goto-tag '(cscsd-goto-orc))
     (csspdb-top-node "Score" 'sco 'csspdb-goto-tag '(cscsd-goto-sco))
    (when (csspdb-csd-contains-p "<CsSampleB")
      (csspdb-top-node "Encoded samples" 'CsSampleB))
    (when (csspdb-csd-contains-p "<CsFileB")
      (csspdb-top-node "Encoded files" 'CsFileB))
    (when (and (featurep 'ses)
	       (csspdb-csd-contains-p "<SES"))
      (csspdb-top-node "Spreadsheets" 'SES))
    (when (and (featurep 'csound-key)
	       (featurep 'keykit-mode)
	       (csspdb-csd-contains-p "<KeyPhrase"))
     (csspdb-top-node "KeyPhrases" 'KeyPhrase))
    (when (featurep 'csound-eel)
      (when (csspdb-csd-contains-p cseel-meta-tag)
	(csspdb-top-node "Meta-comments" 'eel))
      (when (csspdb-csd-contains-p "<ELISP")
        (csspdb-top-node "Embedded Elisp" 'elisp))))
  ;;
  ;; sub levels
  ;;-----------
  (cond
   ;;
   ;; Orchestra
   ;;----------
   ;;; level 1
   ((equal tag-type 'orc)
    (when (csspdb-csd-contains-p "^[^\n;]*ftgen[ \t]")
      (csspdb-subnode "FTables" 'ftgens))
    (csspdb-subnode "Instruments" 'instruments)
    (when (csspdb-csd-contains-p "^[ \t]*opcode[ \t]+")
      (csspdb-subnode "Opcodes" 'opcodes)))
   ;;; level 2
   ((equal tag-type 'ftgens)
    (dolist (ft (csspdb-within-csd-buffer
		 (let (lf)
		   (save-excursion
		     (cscsd-goto-orc)
		     (while (re-search-forward 
                             "^[ \t]*\\(gi[a-zA-Z0-9]+\\)[ \t]+ftgen[ \t]+\\([0-9]+\\)"
                             (cscsd-orc-end) t)
		       (add-to-list 'lf (list (match-string 0) (match-string 2)) t)))
		   lf)))
      (speedbar-make-tag-line
       'bracket ?~ 'csspdb-display-table (string-to-number (cadr ft))
       (concat "ftgen " (cadr ft)) 'csspdb-goto-tag (car ft)
       'default depth)))
   ((equal tag-type 'instruments)
    (dolist (instr (csspdb-within-csd-buffer
		    (let (linstr)
		      (save-excursion
			(cscsd-goto-orc)
			(while (re-search-forward 
                                "^[ \t]*\\(instr[ \t]+\\(\\([0-9,a-zA-Z \t]+\\).*\\)\\)" 
                                (cscsd-orc-end) t)
			  (add-to-list 'linstr (list (match-string 1) (match-string 2) (match-string 3)) t)))
		      linstr)))
      (speedbar-make-tag-line
       'bracket ?~ 'csspdb-hideshow (caddr instr)
       (caddr instr) 'csspdb-re-search-this (car instr) 
       'default depth)))
   ;;
   ((equal tag-type 'opcodes)
    (dolist (opcode (csspdb-within-csd-buffer
		     (let (lopcodes)
		       (save-excursion
			 (cscsd-goto-orc)
			 (while (re-search-forward "^[ \t]*opcode[ \t]+\\(\\([^,]+\\).*\\)" 
						   (cscsd-orc-end) t)
			   (add-to-list 'lopcodes 
                                        (list (match-string 0) (match-string 1) (match-string 2))
                                        t)))
		      lopcodes)))
      (speedbar-make-tag-line
       'bracket ?~ 'csspdb-hideshow (cadr opcode)
       (caddr opcode) 'csspdb-goto-tag (car opcode)
       'default depth)))
   ;; Score
   ;;------
   ;;; level 1
   ((equal tag-type 'sco)
    (when (csspdb-csd-contains-p "^[ \t]*f")
      (csspdb-subnode "FTables" 'ftables))
    (when (csspdb-within-csd-buffer
	   (save-excursion (cscsd-goto-sco) (scomx-search-next-tag)))
      (csspdb-subnode "Matrices" 'matrices)))
   ;;; level 2
   ((equal tag-type 'ftables)
    (dolist (ft (csspdb-within-csd-buffer
		 (let (lf)
		   (save-excursion
		     (cscsd-goto-sco)
		     (while (re-search-forward "^[ \t]*\\(f[ \t]*[0-9]+\\)[ \t]"
					       (cscsd-sco-end) t)
		       (add-to-list 'lf (match-string 1) t)))
		   lf)))
      (speedbar-make-tag-line
       'bracket ?~ 'csspdb-display-table (string-to-number (delete ?f ft))
       ft 'csspdb-goto-tag ft
       'default depth)))
   ;;
   ((equal tag-type 'matrices)
    (dolist (mat (csspdb-within-csd-buffer
		  (save-excursion
		    (cscsd-goto-sco)
		    (scomx-get-all-selections))))
      (speedbar-make-tag-line 
       'bracket ?> 'csspdb-goto-tag `(scomx-goto-selection ,mat)  
       (concat "\"" mat "\"") 'csspdb-goto-tag `(scomx-goto-selection ,mat)     
       'default depth)))
   ;;
   ;; EEL
   ;;----
   ((equal tag-type 'elisp)
    (dolist (area (csspdb-fetch-markups "ELISP"))
      (speedbar-make-tag-line
       'bracket ?> 'csspdb-re-search-this "^[ \t]*<ELISP>"
       (progn (string-match "[; ]+\\([^;[]*\\)" (cadr area)) (match-string 1 (cadr area)))
       'csspdb-re-search-this "^[ \t]*<ELISP>"
       'default depth)))
   ((equal tag-type 'eel)
    (dolist (mtag (csspdb-within-csd-buffer
		   (save-excursion
		     (let (tag all-tags)
		       (goto-char (point-min))
		       (while (setq tag (cseel-get-next-tag))
			 (push (list tag (point)) all-tags))
		       (reverse all-tags)))))
      (let ((istop (member (caar mtag) '("menu" "endmenu" "toggles" "endtoggles" "begpublic" "endpublic"))))
	(speedbar-make-tag-line 
	 'bracket (if istop ?> ?_) 'csspdb-goto-metatag mtag
	 (concat (if istop "" " ") (mapconcat 'identity  (car mtag) " ")) 'csspdb-goto-metatag mtag
	 'default depth))))
   ;;
   ;; CsSampleB and CsFileB
   ;;----------------------
   ((member tag-type '(CsSampleB CsFileB))
    (dolist (area (csspdb-fetch-markups (symbol-name tag-type)))
      (speedbar-make-tag-line
       'bracket ?> 'ignore nil
       area 'ignore nil
       'default depth)))
   ;;
   ;; Spreadsheets
   ;;-------------
   ((equal tag-type 'SES)
    (dolist (area (csspdb-fetch-markups "SES"))
      (speedbar-make-tag-line 
       'bracket ?> 'csspdb-restore-ses-area (cadr area)
       (concat "\"" (cadr area) "\"") 'csspdb-restore-ses-area (cadr area)
       'default depth)))
   ;;
   ;; KeyPhrases
   ;;-----------
   ((equal tag-type 'KeyPhrase)
    (dolist (area (csspdb-fetch-key-markups))
      (speedbar-make-tag-line
       'bracket ?> 'csspdb-search-this (format "<KeyPhrase label=\"%s\">" area)
       area
       'csspdb-search-this (format "<KeyPhrase label=\"%s\">" area)
       'default depth)))))

(defun csspdb-fetch-markups (name) 
  (csspdb-within-csd-buffer
    (save-excursion
      (goto-char (point-max))
      (let (areas)
	(while (re-search-backward (concat "^[ \t]*<" name "\\([^>]*\\)>\\(.*\\)") nil t)
	  (push (list (match-string 1) (match-string 2)) areas))
	areas))))

(defun csspdb-fetch-key-markups () 
  (csspdb-within-csd-buffer
    (save-excursion
      (goto-char (point-max))
      (let (areas)
	(while (re-search-backward (concat "^[ \t]*<KeyPhrase label=\"\\(.*\\)\">") nil t)
	  (push (match-string 1) areas))
	areas))))

(defun csspdb-csd-contains-p (string) 
  (csspdb-within-csd-buffer
    (save-excursion
      (set-buffer (first (buffer-list))) ;; bizarre mais nécessaire...
      (save-excursion
	(goto-char (point-min))
	(re-search-forward string nil t)))))

(defun csspdb-search-this (text this indent)
  (csspdb-within-csd-buffer
   (goto-char (point-min))
   (search-forward this)))

(defun csspdb-re-search-this (text this indent)
  (csspdb-within-csd-buffer
   (goto-char (point-min))
   (re-search-forward this))) 

(defun csspdb-restore-ses-area (text sid indent)
  (csspdb-within-csd-buffer
   (scomx-restore-ses-area sid)))

(defun csspdb-process-KeyPhrase (text sid indent)
  (csspdb-within-csd-buffer
   (cscsd-process-KeyPhrase sid)))

(defun csspdb-hideshow (text instr indent)
  (csspdb-within-csd-buffer
   (cscsd-hideshow-instrument instr)))

(defun csspdb-goto-metatag (text tag indent)
  (csspdb-within-csd-buffer
   (goto-char (cadr tag))))

(defun csspdb-goto-tag (text tag indent)
  "When user clicks on TEXT, go to a tag TAG.
The INDENT level is ignored."
  (csspdb-within-csd-buffer
   (goto-char (point-min))
   (if (stringp tag)
       (re-search-forward (concat "^[ \t]*" tag))
     (eval tag))))

(defun csspdb-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is the data related to this node.
INDENT is the current indentation depth."
  (cond ((string-match "+" text)	;we have to expand this node
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (csspdb-hierarchy-buttons token (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t ()))
;	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun csspdb-display-table (text token indent)
  (csspdb-within-csd-buffer
   (csft-display-ftable token)))


;;======================================================================
;; this is it

(provide 'csound-spdb)

;; csound-spdb.el ends here




