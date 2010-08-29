;;; -*- auto-recompile: t -*-

;;; csound-eel.el --- emacs lisp in a CSD

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-eel.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-eel.el is distributed in the hope that it will be useful,
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

;; last modified July 15, 2009

;;; Code:

(require 'cl)
(require 'embedded-elisp-library)
(require 'csound-fltk)

(defgroup csound-eel nil
  "Emacs Lisp in a CSD"
  :group 'csound-x
  :prefix "cseel-")

;; ===================================================================
;;                          initialization
;; ===================================================================

;; defining new allowed buttons for <ELISP> areas:
(add-to-list 'embedded-buttons-alist '("export" . 'cseel-export-this-snapshot))
(add-to-list 'embedded-buttons-alist '("restore" . 'eval-this-embedded-elisp))
(add-to-list 'embedded-buttons-alist '("see/hide" . 'collapse-unfold-this-embedded-elisp))

;; and taking them into account immediately:
(eel-wake-up-embedded-buttons t)


;; ===================================================================
;;          misc. utilities (library for embedded elisp)
;; ===================================================================

(defun cseel-new-section ()
  "Go to the end of the score, insert a s statement"
  (cscsd-goto-end-of-sco)
  (unless (re-search-backward "^[ \t]*e[ \t]*$" nil t)
    (search-backward "</CsScore>" nil t)
    (save-excursion
      (insert "e\n")))
  (insert "\ns\n\n")
  (forward-line -1))


;; ===================================================================
;;                        general-purpose macros
;; ===================================================================


(defmacro csd-process-variation (&rest body)
  "Copy the current buffer in a temporary buffer, evaluate BODY there,
write the buffer in `cscsd-temp-csd-file' and process it with `cscsd-process'"
  `(let ((this-buffer (current-buffer)))
     (with-temp-file cscsd-temp-csd-file
       (insert-buffer this-buffer)
       ,@body)
     (cscsd-process cscsd-temp-csd-file)))
	   	   
(defmacro csd-process-variation-as (name &rest body)
  "Copy the current buffer in a temporary buffer, evaluate BODY there,
write the buffer in `cscsd-temp-csd-file' and process it with `cscsd-process'
the audio output is called NAME"
  `(let ((this-buffer (current-buffer)))
     (with-temp-file cscsd-temp-csd-file
       (insert-buffer this-buffer)
       ,@body)
     (cscsd-process cscsd-temp-csd-file ,name)))
     
	   	   

;; ===================================================================
;;                           meta tags queries
;; ===================================================================

(defvar cseel-meta-tag ";|"
  "sco/orc comment beginning as such will be recognized as meta instructions")

(defun cseel-meta-tag (str)
  (concat cseel-meta-tag str))

(defvar cseel-keywords '("menu" "endmenu"
                         "toggle" "endtoggle"
                         "public" "endpublic")
  "Reserved words for meta-comments")

(defun cseel-goto-tag (tag &rest tags)
  "Move to the first occurrence of `cseel-meta-tag' followed by TAGS
all TAGS should be words, with no spaces"
  (goto-char (point-min))
  (re-search-forward (concat cseel-meta-tag 
			     tag
			     (mapconcat(lambda (s)
				       (concat "[ \t]+" s)) tags "")  
			     "[ \t]*$") nil t))

(defun cseel-get-next-tag (&optional tag)
  "Move to the next occurrence of `cseel-meta-tag'
returns the associated keywords, as a list"
  (if tag
      (let (tags ok)
	(while (and (re-search-forward (concat cseel-meta-tag "\\(.*\\)") nil t)
		    (null (setq ok (string= 
				    (car (setq tags (split-string (match-string-no-properties 1))))
				    tag)))))
	(if ok tags nil))
    (if (re-search-forward (concat cseel-meta-tag "\\(.*\\)") nil t)
	(split-string (match-string-no-properties 1))))) 
	
	     
;; ===================================================================
;;                          ;|begPublic  
;;                          ;|endPublic      
;; ===================================================================

(defvar cseel-csd-frame-parameters ()
  "")
(make-variable-buffer-local 'cseel-csd-frame-parameters)

(defvar cseel-csd-public-buffers ()
  "")
(make-variable-buffer-local 'cseel-csd-public-buffers)

(defvar cseel-csd-public-frame ()
  "")
(make-variable-buffer-local 'cseel-csd-public-frame)
 
(defun cseel-display-only-public-sections ()
  (interactive)
  (let ((f (selected-frame)))
    (cseel-display-all-public-sections)
    (setq cseel-csd-frame-parameters
          (delete-if (lambda (pair)
                       (null (member (car pair) '(top left width height)))) 
                     (frame-parameters f)))
    (delete-frame f)))

(defun cseel-display-all-public-sections ()
  (cseel-kill-public-buffers)
  (text-mode)
  (save-excursion
    (goto-char (point-min))
    (let ((name "") (pb-heights nil) (pb-widths nil))
      (while (setq name (cseel-get-next-tag "begpublic"))
	(let* ((beg (point))
               (section (cscsd-current-section))
	       (end (progn (cseel-get-next-tag "endpublic")
			   (re-search-backward cseel-meta-tag)))
               (ibuff (make-indirect-buffer (current-buffer) 
				 (format "%s in CSD %s" (or (cadr name) "?") (buffer-name))
				 t)))
          (add-to-list 'cseel-csd-public-buffers ibuff t)
          (save-excursion
            (set-buffer ibuff) 
            (setq pb-heights (append pb-heights (list (+ 3 (count-lines beg end)))))
            (narrow-to-region beg end)
            (goto-char (point-min))
            (case section
              ('orc (csound-orc-mode))
              ('sco (csound-sco-mode))
              (t (csound-csd-mode)))
            (setq pb-widths (append pb-widths (list (loop do (goto-char (point-at-bol)) 
                                                          maximize (current-column) into width
                                                          do (forward-line)
                                                          until (= (point) (point-max))
                                                          finally return width)))))))
      (let ((public-buffers cseel-csd-public-buffers))
        (setq cseel-csd-public-frame
              (new-frame (list (cons 'name (format "Public Interface to %s" (buffer-name)))
                               (cons 'top (frame-parameter (selected-frame) 'top))
                               (cons 'left (frame-parameter (selected-frame) 'left))
                               (cons 'width (frame-parameter (selected-frame) 'width))
                               (cons 'height (apply '+ pb-heights))
                               (cons 'tool-bar-lines nil)))) 
        (select-frame cseel-csd-public-frame)
        (switch-to-buffer (car public-buffers))
        (loop for pb in (cdr public-buffers)
              for h in pb-heights
              do (progn
                   (select-window (split-window-vertically h))
                   (setq window-size-fixed t)
                   (switch-to-buffer pb)))))))

(defun cseel-kill-public-buffers ()
  (save-excursion
    (dolist (b cseel-csd-public-buffers)
      (set-buffer b)
      (set-buffer-modified-p nil)
      (kill-buffer b)))
  (setq cseel-csd-public-buffers nil))

(defun cseel-collapse-public-sections ()
  (interactive)
  (let ((base (buffer-base-buffer)))
    (add-to-list 'cseel-csd-frame-parameters (cons 'top (frame-parameter cseel-csd-public-frame 'top)))
    (add-to-list 'cseel-csd-frame-parameters (cons 'left (frame-parameter cseel-csd-public-frame 'left)))
    (select-frame (new-frame cseel-csd-frame-parameters))
    (switch-to-buffer base)
    (cseel-kill-public-buffers)
    (setq cseel-csd-frame-parameters nil)
    (delete-frame cseel-csd-public-frame)
    (setq cseel-csd-public-frame nil)
    (csound-csd-mode)))


;; ===================================================================
;;          ;|menu           ;|toggles
;;          ;|endmenu        ;|endtoggles  
;; ===================================================================
 
(defmacro with-cs-syntax (&rest body)
  "Attempt to have a correct comment syntax anywhere in a MMM-enabled csd file"
  `(with-syntax-table csound-sco-mode-syntax-table
     (let ((comment-start ";") (comment-end ""))
       ,@body)))

(defun cseel-uncomment-lines-below ()
  "Uncomment all lines between points and the next meta tag"
  (let ((beg (point-at-bol 2))
	(end (save-excursion (cseel-get-next-tag)
			     (point-at-bol))))
    (when (> (- end beg) 1)
      (with-cs-syntax (uncomment-region beg end)))))

(defun cseel-comment-lines-below ()
  "Comment all lines between points and the next meta tag"
  (unless (save-excursion (forward-line 1) (looking-at "^[ \t]*;"))
    (let ((beg (point-at-bol 2))
	  (end (save-excursion (cseel-get-next-tag)
			       (point-at-bol))))
      (when (> (- end beg) 1)
	(with-cs-syntax (comment-region beg end))))))

(defun cseel-menu-item-active-p (name item)
  "Check weither ITEM in meta menu/toggles NAME is currently uncommented"
  (save-excursion
    (or (cseel-goto-tag "menu" name)
	(cseel-goto-tag "toggles" name))
    (if (cseel-get-next-tag item)
	(null (save-excursion (forward-line 1) (looking-at "^[ \t]*;")))
      nil)))

(defun cseel-toggle-menu-item (name item)
  "Toggle commented status of ITEM in meta toggles NAME"
  (interactive)
  (save-excursion
    (cseel-goto-tag "toggles" name)
    (cseel-get-next-tag item)
    (if (cseel-menu-item-active-p name item)    
	(cseel-comment-lines-below)
      (cseel-uncomment-lines-below))))

(defun cseel-set-menu-item (name &optional item)
  "Comment out all items in meta menu/toggles NAME, then uncomment ITEM if provided"
  (interactive)
  (save-excursion
    (or (cseel-goto-tag "menu" name)
	(cseel-goto-tag "toggles" name))
    (while (null (member (car (cseel-get-next-tag)) '("endmenu" "endtoggles")))
      (cseel-comment-lines-below))
    (when item
    (or (cseel-goto-tag "menu" name)
	(cseel-goto-tag "toggles" name))
      (if (cseel-get-next-tag item)
	  (cseel-uncomment-lines-below)))))


;;;===================================================================
;;;                      ; |DO| sections 
;;;===================================================================

(defvar cseel-eval-on-processing-p nil
  "When non-nil, always evaluate all embedded elisp code before processing.
Dangerous !")

(make-variable-buffer-local 'cseel-eval-on-processing-p)
(add-hook 'csound-csd-before-process-hook 'cseel-eval-on-processing)

(defun cseel-eval-on-processing ()
  (when cseel-eval-on-processing-p
    (embedded-elisp-eval-everything)
    (basic-save-buffer)))

(defvar cseel-clean-after-processing-p nil
  "When non-nil, always remove all generated material from the DO sections
after processing.")

(make-variable-buffer-local 'cseel-clean-after-processing-p)
(add-hook 'csound-csd-after-process-hook 'cseel-clean-after-processing)

(defun cseel-clean-after-processing ()
  (when cseel-clean-after-processing-p
    (cseel-clean-do-sections)
    (basic-save-buffer)))

(defun cseel-insert-do-section (&optional code)
  "Insert a DO button at point triggering CODE, a string"
  (interactive)
  (require 'embedded-elisp-library)
  (embedded-elisp-insert-do-button (or code "()"))
  (insert ?\n (cseel-meta-tag "DONE"))
  (unless code
    (search-backward ")")))

(defvar cseel-DO-history nil
  "History associated to function `cseel-DO' user input")

(defcustom cseel-DO-templates
  (mapcar 
   'prin1-to-string 
   '((FLcolor "orange")
     (FLsetColor "green" "ihandle")
     (FLinfo "info" "some info about\\some interactive FLTK thingy" "yellow")
     (i 1 0 1 0 0)))
  "A list of templates forming the initial history for command `cseel-DO'"
  :type '(repeat string)
  :set (lambda (symb val)
	 (set symb val)
	 (setq cseel-DO-history val))
  :group 'csound-eel)

(defun cseel-DO ()
  "Prompt the user for an Emacs Lisp expression, create a DO button
evaluating this expression, and trigger it once.
The variable `cseel-DO-templates' initializes the history associated to
the user input. It can be customized to provide an initial set of templates."
  (interactive)
  (if (eq (cscsd-current-section) 'out)
      (message "DO sections can only appear in score or orchestra.")
    (cseel-insert-do-section
     (prin1-to-string
      (read-from-minibuffer "DO: " nil nil t 'cseel-DO-history)))
    (insert ?\n)
    (forward-line -2)
    (embedded-elisp-eval-do-button)))

(defun cseel-range-of-do-section ()
  (save-excursion
    (goto-char (point-at-bol))
    (embedded-elisp-goto-next-do-button)
    (search-forward 
     (cseel-meta-tag "DONE")
     (save-excursion
       (and (embedded-elisp-goto-next-do-button) (point)))
     t)))

(defun cseel-remove-do-machinery ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (embedded-elisp-goto-next-do-button)
      (kill-whole-line)
      (when (search-forward (cseel-meta-tag "DONE") 
			    (cseel-range-of-do-section) t)
	(kill-whole-line)))))

(defun cseel-clean-do-sections ()
  "Remove all material generated by DO sections"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (embedded-elisp-goto-next-do-button)
      (let ((end (cseel-range-of-do-section)))
	(forward-line 1)
	(when end
	  (kill-region (point) (1+ end)))))))

(defun cseel-eval-do-button ()
  "The actual function evaluating a DO button in a CSD buffer.
See variable `embedded-elisp-eval-do-button-function'"
  (let ((end (cseel-range-of-do-section)))
    (forward-line 1)
    (when end (kill-region (point) end))
    (eval-last-sexp nil)
    (insert ?\n (cseel-meta-tag "DONE"))
    (unless end (insert ?\n))))


;; ===================================================================
;;                         "Csd" submenu
;; ===================================================================

(defmacro cseel-menu-collect-while (test &rest body)
  "Collect a list of vectors for menu item specifications
BODY must evaluate to such a vector, or possibly to a submenu spec
BODY may also return nil, in which case no item is created
TEST is performed as long as it is true, the macro starting with point in \(point-min)"
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-min))
     (let (entries this-entry)
       (while ,test
	 (setq this-entry (progn ,@body))
	 (when this-entry
	   (add-to-list 'entries this-entry t)))
       entries)))

(defun cseel-menu ()
  (delete-if 'null
   `(("Meta-Settings"
     ["Take a snapshot" cseel-store-snapshot (cscsd-non-indirect-buffer-p)]
     ;; ----------------------------  snapshots
     ("Restore snapshot"
      ,@(cseel-menu-collect-while (re-search-forward "<ELISP>.*SNAPSHOT: \\([^ \t\n]+\\)" nil t)
	  (let ((name (match-string-no-properties 1)))
	    (vector name
		    `(when (yes-or-no-p "restore this snaphot and forget current settings ?")
                       (save-excursion
                         (goto-char (point-min))
                         (if (re-search-forward (concat "<ELISP>.*SNAPSHOT: " ,name) nil t)
                             (eval-this-embedded-elisp))))
		    t)))
     )
     "--"  
     ;; ----------------------------  ;|menu  &  ;|toggles
     ,@(let (tags)
       (cseel-menu-collect-while (setq tags (cseel-get-next-tag))
         (let ((type (car tags)))
           (when (member type '("menu" "toggles"))
             (let* ((name (cadr tags))
                    (end-menu (concat "end" type))
                    (action (if (string= type "menu")
                                'cseel-set-menu-item 
                              'cseel-toggle-menu-item))
                    item
                    (menu (list name)))
		 (save-excursion
                   (loop while (setq item (car (cseel-get-next-tag)))
                         until (string= item end-menu)
                         if (not (member item cseel-keywords))
                         do (add-to-list 'menu (vector item (list action name item)
                                                       :style 'toggle
                                                       :selected `(cseel-menu-item-active-p ,name ,item))
                                         t)
                         finally return menu)))))))
     "--"
     ;; ----------------------------  #define
     ,@(cseel-menu-collect-while (re-search-forward "^[ \t]*#[ \t]*define[ \t]*\\([^ \t#\n\r]+\\)" nil t)
	 (let ((name (match-string-no-properties 1) )
	      (section (cscsd-current-section)))
	  (vector (delete ?\n (concat "(" (if (eq section 'orc) "orc)" "sco)") " #define "
				      (car (split-string name  "(" t)))) 
		  `(progn ,(if (eq section 'orc) '(cscsd-goto-orc) '(cscsd-goto-sco))
			  (re-search-forward ,(concat "^[ \t]*#[ \t]*define[ \t]*" name "[ \t]*")))
		  t))))
     ;; ----------------------------  ;|begpublic   & ;|endpublic 
     ["Full editing mode" cseel-collapse-public-sections 
      :visible
      (and (buffer-base-buffer) (string-match ".* in CSD .*" (buffer-name)))]
     ["Public mode" cseel-display-only-public-sections 
      :visible
      (and (not (buffer-base-buffer)) (save-excursion (goto-char (point-min))
                                                      (cseel-get-next-tag "begpublic")))]
     ("Embedded Emacs Lisp"
       ["Insert DO section" cseel-DO (not (eq (cscsd-current-section) 'out))]
       ["Eval embedded Elisp when processing"
	(setq cseel-eval-on-processing-p (not cseel-eval-on-processing-p))
	:style toggle
	:selected cseel-eval-on-processing-p]
       ["Clean all DO sections" cseel-clean-do-sections t]
       ["Clean DO sections after processing"
	(setq cseel-clean-after-processing-p (not cseel-clean-after-processing-p))
	:style toggle
	:selected cseel-clean-after-processing-p]
       ["Remove all DO machinery" cseel-remove-do-machinery t]
       "--"
       ["Edit embedded Elisp indirectly" cseel-edit-indirectly t]
       ["Invoke EEL mode" embedded-elisp-mode (cscsd-non-indirect-buffer-p)]))))


(defun cseel-edit-indirectly ()
  "Create an indirect buffer narrowing to the <ELISP> section,
which is made visible if it was hidden."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (search-forward "<ELISP>" nil t)))
      (let ((buff (make-indirect-buffer (or (buffer-base-buffer (current-buffer))
					    (current-buffer))
					(concat "ELISP in " (buffer-name)) t)))
	(display-buffer buff)
	(set-buffer buff)
	(unless cscsd-use-MMM
	  (emacs-lisp-mode))
	(setq header-line-format nil
	      buffer-invisibility-spec nil)
	(goto-char (point-min))
	(search-forward "<ELISP>")
	(if (looking-at "[ \t]*;")
	    (forward-line 1))
	(narrow-to-region (point) 
			  (save-excursion (search-forward "</ELISP>")
					  (search-backward "</ELISP>")))))))

(defcustom cseel-elisp-pops-up t
  "If non-nil, the <ELISP> section appears in a pop-up frame when edited indirectly."
  :type 'boolean
  :group 'csound-csd)  

(when cseel-elisp-pops-up
  (add-to-list 'special-display-regexps "^ELISP in .*$"))


;; ===================================================================
;;                       snapshots
;; ===================================================================

(defun cseel-get-snapshot ()
  "Return the snapshot data for the current state of the csd"
  (save-excursion
    (let (tags in-tags snapshot muted)
      ;; #defines
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]*#[ \t]*define[ \t]+\\([a-zA-Z0-9]+\\)[ \t]+#\\([^#]+\\)#"
	      nil t)
	(setq snapshot (append snapshot (list (list :define (match-string-no-properties 1)
						    :value (match-string-no-properties 2)
						    :in (cscsd-current-section))))))
      ;; meta menus/ktoggles
      (goto-char (point-min))
      (while (setq tags (cseel-get-next-tag))
	(if (or (string= (car tags) "menu")
		(string= (car tags) "toggles"))
	    (while (and (setq in-tags (cseel-get-next-tag))
			(null (or (string= (car in-tags) "endmenu")
				  (string= (car in-tags) "endtoggles"))))
	      (setq snapshot
		    (append snapshot
			    (list
			     (list :menu (cadr tags) 
				   :type (car tags)
				   :item (car in-tags)
				   :active (null (save-excursion
						   (forward-line 1) 
						   (looking-at "^;"))))))))))
      ;; muted i-statements
      (goto-char (csound-sco-point-min))
      (while (re-search-forward (format "^%s \\([0-9.]+\\)" csound-sco-muter) (csound-sco-point-max) t)
        (add-to-list 'muted (read (match-string 1))))
      (when muted 
        (setq snapshot (append snapshot (list (list :mute muted)))))
      snapshot)))

(defun cseel-restore-snapshot (snapshot)    
  "Restore the snapshot at point
this function must be invoked while point is on the corresponding <ELISP> line"
  (dolist (s snapshot)
    (when (plist-get s :menu)                    ;; idiot: c'est répété pour chaque item...
      (cseel-set-menu-item (plist-get s :menu))))
  (dolist (s snapshot)
    (when (and (plist-get s :menu)
               (plist-get s :active))
      (cseel-set-menu-item (plist-get s :menu) (plist-get s :item)))
    (when (plist-get s :mute)
      (csound-sco-uncomment-all-instruments)
      (dolist (i (plist-get s :mute))
        (csound-sco-comment-instrument i)))
    (when (plist-get s :define)
      (cscsd-set-macro-def (plist-get s :define) (plist-get s :value) (plist-get s :in)))))

(defun cseel-store-snapshot (comment)
  "Write the <ELISP> area allowing the restoration of the current settings
COMMENT should be one word, the snapshot name"
  (interactive "sName: ")
  (save-excursion
    (goto-char (point-min))
    (insert "<ELISP>")
    (save-excursion
      (insert " ;SNAPSHOT: " comment " [export] [restore] [see/hide]\n"
	      "(require 'csound-eel)\n(cseel-restore-snapshot '"
	      (with-output-to-string (prin1 (cseel-get-snapshot)))
	      ")"
	      "\n</ELISP>\n\n")
      (if (and cscsd-use-MMM mmm-mode) (mmm-parse-buffer))
      (eel-wake-up-embedded-buttons))
    (embedded-elisp-mode t) ;;;;;;;;;;;;;;;;;;;;;; conflict with MMM somehow !!
    (collapse-unfold-this-embedded-elisp)))

(defun cseel-export-this-snapshot (&optional filename)   ; à terminer
  "Write a csd files with the snapshot at point restored
removes all elisp areas and meta instruction from this file
the current buffer is not changed"
  (interactive)
  (unless (string= (setq filename (or filename (read-file-name 
						"Export to csd file: "
						(file-name-directory (buffer-file-name)))))
		   "")
    (let ((buff (current-buffer))
	  (pos (point)))
      (with-temp-file filename
	(insert-buffer-substring buff)
	(goto-char pos)
	(eval-this-embedded-elisp)
	;; (eel-remove-all-elisp)      ; ... à debugger, celui-ci
	;; (cscsd-expand-all-defines)     ; devrait simplifier les #define ...
	;; (cseel-remove-all-metatags)    ; devrait simplifier les méta instructions
	))))


;; === this is it.
(provide 'csound-eel)

;; csound-eel.el ends here

