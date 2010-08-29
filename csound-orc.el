;;; -*- auto-recompile: t -*-

;;; csound-orc.el --- major mode for editing Csound orchestras

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

; ---------------
; initially based on the code by John Fitch (26 Sep 1996), version 1.16
; heavily hacked and adapted for Csound-X
; by Stef <hepta@zogotounga.net>

; last modification on October 27, 2009

(require 'csound-csd)
(require 'csound-doc)
(require 'csound-kbs)
(require 'font-lock)

(defvar csound-orc-mode-map (make-sparse-keymap) "Keymap for csound-orc-mode")

(defgroup csound-orc nil
  "csound-orc major mode settings"
  :group 'csound-x
  :prefix "csound-orc-")

(defcustom csound-orc-mode-never-indent nil
  "If t, the indentation code for csound-orc-mode is turned off."
  :type 'boolean
  :group 'csound-orc)

(defcustom csound-orc-auto-indent nil
  "If t, indentation happens at each <RET>"
  :type 'boolean
  :set (lambda (p val)
	 (custom-set-default p val)
	 (csound-orc-mode-populate-keymap))
  :group 'csound-orc)

(defcustom csound-orc-indent-buffer-remove-breaks t
  "If not nil, then csound-orc-mode will remove line breaks \  
when indenting a whole buffer" 
  :type 'boolean
  :group 'csound-orc)

(defcustom csound-orc-indent-buffer-expose-tags t
  "If not nil, then csound-orc-mode will separate tags:  
from following statements via newlines" 
  :type 'boolean
  :group 'csound-orc)

(defcustom csound-orc-base-indentation 2
  "Indentation depth for basic orchestra statements" 
  :type 'integer
  :group 'csound-orc)

(defcustom csound-orc-instr-indentation 4
  "Indentation for instr/endin/opcode/endop statements" 
  :type 'integer
  :group 'csound-orc)

(defcustom csound-orc-if-indentation 0
  "Indentation for if/else/elseif/endif statements" 
  :type 'integer
  :group 'csound-orc)

(defcustom csound-tab-stops '(12 15 22 24 26 28 30)
  "Positions for columns layout."
  :type '(repeat integer)
; :set ...... do something here 
  :group 'csound-orc)

(defcustom csound-orc-comment-col 50
  "Column at which to align comments."
  :type 'integer
  :group 'csound-orc)

;; hooks 

(defcustom csound-orc-mode-hook nil
  "Hook run when csound-orc-mode is started."
  :type 'hook
  :group 'csound-orc)

(defcustom csound-orc-mode-load-hook nil
  "Hook run when csound-orc-mode is loaded."
  :type 'hook
  :group 'csound-orc)


;;======================================================================
;                       csound-orc-mode
;;======================================================================

(defun csound-orc-mode ()
  "Major moode for editing Csound orchestras

\\{csound-orc-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map csound-orc-mode-map)
  (set-syntax-table csound-orc-mode-syntax-table)
  (setq mode-name "Csound Orchestra"
        major-mode 'csound-orc-mode)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)
  (setq comment-start "; "
        comment-end ""
        comment-column csound-orc-comment-col
        indent-line-function 'csound-orc-mode-indent)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'csound-orc-mode-comment)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*"
        csound-inline-empty-comment-pattern "^.+;+ *$"
        csound-code-level-empty-comment-pattern "^[\t ]+;; *$"
        csound-flush-left-empty-comment-pattern "^;;; *$")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (append csound-tab-stops
                              (cons csound-orc-comment-col nil)))
  ;; tool bar extension
  (csound-orc-define-tool-bar)
  ;; multiline comments:
  (add-to-list 'font-lock-extend-region-functions
	       'csound-scorc-extend-region-comment)
  ;; python code:
  (add-to-list 'font-lock-extend-region-functions
	       'csound-orc-extend-region-python)
  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(csound-font-lock-keywords
			     nil nil nil beginning-of-line))
  ;; (font-lock-mode 1)
  (csound-install-orc-menu)
  (add-hook 'activate-menubar-hook 'cscsd-maybe-refresh-menu nil t)
  (run-hooks 'csound-orc-mode-hook))

(defun csound-orc-define-tool-bar ()
  (when (boundp 'tool-bar-map)
    (defvar csound-orc-tool-bar-map (copy-sequence tool-bar-map))
    (let ((tool-bar-map csound-orc-tool-bar-map))
      (tool-bar-add-item-from-menu 
       'cscsd-process "csd-process" csound-orc-mode-map
       :enable (and (featurep 'csound-csd) (cscsd-indirect-buffer-p))))
    (set (make-local-variable 'tool-bar-map) csound-orc-tool-bar-map)))

(csound-orc-mode-populate-keymap)

(defvar csound-orc-mode-syntax-table nil "Syntax table")

(unless csound-orc-mode-syntax-table
  (setq csound-orc-mode-syntax-table
        (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?_  "w" csound-orc-mode-syntax-table))

;;======================================================================
;                       CSD integration
;;======================================================================

(defun csound-orc-point-min ()
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-orc-beginning)
    (point-min)))

(defun csound-orc-point-max ()
  (if (cscsd-buffer-is-a-csd-p)
      (cscsd-orc-end)
    (point-max)))

;;======================================================================
;                       indentation
;;======================================================================

(defun csound-orc-multiline-p ()
  "answer t if the current line is a \
prolongation"
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at ".*\\\\[ \t]*$")))      

(defun csound-orc-multiline-comment-p ()
  "answer t if the current line belongs to a /* comment */"
  (save-excursion
    (beginning-of-line)
    (let (beg end)
      (or (looking-at "^[ \t]*/\\*")
          (and (setq beg (save-excursion (re-search-backward "/\\*" nil t)))
               (not (save-excursion (re-search-backward "\\*/" beg t)))
               (setq end (save-excursion (re-search-forward "\\*/" nil t)))
               (not (save-excursion (re-search-forward "/\\*" end t))))))))

(defun csound-orc-mode-indent ()
  "indent current line"
  (interactive)
  (untabify (point-at-bol) (point-at-eol))
  (unless (or csound-orc-mode-never-indent 
              (csound-orc-multiline-comment-p)
	      (csound-orc-multiline-p))
    (save-excursion      
      (goto-char (point-at-bol))
      (unless (looking-at "[ \t]*\\(;\\|$\\|<\\)") ; don't touch non-code lines
        (save-restriction
          (narrow-to-region (point) (point-at-eol))
          (delete-region (point) (progn (back-to-indentation) (point)))
	  (cond
	   ;; Tag:
	   ;;-----
	   ((re-search-forward "^[ \t]*\\sw+:" nil t)
	    (csound-tabulate-left-comment))
	   ;; Free structures, with or without indentation:
	   ;;----------------------------------------------
           ((looking-at "[#\\$]") nil)
	   ((looking-at "\\b\\(else\\|end\\)?if\\b")
            (insert (make-string csound-orc-if-indentation ? ))
	    (csound-tabulate-left-comment))
	   ((looking-at "\\b\\(instr\\|opcode\\|endin\\|endop\\)")
	    (insert (make-string csound-orc-instr-indentation ? )))
           ;; Abbrev:
           ;;--------
           ((and (looking-at "^\\b\\(\\sw+\\)\\b$")
                 (not (member (match-string 1) csdoc-opcodes)))
            (widen)
            (re-search-forward "\\sw+")
            (csound-abbrev))
	   ;; General case:
	   ;;--------------
           (t
	    (delete-horizontal-space) ;Remove initial space
	    (if ;; opcode with no answer
		(or (and (looking-at "\\sw+")
			 (member (match-string 0) csdoc-0-opcodes))
		    ;; non-classified opcodes:
		    (and (looking-at "[^iakgwfS]")
			 (not (looking-at "[a-zA-Z0-9_.]+[ \t]*="))
			 (or (looking-at "[a-zA-Z0-9_.]+[ \t]+[^,]+[ \t]*\\([,;/]\\|$\\)")
			     (looking-at "[a-zA-Z0-9_.]+[ \t]*$"))))
		(tab-to-tab-stop)
	      ;; opcode with answer(s):
	      ;; - tabulate and skip first answer
	      (insert (make-string csound-orc-base-indentation ? ))
	      (re-search-forward "\\b[a-zA-Z0-9_.]+\\b") 
	      ;; - deal with special case $macro(...)
	      (if (looking-at "(")
		  (re-search-forward ")" nil t))
	      ;; - skip all possible other answers
	      (while (looking-at "[ \t]*,") 
		(search-forward ",")
		(skip-chars-forward " \t")
		(skip-chars-forward "a-zA-Z0-9_\."))
	      ;; - tabulate opcode
	      (delete-horizontal-space)
	      (tab-to-tab-stop))
	    ;; arguments:
	    ;; tabulate first argument if any
	    (unless (looking-at "[ \t]*;")
	      (when (re-search-forward "\\(\\b[a-zA-Z0-9_.]+\\b\\|=\\)[ \t]*" nil t)
		(csound-shrink-horizontal-space))
	      (tab-to-tab-stop))
	    ;; tabulate left comment if any
	    (csound-tabulate-left-comment))))))))

(defun csound-tabulate-left-comment ()
  "tabulate left comment in current line, if any"
  (when (search-forward ";" (point-at-eol) t)      
    (untabify (point) (point-at-eol))
    (backward-char 1)
    (csound-shrink-horizontal-space)
    (indent-to comment-column)))

(defun csound-shrink-horizontal-space ()
  (delete-horizontal-space)
  (insert " "))

(defun csound-orc-indent-buffer ()
  (interactive)
  (save-excursion
    (if csound-orc-indent-buffer-remove-breaks
        (cscsd-remove-breaks))
    (if csound-orc-indent-buffer-expose-tags
        (while (re-search-forward "^[ \t]*\\sw+:" (csound-orc-point-max) t)
          (unless (looking-at "[ \t]*$")
            (insert ?\n)))))
  (indent-region (csound-orc-point-min) (csound-orc-point-max) nil))

(defun csound-orc-mode-comment ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ";")
        0
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun csound-orc-newline-and-indent ()
  (interactive)
  (csound-orc-mode-indent)
  (insert "\n"))

;;;; Electric characters

(defun csound-colon ()                
  "Insert a colon: if it follows a label, delete the label's indentation."
  (interactive)
  (insert ":")
  (unless (or (csound-orc-multiline-comment-p)
	      (save-excursion (search-backward ";" (point-at-bol) t))
	      (not (save-excursion
		     (beginning-of-line)
		     (looking-at "[ \t]*\\(\\sw\\|\\s_\\)+:$"))))
    (csound-orc-newline-and-indent)))

;;======================================================================
;                       fontlock support
;;======================================================================

(defun csound-orc-extend-region-python ()
  (csound-scorc-extend-region "{{" "}}"))

(defun csound-scorc-extend-region-comment ()
  (csound-scorc-extend-region "/\\*" "\\*/[ \t]*$"))

(defun csound-scorc-extend-region (brgx ergx)
  (let* ((next-end-comment
	  (save-excursion
	    (goto-char font-lock-end)
	    (re-search-forward ergx nil t)))
	 (prev-beg-comment 
	  (save-excursion
	    (goto-char font-lock-beg)
	    (re-search-backward brgx nil t))))
    (when (or prev-beg-comment
	      next-end-comment)
      (let ((prev-end-comment 
	     (save-excursion
	       (goto-char font-lock-beg)
	       (re-search-backward ergx nil t)))
	    (next-beg-comment 
	     (save-excursion
	       (goto-char font-lock-end)
	       (re-search-forward brgx nil t)))
	    (changed nil))
	(when (and prev-beg-comment
		   (or (null prev-end-comment)
		       (< prev-end-comment prev-beg-comment)))
	  (setq font-lock-beg prev-beg-comment
		changed t))
	(when (and next-end-comment
		   (or (null next-beg-comment)
		       (< next-end-comment next-beg-comment)))
	  (setq font-lock-end next-end-comment
		changed t))
	changed))))

(defun csound-font-lock-keywords ()
  (list  
   ;; declarations
   `(,csound-regexp-declaration . 'csound-decls-face)
   ;; opcodes
   `(,csound-regexp-control-flow . 'csound-flow-face)
   `(,csound-regexp-IO . 'csound-inout-face)
   `(,csound-regexp-0-opcodes . 'csound-0-opcode-face)
   `(,csound-regexp-functions 1 'csound-funcs-face t)
   `(,csound-regexp-general-opcodes . 'csound-opcode-face)
   ;; misc
   '("\\b\\(sr\\|k\\(r\\|smps\\)\\|nchnls\\)\\b" . 'csound-globs-face)
   '("^[ \t]*[a-zA-Z0-9_.]+[ \t]*\\(=\\)[ \t]" (1 'csound-equals-face))
   '("^[ \t]*\\(\\b[a-zA-Z0-9_.]+\\b\\):" 1 'csound-label-face t)
   ;; variables
   '("\\<p[0-9]+\\>" . 'csound-ivar-face)
   '("\\<g?i[a-zA-Z0-9_.]+\\>" . 'csound-ivar-face)
   '("\\<g?k[a-zA-Z0-9_.]+\\>" . 'csound-kvar-face)
   '("\\<g?w[a-zA-Z0-9_.]+\\>" . 'csound-wvar-face)
   '("\\<g?k[a-zA-Z0-9_.]+\\>" . 'csound-fvar-face)
   '("\\<g?a[a-zA-Z0-9_.]+\\>" . 'csound-avar-face)
   '("\\<g?S[a-zA-Z0-9_.]+\\>" . 'csound-Svar-face)
   ;; #include, #define, #ifdef, #end
   '("^[ \t]*#[ \t]*define.*\\(#\\)" 1 'csound-define-face t)
   '("^[ \t]*#[ \t]*define.*?\\(#\\)" 1 'csound-define-face t)
   '("^[ \t]*#[ \t]*define\\([^(#\r\n]*\\)" 1 'csound-flow-face t)
   '("^[ \t]*#[ \t]*include" 0 'csound-flow-face t)
   '("^[ \t]*#[ \t]*define" 0 'csound-define-face t)
   '("^[ \t]*#" 0 'csound-define-face t)
   '("#[ \t]*$" 0 'csound-define-face t)
   '("^[ \t]*#[ \t]*undef" 0 'csound-define-face t)
   '("^[ \t]*#[ \t]*\\(ifdef.*\\|ifndef.*\\|else\\|end\\)" 0 'csound-flow-face t)
   ;; macros invocations
   '("\\$[a-zA-Z0-9_.]+" 0 'csound-flow-face t)     
   '("[(']\\(\\(i\\|p\\|gi\\)[a-zA-Z0-9_.]+\\)"  1 'csound-ivar-face t)
   '("[(']\\(g?i[a-zA-Z0-9_.]+\\)"  1 'csound-ivar-face t)
   '("[(']\\(g?k[a-zA-Z0-9_.]+\\)"  1 'csound-kvar-face t)
   '("[(']\\(g?w[a-zA-Z0-9_.]+\\)"  1 'csound-wvar-face t)
   '("[(']\\(g?f[a-zA-Z0-9_.]+\\)"  1 'csound-fvar-face t)
   '("[(']\\(g?a[a-zA-Z0-9_.]+\\)"  1 'csound-avar-face t)
   '("[(']\\(g?S[a-zA-Z0-9_.]+\\)"  1 'csound-Svar-face t)
   ;; goto label
   '("goto[ \t]*\\([a-zA-Z0-9_.]+\\)" 1 'csound-label-face t)  
   '("\\<\\(end\\)?if\\>" 0 'csound-flow-face t) 
   '("\\<elseif" 0 'csound-flow-face t) 
   '("\\<\\(else\\)?if.*\\(then\\)" 2 'csound-flow-face t) 
   ;; strings
   '("\"[^\"\n]*\""  0 'csound-string-face t)	  
   ;; comments 
   '("\\(;[^|\n\r].*$\\)" 1 'csound-comment-face t)
   '("\\(/\\*[[:ascii:]]*?\\*/\\)[ \t]*$" 1 'csound-comment-face t)
   ;; python code 
   '("{{\\([[:ascii:]]*?\\)}}" 1 'csound-python-code-face t)
   ;; meta-labels
   '(";|.*$" 0 'csound-label-face t)))

(defvar csound-font-lock-keywords (csound-font-lock-keywords )
  "Default expressions to highlight in Csound mode.")

(defgroup csound-faces nil
  "csound modes faces"
  :group 'csound-x
  :prefix "csound-")

(dolist 
    (fspec
     '( ;; face      "colour"	"background"   bold underline 
       (csound-opcode-face "orangered2" nil t nil "generic opcodes")
       (csound-define-face "red" nil t nil "#.. statements")
       (csound-inout-face "blue" nil nil t "I/O opcodes")
       (csound-string-face "magenta" nil nil nil "strings")
       (csound-globs-face "purple" nil nil nil "reserved global variables")
       (csound-funcs-face "magenta4" nil nil nil "functions")
       (csound-equals-face "red" nil t nil "=")
       (csound-label-face "red" nil nil t "labels")
       (csound-flow-face "blue" nil t nil "control flow opcodes")
       (csound-0-opcode-face "firebrick" nil nil nil "opcodes with no output")
       (csound-ivar-face "ForestGreen" nil nil nil "i-rate variables")
       (csound-kvar-face "RoyalBlue" nil nil nil "k-rate variables")
       (csound-wvar-face "DodgerBlue" nil nil nil "w-rate variables")
       (csound-fvar-face "MediumBlue" nil nil nil "f-rate variables")
       (csound-avar-face "DarkGoldenrod" nil nil nil "a-rate variables")
       (csound-Svar-face "SaddleBrown" nil nil nil "a-rate variables")
       (csound-comment-face "darkGreen" "lightYellow"  nil nil "comments")
       (csound-long-comment-face "darkGreen" nil nil nil "comments")
       (csound-python-code-face "DeepSkyBlue4" "beige" nil nil "Python code")
       (csound-decls-face "black" "yellow" t nil "instrument declaration")))
  (eval `(defface ,(nth 0 fspec) 
           '((t (:foreground ,(nth 1 fspec) 
                 :background ,(nth 2 fspec)
                 :bold ,(nth 3 fspec)
                 :underline ,(nth 4 fspec))))
           ,(format "Face for %s in csound orchestras" (nth 5 fspec))
           :group 'csound-faces)))


;;======================================================================
;                       commenting/uncommenting
;;======================================================================

(defvar csound-code-level-empty-comment-pattern nil)
(defvar csound-flush-left-empty-comment-pattern nil)
(defvar csound-inline-empty-comment-pattern nil)

(defun csound-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun csound-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) ?;
            )
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1)))

(defun csound-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
        or commenting out of the line if point is at its beginning
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger csound-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((csound-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert comment-start))

   ;; Very beginning of a nonblank line ?
   ;; Comment it out.
   ((looking-at "^[ \t]+")
    (insert ";"))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((csound-line-matches "^[^;\n]+$")
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((csound-line-matches csound-flush-left-empty-comment-pattern)
    (insert ";"))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty.
   ((csound-line-matches csound-code-level-empty-comment-pattern)
    (csound-pop-comment-level)
    (insert ";;;"))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty.
   ((csound-line-matches csound-inline-empty-comment-pattern)
    (csound-pop-comment-level)
    (tab-to-tab-stop)
    (insert ";; "))

   ;; If all else fails, insert character
   (t
    (insert ";")))
  (end-of-line))

;; (defun uncomment-region ()
;;   "Call `\\[comment-region]' with an argument"
;;   (interactive)
;;   (comment-region (point) (mark) '(t)))

;;======================================================================
;                       menu
;;======================================================================

(defun csound-install-orc-menu ()
  (easy-menu-define csound-orc-menu csound-orc-mode-map
    "Menu provided by `csound-orc-mode'"
    `("ORC"
      ["Indent Region" indent-region t]
      ["Indent Orchestra" csound-orc-indent-buffer t]
      ["Comment Region" comment-region t]
      ["Uncomment Region" uncomment-region t]
      ["Power-of-2 Region" csound-e-power t]
      "-----"
      ["Change Channels" csound-set-nchnls t]
      ["Change KSMPS" csound-set-ksmps t]
      ["Change KR" csound-set-kr t]
      ["Change SR" csound-set-definite-sr t]
      "-----"
      ["Wrap in CSD" cscsd-wrap-buffer 
       :visible (and (cscsd-non-indirect-buffer-p)
		     (not (cscsd-buffer-is-a-csd-p)))]
      ["Send to muO"  muo-get-orc :visible (featurep 'surmulot) ]
      ["Visit associated score" (find-file (funcall cscsd-associated-sco))
       :visible (and (not (cscsd-buffer-is-a-csd-p)) 
		     (funcall cscsd-associated-sco))]
      "--"
      ["Hide all instruments and opcodes" cscsd-hide-instruments t]
      ["Show all instruments and opcodes" cscsd-show-instruments t]
      ["Hide/show this instrument/opcode" 
       (cscsd-hideshow-instrument (cscsd-this-instrument)) (cscsd-this-instrument)]
      ["Edit this instrument by itself"            cscsd-instr-alone  (cscsd-this-instrument)]
      "--"
      ["Display ftgen table at point"   csft-display-ftable 
       (thing-at-point-looking-at "^.*ftgen.*")]
      ["Occur word at point"            (occur (word-at-point)) t]
      ["Source of macro at point"        cscsd-source-macro 
       (thing-at-point-looking-at "\\$[a-zA-Z0-9_]+")]
      ["Visit included file at point"    cscsd-find-include 
       (thing-at-point-looking-at "^[ \t]*#[ \t]*include.*")]
;;       "--"
;;       ,(csound-doc-submenu)
;;       ["Search orchestra library"   cscsd-csmode-search-query t]
;;       ["Re-search orchestra library"   cscsd-csmode-search-regexp-query t]
;;       ["Find all UDOs in orchestra library"   
;;        (cscsd-csmode-search-regexp-query "^[ \t]*opcode[ \t]+") t]
;;      "--"
;;       ;; the repository submenu is only computed upon request:
;;       ,(or cscsd-repository-menu-cache (cscsd-make-repository-menu))
;;       "--"
;;       ,(cscsd-processing-menu)
;;       ["Process source CSD"              (cscsd-process (buffer-base-buffer (current-buffer)))
;;        (cscsd-indirect-buffer-p)]
;;       ["Process via temp CSD" cscsd-process-scorc-via-temp-csd
;;        (and (cscsd-non-indirect-buffer-p) (not (cscsd-buffer-is-a-csd-p)))]
      "--"
      ["Compile orchestra (to DAC)"               cscsd-play-orchestra (null (cscsd-buffer-is-a-csd-p))]
      ["Compile orchestra (to file)"               cscsd-compile-orchestra (null (cscsd-buffer-is-a-csd-p))]
      ["Play file"           cscsd-play-audio (cscsd-audio-available-p)]
      ["Edit file"           cscsd-edit-audio (cscsd-audio-available-p)])))

(defun csound-e-power (start end)
  (interactive "r")
  (let ((number 
         (condition-case nil
             (expt 2 (round (log (string-to-number (buffer-substring start end)) 2))) 
           (error nil))))
    (when number
      (delete-region start end)
      (insert (format "%d" number)))))

(defun csound-orc-global-value (glob)
  (save-excursion
    (goto-char (csound-orc-point-min))
    (when (re-search-forward (concat glob "[ \t]*=[ \t]*\\([0-9]+\\)")
                             (csound-orc-point-max) t)
      (string-to-number (match-string-no-properties 1)))))

(defun csound-orc-set-global (glob val)
  (save-excursion
    (goto-char (csound-orc-point-min))
    (if (re-search-forward (concat glob "[ \t]*=[ \t]*\\([0-9]+\\)") (csound-orc-point-max) t)
        (replace-match (int-to-string val) nil nil nil 1)
      (insert glob " = " (int-to-string val) ?\n))))

(defun csound-set-sr (sr-rate &optional definite)
  "Set SR global and adjust KR according to KSMPS"
  (interactive "nSample rate: ")
  (let ((kr-rate (csound-orc-global-value "kr"))
        (ksmps (csound-orc-global-value "ksmps")))
    (csound-orc-set-global "sr" sr-rate)
    (when (and kr-rate ksmps
               (not (= sr-rate (* kr-rate ksmps))))
      (csound-set-kr (if definite sr-rate (/ sr-rate ksmps))))))

(defun csound-set-definite-sr (sr-rate)
  "Set SR global and adjust KR according to KSMPS"
  (interactive "nSample rate: ")
  (csound-set-sr sr-rate t))

(defun csound-set-kr (kr-rate &optional ksmps-ok)
  "Set KR global and adjust KSMPS according to SR"
  (interactive "nControl rate: ")
  (let ((sr-rate (csound-orc-global-value "sr"))
        (ksmps (csound-orc-global-value "ksmps")))
    (csound-orc-set-global "kr" kr-rate)
    (when (and sr-rate ksmps
               (not (= sr-rate (* kr-rate ksmps))))
      (unless ksmps-ok
        (csound-set-ksmps (/ sr-rate kr-rate) t)
        (setq ksmps (csound-orc-global-value "ksmps")
              kr-rate (csound-orc-global-value "kr")
              sr-rate (csound-orc-global-value "sr")))
      (if (not (= sr-rate (* kr-rate ksmps)))
          (csound-set-sr (* kr-rate ksmps) t)))))

(defun csound-set-ksmps (ksmps &optional kr-ok)
  "Set KSMPS global and adjust KR then possibly SR accordingly"
  (interactive "nControl samples: ")
  (let ((sr-rate (csound-orc-global-value "sr"))
        (kr-rate (csound-orc-global-value "kr")))
    (csound-orc-set-global "ksmps" ksmps)
    (when (and kr-rate sr-rate
               (not (= sr-rate (* kr-rate ksmps))))
      (unless kr-ok
        (csound-set-kr (/ sr-rate ksmps) t)
        (setq ksmps (csound-orc-global-value "ksmps")
              kr-rate (csound-orc-global-value "kr")
              sr-rate (csound-orc-global-value "sr")))
      (if (not (= sr-rate (* kr-rate ksmps)))
          (csound-set-sr (* kr-rate ksmps) t)))))

(defun csound-set-nchnls (num-chans)
  "Set nchnls"
  (interactive "nchannels: ")
  (csound-orc-set-global "nchnls" num-chans))

;;======================================================================
;                       expansion into opcode templates
;;======================================================================

(defun csound-abbrev ()
  "Expand Csound opcode"
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "^\\b\\(\\sw+\\)\\b$"))
    (let* ((abbrev (match-string 1))
           (beg (point-at-bol))
           (line (buffer-substring beg (point-at-eol))))
      (atomic-change-group
	(unless 
	    (loop for expansion 
		  in (loop for opcode 
			   in (sort (set-difference csdoc-opcodes csdoc-functions) 'string<)
			   when (string-match abbrev opcode)
			   collect opcode into expansions
			   finally return expansions)
		  if (progn
		       (delete-region beg (point))
		       (insert (csdoc-opcode :template expansion)
			       ?\n "; "(csdoc-opcode :doc expansion))
		       (let ((query-replace-map (append query-replace-map '((tab . skip)))))
			 (y-or-n-p "Accept? ")))
		  return t)
	  (message "no more completion for %s" abbrev)
	  (delete-region beg (point))
	  (insert line))))))

;;======================================================================

(provide 'csound-orc)
(run-hooks 'csound-orc-mode-load-hook)

;;; csound-orc.el ends here
