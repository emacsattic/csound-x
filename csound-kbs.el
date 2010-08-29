;;; -*- auto-recompile: t -*-

;;; csound-kbs.el --- csound-x key bindings

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-kbs.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-kbs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

(defun csound-orc-mode-populate-keymap ()
  ; comments
  (if csound-x-disable-smart-colon
      (define-key csound-orc-mode-map ":" nil)
    (define-key csound-orc-mode-map ":"        'csound-colon))
  (if csound-x-disable-smart-semicolon
      (define-key csound-orc-mode-map ";" nil)
    (define-key csound-orc-mode-map ";"        'csound-comment))
  ;; indentation keys
  (define-key csound-orc-mode-map "\t" 'csound-orc-mode-indent)
  (define-key csound-orc-mode-map "\C-j" 'csound-orc-newline-and-indent)
  (if csound-orc-auto-indent
      (define-key csound-orc-mode-map "\r" 'csound-orc-newline-and-indent)
    (define-key csound-orc-mode-map "\r" nil)))

(defun csound-sco-mode-populate-keymap ()
  ;; comment key
  (if csound-x-disable-smart-semicolon
      (define-key csound-sco-mode-map ";" nil)
    (define-key csound-sco-mode-map ";" 'csound-sco-comment))
  ;; indentation keys 
  (define-key csound-sco-mode-map "\t" 'csound-sco-indent-command)
  (define-key csound-sco-mode-map "\C-j" 'newline-and-indent)
  ;;  electric return
  (define-key csound-sco-mode-map "\r" 'cscsd-sco-electric-return))



(defun cscsd-define-key (key command &optional orc-command sco-command)
  "Define KEY for all major modes in a CSD buffer"
  (define-key csound-csd-mode-map key command)
  (define-key csound-orc-mode-map key (or orc-command command))
  (define-key csound-sco-mode-map key (or sco-command command)))


(defun csound-define-CSD-keys ()
  "Define all key bindings available from a CSD buffer
This effects keymaps for several major modes"

  ;; opcode documentation keys
  (cscsd-define-key "\C-c\C-d" 'csdoc-html-document-opcode)
  (cscsd-define-key "\C-c\C-h" 'csdoc-browse-html)
  (cscsd-define-key "\C-c\C-e" 'csdoc-fetch-opcode-example)
  (define-key csound-orc-mode-map "\C-c\C-o" 'csdoc-insert-opcode-html-template)

  ;; navigation
  (define-key csound-csd-mode-map "\C-c\C-i" 'cscsd-goto-orc)
  (define-key csound-csd-mode-map "\C-c\C-s" 'cscsd-goto-sco)
  (define-key csound-orc-mode-map "\C-c\C-i" 'cscsd-goto-orc) ;; ?? 
  (define-key csound-orc-mode-map "\C-c\C-s" 'cscsd-goto-sco) ;; ?? 
  (define-key csound-sco-mode-map "\C-c\C-i" 'cscsd-goto-orc) ;; ?? 
  (define-key csound-sco-mode-map "\C-c\C-s" 'cscsd-goto-sco) ;; ?? 

  (cscsd-define-key "\C-c\C-n" 'cscsd-show-structure)


  ;;  electric return
;  (cscsd-define-key "\C-j" 'cscsd-break-line-at-point) ......... ??

  ;; processing
  (define-key csound-csd-mode-map "\C-c\C-p" 'cscsd-process)
  (define-key csound-orc-mode-map "\C-c\C-p" 'cscsd-compile-orchestra) ;; ?? 
  (define-key csound-sco-mode-map "\C-c\C-p" 'cscsd-compile-score)  ;; ??
  (cscsd-define-key "\C-c\C-c" 'cscsd-kill-csound)

  )

;; to do: global keys
; cscsd-at-point
; cseel-DO


;; ================== this is it.
(provide 'csound-kbs)

;; csound-kbs.el ends here
