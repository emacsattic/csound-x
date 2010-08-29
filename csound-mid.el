;;; -*- auto-recompile: t -*-

;;; csound-mid.el --- MIDI files management

;; This file is not part of GNU Emacs.
;; 
;; csound-mid.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-mid.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------

;;    ...

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;;
;;; Installation:
;;;--------------
;;                  
;;   this package should be installed automatically by csound-x
;;

;; last modified February 9, 2008

;;;=====================================================================
;;; Code:

(require 'cl)
(require 'keykit-mode nil t) ; ........??
(require 'query-sheet)


(defgroup csound-mid nil
  "MIDI files management"
  :group 'csound-x
  :prefix "csmid-")

(defcustom csmid-midi-path '()
  "List of directories to be scanned for MIDI files"
  :type '(repeat string)
  :group 'csound-x-applications-paths
  :group 'csound-mid)

(defun csmid-midi-path ()
  (mapcar 'substitute-in-file-name csmid-midi-path))

(defcustom csmid-midi-deep-path '()
  "List of directories to be scanned recursively for MIDI files"
  :type '(repeat string)
  :group 'csound-x-applications-paths
  :group 'csound-mid)

(defun csmid-midi-deep-path ()
  (mapcar 'substitute-in-file-name csmid-midi-deep-path))

(defcustom csmid-midi-directory ""
  "Directory where newly created MIDI files are to be saved"
  :type 'directory
  :group 'csound-x-applications-paths
  :group 'csound-mid)

(defun csmid-midi-directory ()
  (substitute-in-file-name csmid-midi-directory))


;;;=====================================================================
;;;                    MIDI files submenu service
;;;=====================================================================

(defstruct csmid-files-submenu
  (current-file nil)
  (item-action 'ignore)
  (current-file-action 'ignore)
  (default-directory nil)
  (cache nil))
	  
(defun csmid-files-submenu-symbol (id)
  "Return the private symbol associated to public symbol ID"
  (intern (concat (symbol-name id) ":midi-files-submenu")))

(defun csmid-current-file-in-submenu (id &optional buffer)
  "Return the last MIDI file that was selected with the submenu ID in BUFFER
or nil if there is no such file"
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    ;; there are many reasons why this can fail, so let's be careful:
    (condition-case nil 
	(csmid-files-submenu-current-file (symbol-value (csmid-files-submenu-symbol id)))
      (error nil))))

;TEST (csmid-current-file-in-submenu 'some-undefined-tag) => nil


(defun csmid-midifiles-submenu (id label refresh-function &optional action dir current-action)
  "Return a list defining a submenu called LABEL featuring an item 
for each midifile in the directories `csmid-all-nonempty-directories-in-path' 
The function set-up a buffer local structure identified with symbol ID 
representing the submenu. This allows for caching the list of files; 
it also allows for multiple \"instances\" of the submenu within a given buffer, 
each of them being associated to different behaviors according to the optional
arguments:

ACTION is the function evaluated when a midi file item is operated; 
the unique argument of ACTION is the midi file name
DIR is the default initial directory when browsing the file system
CURRENT-ACTION is the function evaluated by the last item in the menu; 
this item gives access to the last MIDI file selected through the submenu: 
it is different for each ID, and also for each buffer since the whole submenu 
is buffer-local. See `csmid-current-file-in-submenu'"
  (let ((menu-name (csmid-files-submenu-symbol id)))
    (unless (and (boundp menu-name)
		 (csmid-files-submenu-p (symbol-value menu-name)))
;      (message "new definition for %s" menu-name)
      (make-variable-buffer-local menu-name)
      (set menu-name (make-csmid-files-submenu)))
    (when action
      (setf (csmid-files-submenu-item-action (symbol-value menu-name)) action))
    (when current-action
      (setf (csmid-files-submenu-current-file-action (symbol-value menu-name)) current-action))
    (when dir
      (setf (csmid-files-submenu-default-directory (symbol-value menu-name)) dir))
    (or (csmid-files-submenu-cache (symbol-value menu-name))
	(setf (csmid-files-submenu-cache (symbol-value menu-name))
	      `((,label
		 ,@(mapcar 
		    (lambda (path) 
		      (let ((files (qsheet-files-in-directories 
				    (list path) nil nil "\.midi*$")))
			`(,path
			  ,@(mapcar 
			     (lambda (midif)
			       `[,(file-name-nondirectory (file-name-sans-extension midif))
				 (progn
				   (setf (csmid-files-submenu-current-file ,menu-name) ,midif)
				   (funcall (csmid-files-submenu-item-action ,menu-name) ,midif))
				 :style toggle
				 :selected (equal (csmid-files-submenu-current-file ,menu-name)
					    ,midif)])
			     files))))
		    (csmid-all-nonempty-directories-in-path))
		 "--"
		 ["Refresh listing" (progn
				      (setq csmid-cache-all-nonempty-directories-in-path nil)
				      (setf (csmid-files-submenu-cache ,menu-name) nil)
				      (funcall (or ',refresh-function 'ignore)))
		  t]
		 ["... browse" (funcall (csmid-files-submenu-item-action ,menu-name)
					(read-file-name
					 "MIDI file:" 
					 (csmid-files-submenu-default-directory ,menu-name)))
		  t]
		 "--"
		 "current"
		 [(format "%s" (csmid-files-submenu-current-file ,menu-name)) 
		  (csmid-files-submenu-current-file-action ,menu-name) t]))))))

(defvar csmid-cache-all-nonempty-directories-in-path ()
  "A cache for the value of function `csmid-all-nonempty-directories-in-path'")

(defun csmid-all-nonempty-directories-in-path ()
  "Return the list of directories to be scanned for MIDI files
This is constructed from the values of `csmid-midi-path' and `csmid-midi-deep-path'
This value is cached in `csmid-cache-all-nonempty-directories-in-path'"
  (or csmid-cache-all-nonempty-directories-in-path
      (setq csmid-cache-all-nonempty-directories-in-path
	    (delete-if
	     (lambda (dir)
	       (or (null (file-exists-p dir))
		   (zerop (length (directory-files dir nil "\\.midi*$")))))
	     (union (qsheet-directories-under (csmid-midi-deep-path))
		    (csmid-midi-path)
		    :test 'equal)))))

;;======================================================================
;; this is it

(provide 'csound-mid)

;; csound-mid.el ends here




