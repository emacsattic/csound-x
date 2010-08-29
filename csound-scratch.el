;; csound-scratch.el
;; unused code, for experimentation and development



(defun button-mask (beg end label color)
  (let ((map (make-sparse-keymap))
	(tov (make-overlay beg end)))
    (define-key map [mouse-1] 'button-unmask)
;    (define-key map [mouse-1] 'button-edit)
    (overlay-put tov 'mask t)
    (overlay-put tov 'display label)
    (overlay-put tov 'local-map map)
    (overlay-put tov 'rear-nonsticky t)
    (overlay-put tov 'front-nonsticky t)
    (overlay-put tov 'priority 100)
    (overlay-put tov 'face `(:background ,color :box 1))
    (overlay-put tov 'mouse-face `(:background ,color :box 2))))

(defun button-unmask ()
  (interactive)
  (delete-overlay (find-if
		   (lambda (ov)
		     (overlay-get ov 'mask))
		   (overlays-at (point)))))

(defun button-edit ()
  (interactive)
  (let ((ibuff (make-indirect-buffer (buffer-base-buffer (current-buffer)) "yo"))
	(ov (find-if
		   (lambda (ov)
		     (overlay-get ov 'mask))
		   (overlays-at (point)))))
    (switch-to-buffer-other-frame ibuff)
    (narrow-to-region (overlay-start ov) (overlay-end ov))))

;(button-mask (point-at-bol) (point-at-eol) "a" "orange")   

;(button-mask (point-at-bol) (point-at-eol) "sfjè_'-é" "grey") 


(defun cscsd-test-graphical-display ()
  (interactive)
    (let ((ibuff (make-indirect-buffer (current-buffer) "bouh")))
      (switch-to-buffer-other-frame ibuff)
      (button-mask (cscsd-orc-beginning) (cscsd-orc-end) " ORC " "orange")
      (button-mask (cscsd-sco-beginning) (cscsd-sco-end) " SCO " "pink")))