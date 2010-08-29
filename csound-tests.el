;;; -*- auto-recompile: t -*-

;;; csound-tests.el --- testing Csound-X

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-tests.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-tests.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;; last modified August 2, 2009

;==============================================================
; csound-mx tests (score matrices)
;==============================================================

(defmacro in-test-score-1 (id &rest body)
  `(with-temp-buffer
     (insert
      "i1 60     30  10000  110   11  .5  1  .001  1  .5  .5  1  .5   .5  1  1.5
i1 67     5   10000  110   12  .5  1  1     1  .5  .5  1  .    .   1  1.5
i 1 77     .   10000  110   12  .5  1  1     1  .5  .5  1  .    .   1  1.5
i1 83     .   5000   210   11  .5  1  .001  1  .5  .5  1  .    .   1  1.5
i1 83     .   5000   131   11  .5  1  .001  1  .5  .5  1  .    .   1  1.5
i 1 84     10  10000  123   12  .5  1  1     1  .5  .5  1  .9   .1  1  1.5
i1 94     .   10000  98    12  .5  1  1     1  .5  .5  1  .1   .9  1  1.5

i 2 83     14  10000  440   11  0   .1  .1  220   2000

i2 100    1   10000  900   11  1   .1  .1  600   200
i2 101    1   10000  1100  11  0   .1  .1  700   200
i3 102    2   15000  110   10  .5  .01  .001  .9  1  200
i3 104    3   15000  110   10  .5  .01  .001  .9  1  200
i1 104    20  5000   110   12  .5  1  1     1  .5  .5  1  .    .   1  1.5")
     (csound-sco-mode)
     (goto-char (point-min))
     (search-forward "100")
     (scomx-select-column "")
     (search-forward ". ")
     (scomx-select-column "c1")
     (search-forward "i")
     (scomx-select-column "i" t)
     ,@(append body
	       (when id `((scomx-grab-matrix ,id))))))

;TEST (in-test-score-1 "") => '(vec (vec 10000 10000 10000 5000 5000 10000 10000))

;TEST (in-test-score-1 "" (scomx-operate-m "" (1+ m))) => '(vec (vec 10001 10001 10001 5001 5001 10001 10001))

;TEST (in-test-score-1 "" (scomx-alg-mop "" "m + 2"))  => '(vec (vec 10002 10002 10002 5002 5002 10002 10002))

;;==== no need for :"..." format (?)
;; TO DO: change the manual accordingly

;TEST (in-test-score-1 "" (scomx-operate-m "" (+ 10.05 m))) => '(vec (vec (float 1001005 -2) (float 1001005 -2) (float 1001005 -2) (float 501005 -2) (float 501005 -2) (float 1001005 -2) (float 1001005 -2)))
 
;TEST (in-test-score-1 "" (scomx-operate-m "" (+ m :"10.02"))) => '(vec (vec (float 1001002 -2) (float 1001002 -2) (float 1001002 -2) (float 501002 -2) (float 501002 -2) (float 1001002 -2) (float 1001002 -2)))

;TEST (in-test-score-1 nil (scomx-elt 1 1 "i")) => "i1"

;TEST (in-test-score-1 nil (scomx-elt 1 8 "i")) => "i2"
;; TO DO: change the manual accordingly

;TEST (in-test-score-1 nil (scomx-grab-matrix-as-text "")) => "10000\n10000\n10000\n5000\n5000\n10000\n10000"

;TEST (in-test-score-1 nil (scomx-grab-matrix-as-text "c1")) => ".5\n.\n.\n.\n.\n.9\n.1"

;TEST (in-test-score-1 nil (scomx-grab-matrix-as-text "i")) => "1\n1\n1\n1\n1\n1\n2\n2\n2\n3\n3\n1"


;;==== is this what we want ?

;TEST (in-test-score-1 "i") => '(vec (vec 1 1 1 1 1 1 2 2 2 3 3 1))

;TEST (in-test-score-1 "c1") => '(vec (vec (float 5 -1) (error 0 "Expected a number") (error 0 "Expected a number") (error 0 "Expected a number") (error 0 "Expected a number") (float 9 -1) (float 1 -1)))

(defmath fip (x) (* x 3.1415))

;TEST (in-test-score-1 "i" (scomx-operate-m "i" (fip m))) => '(vec (vec (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 6283 -3) (float 6283 -3) (float 6283 -3) (float 94245 -4) (float 94245 -4) (float 31415 -4)))

(defun fip2 (x) (* x 3.1415))

;TEST (in-test-score-1 "i" (scomx-operate-p-nc-nr "i" (fip2 p))) => '(vec (vec (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 31415 -4) (float 6283 -3) (float 6283 -3) (float 6283 -3) (float 94245 -4) (float 94245 -4) (float 31415 -4)))



(provide 'csound-tests)

;; csound-tests.el ends here