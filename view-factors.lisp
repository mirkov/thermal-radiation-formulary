;; Mirko Vukovic
;; Time-stamp: <2011-09-21 10:50:04EDT view-factors.lisp>
;;
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :radiation)

;;;; Radiation view factors
;;; Naming:
;;; - area to area are named f1-2-descriptive-text
;;; - differential to area are named fd1-2-descriptive-text
;;;
;;; All internal calculations are done at double precision
(export '(fd1-2-to-parallel-disk f1-2-parallel-coaxial-equal-disks
	  f1-2-parallel-coaxial-unequal-disks f1-2-parallel-coaxial-disk&ring
	  f1-2-parallel-coaxial-rings))



(defun fd1-2-to-parallel-disk (r h a)
  "Differential element at radius r offset by height h from a
parallel disk of radius a
Adapted from Siegel and Howell, Appendix C, case 19 by a change of notation.
See ~/The-works/generic-works/2008-08-03--reformulation-of-siegel-appendix-c-case-19.tex

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-11.html"
  (let ((rr (/ r a))
	(hh (/ h a)))
    (* 0.5d0
       (- 1d0 (/ (1- (+ (^2 hh) (^2 rr)))
		 (sqrt (- (^2 (+ 1 (^2 hh) (^2 rr)))
			  (* 4d0 (^2 rr)))))))))
(lisp-unit:define-test dA-to-parallel-disk
  ;; Tests based on /projects/low-k-curing/workbooks/2008-08-03--wafer-heat-loss.tex
  (let ((*epsilon* 1e-3))
    (assert-number-equal 1.6636e-3 (fd1-2-to-parallel-disk 2.2 0.2 0.85))
    (assert-number-equal 0.94753 (fd1-2-to-parallel-disk 0.01 0.2 0.85))))



(defun f1-2-parallel-coaxial-equal-disks (r-disk sep)
  "Parallel coaxial disks of radius `r-disk' and separation `sep'


http://www.engr.uky.edu/rtl/Catalog/sectionc/C-40.html"
  (let* ((r^2 (^2 (/ r-disk sep)))
	(x (/ (+ (* 2d0 r^2)
		 1d0)
	      r^2)))
    (* 0.5d0
       (- x (sqrt (- (^2 x) 4d0))))))


(defun f1-2-parallel-coaxial-unequal-disks (r-disk1 r-disk2 sep)
  "Parallel coaxial disks of radius `r-disk' and separation `sep'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-41.html"
  (labels ((r-norm (r h)
	     (/ r h)))
  (let* ((r1^2 (^2 (r-norm r-disk1 sep)))
	 (r2^2 (^2 (r-norm r-disk2 sep)))
	 (x (+ 1d0 (/ (+ 1d0 r2^2)
		      r1^2))))
    (* 0.5d0
       (- x (sqrt (- (^2 x)
		     (* 4d0 (/ r2^2 r1^2)))))))))

(defun f1-2-parallel-coaxial-disk&ring (r-disk sep r-inner r-outer)
  "View factor from ring of radii `r-inner' and `r-outer' to parallel
  coaxial disk of radius `r-disk'.  The separation between the two is
  `sep'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-47.html"
  (labels ((f (r^2 h^2)
	     (- r^2
		(sqrt (- (^2 (+ 1d0 r^2 h^2))
			 (* 4d0 r^2))))))
    (let ((r3^2 (^2 (/ r-outer r-disk)))
	  (r2^2 (^2 (/ r-inner r-disk)))
	  (h^2 (^2 (/ sep r-disk))))
      (* 0.5d0
	 (- (f r3^2 h^2)
	    (f r2^2 h^2))))))

(defun f1-2-parallel-coaxial-rings (r1-inner r1-outer sep
					      r2-inner r2-outer)
  "View factor from ring of radii `r-inner' and `r-outer' to parallel
  coaxial disk of radius `r-disk'.  The separation between the two is
  `sep'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-52.html"
  (labels ((f (ra^2 rb^2 h^2)
	     (sqrt (- (^2 (+ ra^2 rb^2 h^2))
		      (* 4d0 ra^2 rb^2)))))
    (let ((r2^2 (^2 (/ r1-outer r1-inner)))
	  (r3^2 (^2 (/ r2-inner r1-inner)))
	  (r4^2 (^2 (/ r2-outer r1-inner)))
	  (h^2 (^2 (/ sep r1-inner))))
      (* (/ (* 2d0 (- r2^2 1d0)))
	 (- (- (f r2^2 r3^2 h^2)
	       (f 1d0 r3^2 h^2))
	    (- (f r2^2 r4^2 h^2)
	       (f 1d0 r4^2 h^2)))))))





