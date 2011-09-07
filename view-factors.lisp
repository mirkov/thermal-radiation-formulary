;; Mirko Vukovic
;; Time-stamp: <2011-09-07 14:18:32EDT view-factors.lisp>
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

(defun f1-2-base-of-right-cylinder-to-inside-surface
    (|r| |h|)
  "View factor of the base of cylinder of radius `r' to its inside
  surface of height `h'

From the configuration factor library, C-79

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-79.html"
  (let ((H (/ |h| (* 2d0 |r|))))
    (* 2d0 H (- (sqrt (+ 1d0 (^2 H))) H))))

(lisp-unit:define-test f1-2-base-of-right-cylinder-to-inside-surface
  (lisp-unit:assert-number-equal (* 2d0 1d0 (- (sqrt 2d0) 1d0))
		       (f1-2-base-of-right-cylinder-to-inside-surface 1d0 2d0)))

(defun f1-2-inner-surface-of-upper-cylinder-to-base-ring
    (|r1| |r2| |c1| |c2| |a|)
  "View factor of right cylinder of radius `a' to a coaxial ring of
  radii `r1' and `r2'.  The cylinder base is `c1' from the ring plane and top is `c2' from the ring plane.

From configuration factor library, C-85

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-85.html"
  (assert (< |r1| |r2|))
  (assert (<= |r2| |a|))
  (assert (< |c1| |c2|))
  (let ((C1 (/ |c1| |a|))	
	(C2 (/ |c2| |a|))
	(R1 (/ |r1| |a|))	
	(R2 (/ |r2| |a|)))
    (let ((C1^2 (^2 C1))
	  (C2^2 (^2 C2))
	  (C1^4 (^4 C1))
	  (C2^4 (^4 C2))
	  (1+R1^2 (+ 1d0 (^2 R1)))
	  (1-R1^2 (- 1d0 (^2 R1)))
	  (1+R2^2 (+ 1d0 (^2 R2)))
	  (1-R2^2 (- 1d0 (^2 R2))))
      (* (/ -1d0
	    (* 4d0 (- C2 C1)))
	 (+ (- (sqrt (+ C1^4 (* 2 C1^2 1+R2^2) (^2 1-R2^2)))
	       (sqrt (+ C1^4 (* 2 C1^2 1+R1^2) (^2 1-R1^2))))
	    (- (sqrt (+ C2^4 (* 2 C2^2 1+R1^2) (^2 1-R1^2)))
	       (sqrt (+ C2^4 (* 2 C2^2 1+R2^2) (^2 1-R2^2)))))))))
      
(lisp-unit:define-test f1-2-inner-surface-of-upper-cylinder-to-base-ring
  ;; Configuration 79 is a special case of configuration 85 with the
  ;; ring expanding into the base of the cylinder and the cylinder
  ;; touching the base
  (let ((r 1d0)
	(h 1d0))
    (let ((A1 (* pi (^2 r)))
	  (A2 (* 2d0 pi r h)))
      (lisp-unit:assert-number-equal
       (* A1 (f1-2-base-of-right-cylinder-to-inside-surface 1d0 1d0))
       (* A2 (f1-2-inner-surface-of-upper-cylinder-to-base-ring 0d0 1d0 0d0 1d0 1d0))
       "C-85 tested against C79")))
  ;; Split cylinder into two of equal areas.  Test that f12 of the
  ;; whole cylinder is twice f12 o th e individual areas
  (let ((a 1d0)
	(r2 1d0)
	(r1 0d0)
	(c1 1d0)
	(c2 2d0))
    (lisp-unit:assert-number-equal
     (* 2 (f1-2-inner-surface-of-upper-cylinder-to-base-ring r1 r2 0d0 c2 a))
     (+ (f1-2-inner-surface-of-upper-cylinder-to-base-ring r1 r2 0d0 c1 a)
	(f1-2-inner-surface-of-upper-cylinder-to-base-ring r1 r2 c1 c2 a))
     "C-85 split cylinder test"))
  ;; Split base into two of equal areas.  The sum of viewfactors from
  ;; side to the two areas should equal the the viewfactor of the side
  ;; to the total area
  (let ((a 1d0)
	(r2 1d0)
	(r1 (/ (sqrt 2d0)))
	(c1 0d0)
	(c2 1d0))
    (assert-number-equal
     (f1-2-inner-surface-of-upper-cylinder-to-base-ring 0d0 r2 c1 c2 a)
     (+ (f1-2-inner-surface-of-upper-cylinder-to-base-ring 0d0 r1 c1 c2 a)
	(f1-2-inner-surface-of-upper-cylinder-to-base-ring r1 r2 c1 c2 a))
     "C-85 split bottom ring test")))


;;; view factors between rectangular elements
(export '(F1-2-identical-parallel-directly-opposed-rectangles
	  F1-2-Coaxial-parallel-squares-of-different-edge-length
	  f1-2-perpendicular-rectangles-with-common-edge))

;; helper functions and macros
(defun atan* (number1 &optional (number2 1))
  "Parallel plate view factors often use functions of the form
 x*y*atan(x/y) or x*atan(x).

`atan*' implements this functionality, in manner similar to cl's atan"
  (* number1 number2
     (atan number1 number2)))

(defmacro +^2 (&rest args)
  "Expands into sum of arguments squared"
  `(+ ,@(mapcar #'(lambda (arg)
		    `(expt ,arg 2))
		args)))

(defun exp-rat (a b)
  "Expands into

            a 
  ( 	   ) 
  ( a(1+b) )
  ( ------ )
  ( (1+a)b )
  (	   )
"
  (expt (/ (* a (+ 1d0 b))
	   (* (+ 1d0 a) b))
	a))

(define-test exp-rat
  (assert-number-equal 1d0 (exp-rat 1d0 1d0))
  (assert-number-equal (/ 16 9) (exp-rat 2d0 1d0))
  (assert-number-equal (/ 3 4 ) (exp-rat 1d0 2d0)))

;; view factor formulas

(defun F1-2-identical-parallel-directly-opposed-rectangles
    (|a| |b| |c|)
  "View factors between two identical parallel rectangels of lengths a
& b, at distance c from each other

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-11.html"
  (let* ((X (/ |a| |c|))
	 (Y (/ |b| |c|)))
    (let* ((X^2 (^2 X))
	   (Y^2 (^2 Y))
	   (1+X^2 (+ 1d0 X^2))
	   (1+Y^2 (+ 1d0 Y^2)))
      (* (/ 2d0 (* pi X Y))
	 (+ (log (sqrt (/ (* 1+X^2 1+Y^2)
			  (+ 1d0 X^2 Y^2))))
	    (atan* X (sqrt 1+Y^2))
	    (atan* Y (sqrt 1+X^2))
	    (- (atan* X))
	    (- (atan* Y)))))))

(define-test f1-2-identical-parallel-directly-opposed-rectangles
  (let ((*epsilon* 1e-6))
    (assert-number-equal
     (* (/ 2 pi)
	(+ (log (sqrt (/ 4 3)))
	   (* 2 (* (sqrt 2) (atan (/ (sqrt 2)))))
	   (- (* 2 (atan 1)))))
     (f1-2-identical-parallel-directly-opposed-rectangles 1.0d0 1.0d0 1.0d0))))

(defun F1-2-Coaxial-parallel-squares-of-different-edge-length
    (|a| |b| |c|)
  "View factors between two parallel squares of length a & b, at
  distance c from each other

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-12.html
"
  (let* ((A (/ |a| |c|))
	 (B (/ |b| |a|))
	 (X (* A (+ 1 B)))
	 (Y (* A (- 1 B))))
    (if (< A 0.2)
	(/ (^2 (* A B))
	   pi)
	(* (/ (* pi (^2 A)))
	   (+ (log (/ (^2 (+ (* (^2 A) (+ 1 (^2 B))) 2))
		      (* (+ (^2 Y) 2)
			 (+ (^2 X) 2))))
	      (atan* Y (sqrt (+ (^2 Y) 4)))
	      (- (atan* X (sqrt (+ (^2 Y) 4))))
	      (atan* X (sqrt (+ (^2 X) 4)))
	      (- (atan* Y (sqrt (+ (^2 X) 4)))))))))

(define-test F1-2-Coaxial-parallel-squares-of-different-edge-length
  (let ((*epsilon* 1e-6))
    (assert-number-equal
     (* (/ pi)
	(+ (log (/ 16 12))
	   (- (* 4 (atan 1)))
	   (* (sqrt 8) 2 (atan (/ 2 (sqrt 8))))))
     (f1-2-coaxial-parallel-squares-of-different-edge-length 1d0 1d0 1d0))))





(defun F1-2-perpendicular-rectangles-with-common-edge (|l| |w| |h|)
  "View factor between two finite rectangles (dimensions l x w and l x
h) of same common edge of length `l' , at an angle of 90o to each
other.

Rectangle 1 (area 1) is defined by l and w
Rectangle 2 (area 2) is defined by l and h

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-14.html"
  (let*((H (/ |h| |l|))
	(W (/ |w| |l|))
	(W^2 (^2 W))
	(H^2 (^2 H))
	(H^2+W^2 (+ H^2 W^2)))
    (* (/ (* pi W))
       (+ (atan* 1d0 W) (atan* 1d0 H) (- (atan* 1d0 (sqrt H^2+W^2)))
	  (* 0.25d0 (log (* (/ (* (+ 1d0 W^2) (+ 1d0 H^2))
			       (+ 1d0 H^2+W^2))
			    (exp-rat W^2 H^2+W^2)
			    (exp-rat H^2 H^2+W^2))))))))

(define-test f1-2-perpendicular-rectangles-with-common-edge
  (assert-number-equal
   (* (/ pi)
      (+ (/ pi 2)
	 (- (* (sqrt 2d0) (atan (/ (sqrt 2d0)))))
	 (* 0.25 (log (* (/ 4 3)
			 (exp-rat 1d0 2d0)
			 (exp-rat 1d0 2d0))))))
   (f1-2-perpendicular-rectangles-with-common-edge 1d0 1d0 1d0)))
	     

(define-test rectangular-enclosure
  ;; consider a rectangular enclosure of dimensions 0.1x0.08x0.18 This
  ;; test checks the sum of view factors from the 0.1x0.08 face to the
  ;; other five faces equals 1.0
  ;;
  ;; To get full double digit accuracy, the arguments must be double
  ;; precision.
  (assert-number-equal
   1.0
   (+
    (* 2 (f1-2-perpendicular-rectangles-with-common-edge .08d0 0.1d0 0.18d0))
    (* 2 (f1-2-perpendicular-rectangles-with-common-edge .1d0 .08d0 .18d0))
    (f1-2-identical-parallel-directly-opposed-rectangles .08d0 .1d0 .18d0)))
  (let ((*epsilon* 1e-7))
    (assert-number-equal
     1.0
     (+
      (* 2 (f1-2-perpendicular-rectangles-with-common-edge .08 0.1 0.18))
      (* 2 (f1-2-perpendicular-rectangles-with-common-edge .1 .08 .18))
      (f1-2-identical-parallel-directly-opposed-rectangles .08 .1 .18)))))