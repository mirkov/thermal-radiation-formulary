;; Mirko Vukovic
;; Time-stamp: <2011-09-08 18:02:50EDT inner-cylinder-surfaces-view-factors.lisp>
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

;; view factors between inner surfaces of a cylinder:
;; - bases
;; - rings on bases (coaxial to cylinder axis)
;; - disks on bases (coaxial to cylinder axis)
;; - rings on sides (coaxial to cylinder axis)
;;
;;


;; All routines are initially lightly tested for a very special and
;; simple set of parameters.  The latter part of the file contains a
;; suit of more comprehensive tests.  These use symmetry and
;; configuration algebra to test the formulas against each other.

(defun f1-2-inside-cylinder-surface-to-itself (|r| |h|)
  "View factor of the inside surface of cylinder of radius `r' 
  and height `h' to itself

From the configuration factor library, C-78

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-78.html"
  (let ((H (/ |h| (* 2d0 |r|))))
    (- (+ 1d0 H)
       (sqrt (+ 1d0 (^2 H))))))

(define-test f1-2-inside-cylinder-surface-to-itself
  (let ((r 1d0)
	(h 2d0))
    (assert-number-equal
     (- 2d0 (sqrt 2d0))
     (f1-2-inside-cylinder-surface-to-itself r h))))

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

(defun f1-2-disk-in-cylinder-base-to-inside-surface
    (|r1| |r2| |h|)
  "View factor of a disk of radius `r1' in cylinder base of radius
  `r2' to the inside of the cylinder surface of height `h'

From the configuration factor library, C-80

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-80.html"
  (let ((R (/ |r2| |r1|))
	(H (/ |h| |r1|)))
    (assert (<= |r1| |r2|))
    (let ((H^2 (^2 H))
	  (R^2 (^2 R)))
      (* 0.5d0 (+ (- 1d0 R^2 H^2)
		  (sqrt (print (- (^2 (+ 1d0 R^2 H^2))
				  (* 4d0 R^2)))))))))

(define-test f1-2-disk-in-cylinder-base-to-inside-surface
  ;; I compare C-80 against C79
  (assert-number-equal
   (* pi (f1-2-base-of-right-cylinder-to-inside-surface 1d0 1d0))
   (* pi (f1-2-disk-in-cylinder-base-to-inside-surface 1d0 1d0 1d0))))

(defun f1-2-inner-surface-of-upper-cylinder-to-same-diameter-base-ring
    (|r| |h1| |h2|)
  "View factor of a the inner surface of a cylinder of radius `r'
coaxial with a disk of same radius.  The cylinder base is `h2' 
from the disk.  The disk is `h1' high.

From the configuration factor library, C-81

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-81.html"
  (let ((H1 (/ |h1| |r|))
	(H2 (/ |h2| |r|)))
    (* 0.25d0
       (- (* (+ 1d0 (/ H2 H1))
	     (sqrt (+ 4d0 (^2 (+ H1 H2)))))
	  (+ H1 (* 2d0 H2))
	  (* (/ H2 H1)
	     (sqrt (+ 4d0 (^2 H2))))))))

(define-test f1-2-inner-surface-of-upper-cylinder-to-same-diameter-base-ring
  ;; Test if for unity h1, h2, using the simplified result on the web page
  (assert-number-equal
   (/ (- (* 4d0 (sqrt 2d0)) 3d0 (sqrt 5d0))
      4d0)
   (f1-2-inner-surface-of-upper-cylinder-to-same-diameter-base-ring 1d0 1d0 1d0)))

(defun f1-2-interior-cylinder-surface-to-perpendicular-coaxial-disk
    (|r1| |r2| |h1| |h2|)
  "View factor of the inner surface of cylinder of radius `r1' with
  coaxial disk of radius `r2'.  Cylinder base is 'h1' separated from
  the disk.  Cylinder height is 'h2'-'h1'

From configuration factor library, C-82

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-82.html"
  (assert (<= |r1| |r2|))
  (assert (<= |h1| |h2|))
  (let ((R (/ |r1| |r2|))
	(H1 (/ |h1| |r2|))
	(H2 (/ |h2| |r2|)))
    (let* ((R^2 (^2 R))
	   (X1 (+^2 H1 R 1d0))
	   (X2 (+^2 H2 R 1d0)))
      (* (/ (* 4d0 R (- H2 H1)))
	 (+ (- X1 X2)
	    (- (sqrt (- (^2 X1) (* 4d0 R^2))))
	    (sqrt (- (^2 X2) (* 4d0 R^2))))))))

(define-test f1-2-interior-cylinder-surface-to-perpendicular-coaxial-disk
  (let ((r1 1d0)
	(r2 1d0)
	(h1 1d0)
	(h2 2d0))
    (assert-number-equal
     (* 0.25d0 (+ (- 3d0 6d0)
		  (- (sqrt (- 9d0 4d0)))
		  (sqrt (- 36d0 4d0))))
     (f1-2-interior-cylinder-surface-to-perpendicular-coaxial-disk
      r1 r2 h1 h2))))



(defun f1-2-annular-ring-to-interior-cylinder-surface
    (|r1| |r2| |h|)
  "View factor of an annular ring of inner radius `r1', outer radius
  `r2' to the interior cylinder surface of height `h'

From configuration factor library, C-83
http://www.engr.uky.edu/rtl/Catalog/sectionc/C-82.html"
  (assert (< |r1| |r2|))
  (let ((R (/ |r2| |r1|))
	(H (/ |h| |r1|)))
    (let ((R^2 (^2 R))
	  (H^2 (^2 H)))
      (* 0.5d0
	 (+ 1d0
	    (/ (- (* H (sqrt (+ (* 4d0 R^2) H^2)))
		  (sqrt (- (^2 (+^2 1d0 R H))
			   (* 4d0 R^2))))
	       (- R^2 1d0)))))))

(define-test f1-2-annular-ring-to-interior-cylinder-surface
  (let ((r1 1d0)
	(r2 2d0)
	(h 1d0))
    (assert-number-equal
     (* 0.5d0
	(+ 1d0 (* (/ 3d0)
		  (- (* 1d0 (sqrt 17d0))
		     (sqrt 20d0)))))
     (f1-2-annular-ring-to-interior-cylinder-surface r1 r2 h))))

(defun f1-2-interior-cylinder-surface-to-annular-ring-in-base
    (|r1| |r2| |a| |l|)
  "View factor from interior surface of cylinder of radius `a' and
  height `l' to coaxial annular cylinder in its base of inner radius
  `r1', outer radius `r2'

From configuration factor library, C-83

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-82.html"
  (assert (< |r1| |r2|))
  (let ((R1 (/ |r1| |a|))
	(R2 (/ |r2| |a|))
	(L (/ |l| |a|)))
    (let ((R1^2 (^2 R1))
	  (R2^2 (^2 R2))
	  (L^2 (^2 L)))
      (let ((X1 (sqrt (+ (^4 L)
			 (* 2d0 L^2 (+ 1d0 R1^2))
			 (^2 (- 1d0 R1^2)))))
	    (X2 (sqrt (+ (^4 L)
			 (* 2d0 L^2 (+ 1d0 R2^2))
			 (^2 (- 1d0 R2^2))))))
	(* (/ (* 4d0 L))
	   (- (+ X2 R2^2)
	      (+ X1 R1^2)))))))

(define-test f1-2-interior-cylinder-surface-to-annular-ring-in-base
  (let ((|r1| 1/4)
	(|r2| 1/2)
	(|a| 1)
	(|l| 1))
    (let ((*epsilon* 1e-7))
      (assert-number-equal
       (let ((R1 |r1|)
	     (R2 |r2|)
	     (L |l|))
	 (let ((X1 (sqrt (+ 1 (* 2 (+ 1 1/16)) (^2 (- 1 1/16)))))
	       (X2 (sqrt (+ 1 (* 2 (+ 1 1/4)) (^2 (- 1 1/4))))))
	   (* 1/4 (+ X2 (- X1) (^2 R2) (- (^2 R1))))))
       (f1-2-interior-cylinder-surface-to-annular-ring-in-base
	|r1| |r2| |a| |l|)))))

(defun f1-2-inner-surface-of-upper-cylinder-to-base-ring
    (|r1| |r2| |c1| |c2| |a|)
  "View factor of right cylinder of radius `a' to a coaxial ring of
  radii `r1' and `r2'.  The cylinder base is `c1' from the ring plane
  and top is `c2' from the ring plane.

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
      
(define-test f1-2-inner-surface-of-upper-cylinder-to-base-ring
  ;; Configuration 79 is a special case of configuration 85 with the
  ;; ring expanding into the base of the cylinder and the cylinder
  ;; touching the base
  (let ((r 1d0)
	(h 1d0))
    (let ((A1 (* pi (^2 r)))
	  (A2 (* 2d0 pi r h)))
      (assert-number-equal
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


(defun f1-2-cylinder-inside-surface-to-adjecant-inside-surface
    (|r| |h1| |h2|)
  "View factor from a in inside surface of height `h1' of cylinder of
  radius `r' to adjecant inside surface of height `h2'

From configuration factor library, C-86

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-86.html"
  (let ((H1 (/ |h1| |r|))
	(H2 (/ |h2| |r|)))
    (let ((H1^2 (^2 H1))
	  (H2^2 (^2 H2))
	  (H2/H1 (/ H2 H1)))
      (+ (/ H2 2d0)
       (* 0.25d0
	  (+ (sqrt (+ 4d0 H1^2))
	     (* H2/H1 (sqrt (+ 4d0 H2^2)))
	     (- (*(+ 1d0 H2/H1)
		  (sqrt (+ 4d0 (^2 (+ H1 H2))))))))))))

(define-test f1-2-cylinder-inside-surface-to-adjecant-inside-surface
  (let ((r 1)
	(h1 1)
	(h2 1))
    (let ((*epsilon* 1e-6))
      (assert-number-equal
       (+ 1/2 (* 1/4 (+ (sqrt 5)
			(* 1 (sqrt 5))
			(- (* 2 (sqrt 8))))))
       (f1-2-cylinder-inside-surface-to-adjecant-inside-surface
	r h1 h2)))))

(defun f1-2-cylinder-inside-surface-to-other-inside-surface
    (|l1| |l2| |l3| |a|)
  "View factor from a in inside surface of height `l3'-`l2' of
  cylinder of radius `a' to other inside surface of height `l1'.  The
  surfaces are a distance `l2' - `l1' from each other

From configuration factor library, C-87

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-87.html"
  (assert (< |l1| |l2| |l3|))
  (let ((L1 (/ |l1| |a|))
	(L2 (/ |l2| |a|))
	(L3 (/ |l3| |a|)))
    (labels ((X (L)
	       (* L (sqrt (+ 4d0 (^2 L))))))
      (* (/ (* 4d0 (- L3 L2)))
	 (+ (* 2d0 L1 (- L3 L2))
	    (X (- L3 L1))
	    (- (X (- L2 L1)))
	    (- (X L3))
	    (X L2))))))

(define-test f1-2-cylinder-inside-surface-to-other-inside-surface
  (let ((l1 1)
	(l2 2)
	(l3 3)
	(a 1)
	(*epsilon* 1e-5))
    (assert-number-equal
     (let ((X1 (sqrt 5))
	   (X2 (sqrt 8))
	   (X3 (sqrt 13)))
       (* (/ 4)
	  (+ 2
	     (* 2 (sqrt 8))
	     (- (* 1 (sqrt 5)))
	     (- (* 3 X3))
	     (* 2 X2))))
     (f1-2-cylinder-inside-surface-to-other-inside-surface l1 l2 l3 a))))