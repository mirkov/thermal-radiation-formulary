;; Mirko Vukovic
;; Time-stamp: <2011-09-21 14:47:52EDT disk+ring-view-factors.lisp>
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

(in-package :thermal-radiation-formulary)

(defun f1-2-parallel-coaxial-equal-disks (|r| |a|)
  "Parallel coaxial disks of radius `|r|' and separation `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-40.html"
  (let* ((r^2 (^2 (/ |r| |a|)))
	 (x (/ (+ (* 2d0 r^2)
		  1d0)
	       r^2))
	 (A1 (* pi r^2))
	 (A2 A1))
    (values (* 0.5d0
	       (- x (sqrt (- (^2 x) 4d0))))
	    A1 A2)))


(defun f1-2-parallel-coaxial-unequal-disks (|r1| |a| |r2|)
  "Parallel coaxial disks of radius `r-disk' and separation `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-41.html"
  (labels ((r-norm (r h)
	     (/ r h)))
    (let* ((R1^2 (^2 (r-norm |r1| |a|)))
	   (R2^2 (^2 (r-norm |r2| |a|)))
	   (x (+ 1d0 (/ (+ 1d0 R2^2)
			R1^2)))
	 
	   (A1 (* pi (^2 |r1|)))
	   (A2 (* pi (^2 |r2|))))
      (values (* 0.5d0
		 (- x (sqrt (- (^2 x)
			       (* 4d0 (/ R2^2 R1^2))))))
	      A1 A2))))

(define-test coaxial-disks
  ;; test the unequal disks by comparing with equal disks
  (assert-f12-equal
     (f1-2-parallel-coaxial-equal-disks 1d0 1.5d0)
     (f1-2-parallel-coaxial-unequal-disks 1d0 1.5d0 1d0)))

(defun f1-2-parallel-coaxial-disk->ring (|r1| |a| |r2| |r3|)
  "View factor from disk of radius `|r1|' to parallel coaxial ring of
radii `|r2|' and `|r3|'.  The separation between the two is `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-47.html"
  (assert (<= |r2| |r3|))
  (labels ((f (r^2 h^2)
	     (- r^2
		(sqrt (- (^2 (+ 1d0 r^2 h^2))
			 (* 4d0 r^2))))))
    (let ((r3^2 (^2 (/ |r3| |r1|)))
	  (r2^2 (^2 (/ |r2| |r1|)))
	  (h^2 (^2 (/ |a| |r1|)))
	  (A1 (* pi (-^2 |r2| |r3|)))
	  (A2 (* pi (^2 |r1|))))
   (values (* 0.5d0
	      (- (f r3^2 h^2)
		 (f r2^2 h^2)))
	   A1 A2))))

(define-test f1-2-parallel-coaxial-disk->ring
  ;; test by making the ring inner radius 0d0, and comparing with disk
  ;; of same outer radius
  (assert-f12-equal
   (f1-2-parallel-coaxial-equal-disks 1.5d0 2d0)
   (f1-2-parallel-coaxial-disk&ring 1.5d0 2d0 0d0 1.5d0))
  ;; test by using configuration alebra: disk-to-disk vs
  ;; disk-to-smaller-disk + disk-to-remaining-ring
  (assert-f12-equal
   (f1-2-parallel-coaxial-unequal-disks 1.5d0 3d0 2d0)
   (+ (f1-2-parallel-coaxial-unequal-disks 1.5d0 3d0 1.75d0)
      (f1-2-parallel-coaxial-disk->ring 1.5d0 3d0 1.75d0 2d0))))


(defun f1-2-parallel-coaxial-rings (|r1| |r2| |a|
					      |r3| |r4|)
  "View factor from ring of radii `r-inner' and `r-outer' to parallel
  coaxial disk of radius `r-disk'.  The separation between the two is
  `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-52.html"
  (assert (<= |r1| |r2|))
  (assert (<= |r3| |r4|))
  (labels ((f (ra^2 rb^2 h^2)
	     (sqrt (- (^2 (+ ra^2 rb^2 h^2))
		      (* 4d0 ra^2 rb^2)))))
    (let ((r2^2 (^2 (/ |r2| |r1|)))
	  (r3^2 (^2 (/ |r3| |r1|)))
	  (r4^2 (^2 (/ |r4| |r1|)))
	  (h^2 (^2 (/ |a| |r1|)))
	  (A1 (* pi (-^2 |r2| |r1|)))
	  (A2 (* pi (-^2 |r4| |r3|))))
      (values
       (* (/ (* 2d0 (- r2^2 1d0)))
	  (- (- (f r2^2 r3^2 h^2)
		(f 1d0 r3^2 h^2))
	     (- (f r2^2 r4^2 h^2)
		(f 1d0 r4^2 h^2))))
       A1 A2))))

(define-test f1-2-parallel-coaxial-rings
  ;; I test against disk-disk and disk-ring form factors
  (let ((lisp-unit:*epsilon* 1e-9))
    (assert-f12-equal
     (f1-2-parallel-coaxial-equal-disks 1d0 1.5d0)
     (f1-2-parallel-coaxial-rings 1d-9 1d0 1.5d0 0d0 1.d0))
    (assert-f12-equal
     (f1-2-parallel-coaxial-unequal-disks 1.0 2d0 1.5d0)
     (f1-2-parallel-coaxial-rings 1d-9 1.0 2d0 0d0 1.5d0))
    (assert-f12-equal
     (f1-2-parallel-coaxial-disk->ring 1d0 2d0 3d0 4d0)
     (f1-2-parallel-coaxial-rings 1d-9 1d0 2d0 3d0 4d0))))

(define-test f1-2-parallel-coaxial-rings-S&H-5.8
  ;; Example 5-8 of Siegel & Howell for some arbitrary numbers.
  ;; Despite all the numbers double precision, the accuracy is about
  ;; 1e-14
  (let ((r1 1d0)
	(r2 1.28d0)
	(r4 0.72d0)
	(r3 0.92d0)
	(a 0.38d0))
    (let ((a1 (* pi (^2 r1)))
	  (a2 (* pi (-^2 r2 r1)))
	  (lisp-unit:*epsilon* 1d-12))
      (assert-f12-equal
       (- (* (/ (+ a1 a2) a2)
	     (- (f1-2-parallel-coaxial-unequal-disks r2 a r3)
		(f1-2-parallel-coaxial-unequal-disks r2 a r4)))
	  (* (/ a1 a2)
	     (- (f1-2-parallel-coaxial-unequal-disks r1 a r3)
		(f1-2-parallel-coaxial-unequal-disks r1 a r4))))
       (f1-2-parallel-coaxial-rings r1 r2 a r4 r3)))))