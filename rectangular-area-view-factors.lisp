;; Mirko Vukovic
;; Time-stamp: <2011-09-21 11:03:29EDT rectangular-area-view-factors.lisp>
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
	 (Y (/ |b| |c|))
	 (A1 (* |a| |b|))
	 (A2 A1)
	 (F12 (let* ((X^2 (^2 X))
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
    (values F12 A1 A2)))

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
	 (Y (* A (- 1 B)))
	 (A1 (* |a| |b|))
	 (A2 A1)
	 (F12 (if (< A 0.2)
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
    (values F12 A1 A2)))

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
	(H^2+W^2 (+ H^2 W^2))
	(A1 (* |l| |w|))
	(A2 (* |l| |h|))
	(F12 (* (/ (* pi W))
		(+ (atan* 1d0 W)
		   (atan* 1d0 H)
		   (- (atan* 1d0 (sqrt H^2+W^2)))
		   (* 0.25d0 (log (* (/ (* (+ 1d0 W^2) (+ 1d0 H^2))
					(+ 1d0 H^2+W^2))
				     (exp-rat W^2 H^2+W^2)
				     (exp-rat H^2 H^2+W^2))))))))
    (values F12 A1 A2)))

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