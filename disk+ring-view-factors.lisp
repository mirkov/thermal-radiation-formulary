;; Mirko Vukovic
;; Time-stamp: <2011-09-21 11:23:23EDT disk+ring-view-factors.lisp>
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


(defun f1-2-parallel-coaxial-unequal-disks (|r1| |r2| sep)
  "Parallel coaxial disks of radius `r-disk' and separation `sep'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-41.html"
  (labels ((r-norm (r h)
	     (/ r h)))
    (let* ((R1^2 (^2 (r-norm |r1| sep)))
	   (R2^2 (^2 (r-norm |r2| sep)))
	   (x (+ 1d0 (/ (+ 1d0 R2^2)
			R1^2)))
	 
	   (A1 (* pi (^2 |r1|)))
	   (A2 (* pi (^2 |r2|))))
      (values (* 0.5d0
		 (- x (sqrt (- (^2 x)
			       (* 4d0 (/ R2^2 R1^2))))))
	      A1 A2))))

(defun f1-2-parallel-coaxial-disk&ring (|r1| |a| |r2| |r3|)
  "View factor from ring of radii `|r2|' and `|r3|' to parallel
  coaxial disk of radius `|r1|'.  The separation between the two is
  `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-47.html"
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

(defun f1-2-parallel-coaxial-rings (|r1| |r2| |a|
					      |r3| |r4|)
  "View factor from ring of radii `r-inner' and `r-outer' to parallel
  coaxial disk of radius `r-disk'.  The separation between the two is
  `|a|'

http://www.engr.uky.edu/rtl/Catalog/sectionc/C-52.html"
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