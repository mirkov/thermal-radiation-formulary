;; Mirko Vukovic
;; Time-stamp: <2011-08-25 17:26:43EDT radiation.lisp>
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


;;;;  Black-body formulae

(export '(spectral-dist peak-wavelength total-power frac-emiss-pwr))


(defconstant +C1+ 5.9552137e7
  "Constant in Planck's spectral energy distribution law [W um^4/(m^2 sr)]")
(defconstant +C2+ 14387.752
  "Constant in Planck's spectral energy distribution law [um K]")
(defconstant +C3+ 2897.7686 "Constant in Wien's displacement law [um K]")


(defun spectral-dist (lambda temp)
  "Spectral distribution of emissive power

Siegel&Howell (1-11)"
  (/ (* 2 +C1+)
     (* (expt lambda 5)
	(- (exp (/ +C2+
		   (* lambda temp)))
	   1))))

(defun peak-wavelength (temp)
  "Wavelength at which blackbody intensity is a maximum at temperature temp

Siegel&Howell (1-22)"
  (/ +C3+ temp))

(defun total-power (temp)
  "Hemispherical total emissive power for a blackbody at temperature temp

Siegel&Howell (1-28)"
  (* +sigma+ (expt temp 4)))

(defun frac-emiss-pwr (lambdaT)
  "Fractional blackbody emissive power in the range 0 to lambda.T

Siegel&Howell (1-33) for lambda.T<1e4, my expansion otherwise"
  (let ((x (/ +C2+ lambdaT)))
    ;; I have approximately determined that for labmda.T>1e4 the three
    ;; term expansion works OK
    (if (> lambdaT 1e4) 
	(- 1 (* (/ 15 (expt pi 4))
		(+ (/ (^3 x)
		      3)
		   (0- (/ (expt x 4)
			  8))
		   (/ (expt x 5)
		      60))))
	;; I use a 5-term series for lambda.T<1e4
	(let ((val))
	  (* (/ 15 (expt pi 4))
	     (do ((n 1 (1+ n))
		  (acc 0 (+ acc val)))
		 ((= 5 n) acc)
	       (setf val
		     (* (/ (exp (0- (* n x)))
			   n)
			(+ (^3 x)
			   (/ (* 3 x x)
			      n)
			   (/ (* 6 x)
			      (^2 n))
			   (/ 6 (^3 n)))))))))))
	      

;; Example 1-1
(define-test spectral-dist-1
  (assert-equality #'epsilon-equals (* 1 2746)
		   (spectral-dist 6 1273) "elb"))
;; Example 1-2
(define-test spectral-dist-2
  (assert-equality #'epsilon-equals 2.5898754e7
		   (spectral-dist 0.55 5780) "elb"))

;; Example 1-3
(define-test peak-wavelength
  (assert-equality #'epsilon-equals 9.85
		   (peak-wavelength (+ 273 21))))

;; Example 1-8
(define-test frac-emiss-pwr-low-lt-range
  (assert-equality #'epsilon-equals 0.14026 (frac-emiss-pwr 2400))
  (assert-equality #'epsilon-equals 8.703e-5 (frac-emiss-pwr 900)))


(define-test frac-emiss-pwr-high-lt-range
  (assert-equality #'epsilon-equals 0.96893 (frac-emiss-pwr 15000))
  (assert-equality #'epsilon-equals 0.97581 (frac-emiss-pwr 16500)))
