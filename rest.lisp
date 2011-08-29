;; Mirko Vukovic
;; Time-stamp: <2011-08-25 15:23:29EDT rest.lisp>
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


;;;;  Black-body formulae

(export '(beta))	      

(defun beta (eps1 eps2)
  "Calculate the parallel combination of emissivities"
  (let ((eps12 (* eps1 eps2)))
  (/ eps12
     (+ eps1 eps2 eps12))))


(defun parallel-plate-equilibrium (T0 T1 &optional (beta0 1d0) (beta1 1d0))
  "What does this calculate?"
  (let ((beta1/0 (/ beta1 beta0)))
     (* T0
	(^0.25
	 (+ (/ 1 (1+ beta1/0))
	    (/ (^4 (/ T1 T0))
	       (1+ (/ 1 beta1/0))))))))


;;;; simple radiation problems

;;; three infinite plate problems
;; checked against SWP calculation

(defmacro 3parallel-plate-solver ((mat-def rhs-def) &body post-process)
  "Set-up environment for solving three parallel plate radiation
problem.  The first two arguments, `mat-def' and `rhs-def' accept gsll
matrix/vector.  The post-process lists are forms for the case
environment."
  `(let* ((dim 5)
	  lu per x
	  (mat ,mat-def)
	  (rhs ,rhs-def)
	  (rhs-wc ,rhs-def))
     (lambda (command)
       (case command
	 (:solve (multiple-value-bind (lu1 per1)
		     (lu-decomposition mat)
		   (setf lu lu1
			 per per1)
		   (setf x (lu-solve mat rhs-wc per))))
	 (:check (matrix-product-triangular
		  mat (matrix-product-triangular mat (copy x) 1
						 :Upper :NoTrans :NonUnit)
		  1 :Lower :NoTrans :Unit))
	 (:mat mat)
	 (:per per)
	 (:lu lu)
	 (:rhs rhs)
	 (:fluxes x)
	 ,@post-process))))
(defun 3parallel-plate-equilibrium-temp (eps0 T0 eps1-0 eps1-2 eps2 T2
				   &key (q1 0d0))
  "Calculate temperature of middle of three parallel plates with diffuse emission
Parameters:
eps0, T0: emissivity and temperature of plate 0
eps1-0, eps1-2: surface emissivities of plate 1 facing plate 1 and 2
eps2, T2: emissivity and temperature of plate 2

Get results as follows:
temp: (send *prob* :temp)
fluxes: (send *prob* :fluxes)

Need macro send from my-utils.  Otherwise use (funcall closure args)"
  (3parallel-plate-solver
   ((make-marray 'double-float :dimensions (list dim dim)
			       :initial-contents
			       (list (list 1d0 (1- eps0) 0d0 0d0 0d0)
				     (list (1- eps1-0) 1d0 0d0 0d0 (0- eps1-0))
				     (list 0d0 0d0 1d0 (1- eps1-2) (0- eps1-2))
				     (list 0d0 0d0 (1- eps2) 1d0 0d0)
				     (list (0- eps1-0) 0d0 0d0 (0- eps1-2) (+ eps1-0 eps1-2))))
    (make-marray 'double-float :dimensions dim
			       :initial-contents (list (st4 T0 eps0) 0d0 0d0
						       (st4 T2 eps2) q1)))
   (:temp (expt (/ (maref x 4)
		   +sigma+)
		0.25))))

(defun 3parallel-plate-equilibrium-flux (eps0 T0 eps1-0 eps1-2 T1 eps2 T2)
  "Solve flux to middle of three parallel plates with diffuse emission
Parameters:
eps0, T0: emissivity and temperature of plate 0
eps1-0, eps1-2, T1: surface emissivities of plate 1 facing plate 1 and 2 and temperature
eps2, T2: emissivity and temperature of plate 2

Get results as follows:
q1: (send *prob* :q1):  desired flux
fluxes: (send *prob* :fluxes): all fluxes

Need macro send from my-utils.  Otherwise use (funcall closure args)"
  (3parallel-plate-solver
   ((make-marray 'double-float :dimensions (list dim dim)
			   :initial-contents
			  (list (list 1d0 (1- eps0) 0d0 0d0 0d0)
				(list (1- eps1-0) 1d0 0d0 0d0 (0- eps1-0))
				(list 0d0 0d0 1d0 (1- eps1-2) (0- eps1-2))
				(list 0d0 0d0 (1- eps2) 1d0 0d0)
				(list eps1-0 0d0 0d0 eps1-2 1d0)))
    (make-marray 'double-float :dimensions dim
			       :initial-contents (list (st4 T0 eps0) 0d0 0d0
						       (st4 T2 eps2)
						       (st4 T1 (+ eps1-0 eps1-2)))))
   (:q1 (maref x 4))))

	     


