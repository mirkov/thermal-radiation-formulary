;; Mirko Vukovic
;; Time-stamp: <2011-08-26 10:11:32EDT utilities.lisp>
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

(export '(+kb+ +sigma+ ^4 ^0.25 -^4 C->K K->C st4))
(define-symbol-macro +kb+ physics-constants:+boltzmann-constant-SP+)
(define-symbol-macro +sigma+ physics-constants:+stefan-boltzmann-constant-SP+)

(defmacro ^4 (arg)
  `(expt ,arg 4))

(defmacro ^0.25 (arg)
  `(expt ,arg 0.25))

(defun -^4 (arg1 arg2)
  (- (expt arg1 4)
     (expt arg2 4)))

(defmacro C->K (arg)
  `(+ ,arg 273.15))

(defmacro K->C (arg)
  `(- ,arg 273.15))


(defmacro st4 (Temp &optional rel-sigma)
  "Shorthand for (* sigma rel-sigma T^4).
If rel-sigma is not provided, it is assumed to be unity"
  (if rel-sigma
      `(* +sigma+ (expt ,Temp 4) ,rel-sigma)
      `(* +sigma+ (expt ,Temp 4))))
