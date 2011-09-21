;; Mirko Vukovic
;; Time-stamp: <2011-09-21 14:49:29EDT view-factors-utilities.lisp>
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

;; helper functions and macros for defining view-factors.  
(defmacro F12-bind ((f12 &optional a1 a2) &body body)
  "Binds results of f12 and area calculations"
  `(multiple-value-bind (,f12 ,@(if a1 `(,a1 ,@(if a2 `(,a2)))))
			 ,@body))

(defmacro assert-f12-equal (form1 form2)
  `(assert-number-equal
    (f12-bind (f12-1) ,form1 f12-1)
    (f12-bind (f12-2) ,form2 f12-2)))