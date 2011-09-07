(defpackage :thermal-radiation-formulary-unit-tests
  (:nicknames :radiation-tests)
  (:use :cl :gsll :thermal-radiation-formulary :lisp-unit :my-lisp-unit :my-utils)
  (:import-from :physics-constants +boltzmann-constant-SP+))


(in-package :thermal-radiation-formulary-unit-tests)
(define-symbol-macro +kb+ physics-constants:+boltzmann-constant-SP+)


;; calculate equilibrium temperature and check that heat flux from top
;; to center is same as from center to bottom
(define-test parallel-plate-equilibrium-sigma=1
  (let ((T0 100)
	(T1 400)
	Tp)
    (setf Tp (parallel-plate-equilibrium T0 T1))
    (assert-number-equal (- (st4 T1) (st4 Tp))
		     (- (st4 Tp) (st4 T0)))))

;; same as above, but for a problem emissivities that are not unity
(define-test parallel-plate-equilibrium-sigma#1
  (let* ((T0 100)
	 (T1 400)
	 (beta0 0.5)
	 (beta1 0.2)
	 (Tp (parallel-plate-equilibrium T0 T1 beta0 beta1)))
    (assert-number-equal (* beta1 (- (st4 T1) (st4 Tp)))
		     (* beta0 (- (st4 Tp) (st4 T0))))))
		   

;;;; Three parallel plates test.  The numerical values are results of
;;;; calculations on SBCL.  But these were checked against results
;;;; obtained in the SWP document.
(defmacro def-3parallel-plate-test (name funct args numerical-value)
  `(define-test ,name
     (let ((problem (,funct ,@args)))
       (my-utils:send problem :solve)
       (assert-equal 'gsll::vector-double-float
		     (type-of (my-utils:send problem :check)))
       (assert-numerical-equal (gsl:cl-array (my-utils:send problem :rhs))
			       (gsl:cl-array (my-utils:send problem :check)))
       (assert-numerical-equal ,numerical-value
			       (gsl:cl-array (my-utils:send problem :fluxes))))))

(def-3parallel-plate-test 3parallel-plate-equilibrium-temp-black-emissivities
    3parallel-plate-equilibrium-temp (1 573 1 1 1 300)
    #(6112.794921875d0 3286.052703857422d0
      3286.052703857422d0
      459.31048583984375d0 3286.052703857422d0))

(def-3parallel-plate-test 3parallel-plate-equilibrium-temp-gray-emissivities
    3parallel-plate-equilibrium-temp (0.5 573 0.5 0.5 0.5 300)
    #(5170.547515869141d0 4228.30010986328d0
      2343.805297851562d0
      1401.557891845703d0 3286.052703857421d0))

(def-3parallel-plate-test 3parallel-plate-equilibrium-flux-black-emissivities
    3parallel-plate-equilibrium-flux (1 573 1 1 673 1 300)
    #(6112.794921875d0 16693.353576660156d0 16693.353576660156d0
      459.31048583984375d0 16693.353576660156d0))


(def-3parallel-plate-test 3parallel-plate-equilibrium-flux-grey-emissivities
    3parallel-plate-equilibrium-flux (0.5 573 0.5 0.5 673 0.5 300)
    #(6435.703536987305d0 6758.612152099609d0 4874.117340087891d0
      2666.713912963867d0 7081.520767211914d0))




;;;; View factor tests
