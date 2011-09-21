(asdf:defsystem thermal-radiation-formulary
  :name "thermal-radiation-formulary"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "Mirko's thermal-radiation formulary"
  :components
  ((:module "setup"
	    :pathname #P"./"
	    :components
	    ((:file "thermal-radiation-formulary-package-def")
	     (:file "utilities"
		    :depends-on ("thermal-radiation-formulary-package-def"))))
   (:module "view-factors"
	    :depends-on ("setup")
	    :pathname #P"./"
	    :components
	    ((:file "view-factors")
	     (:file "disk+ring-view-factors")
	     (:file "rectangular-area-view-factors")
	     (:file "inner-cylinder-surfaces-view-factors")))
   (:module "spectral-calculations"
	    :depends-on ("setup")
	    :pathname #P"./"
	    :components
	    ((:file "radiation")))
   (:module "rest"
	    :depends-on ("setup")
	    :pathname #P"./"
	    :components
	    ((:file "rest"))))
  :depends-on (:my-utils
	       :gsll
	       :physics-constants
	       :lisp-unit))
