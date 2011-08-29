(asdf:defsystem thermal-radiation-formulary
    :name "thermal-radiation-formulary"
    :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
    :version "0.1"
    :description "Mirko's thermal-radiation formulary"
    :components
    ((:module "setup"
	      :path #P"./"
	      :components
	      ((:file "thermal-radiation-formulary-package-def")
	       (:file "utilities"
		      :depends-on ("thermal-radiation-formulary-package-def"))))
     (:module "view-factors"
	      :depends-on ("setup")
	      :path #P"./"
	      :components
	      (("view-factors")))
     (:module "view-radiation"
	      :depends-on ("setup")
	      :path #P"./"
	      :components
	      (("radiation")))
     (:module "rest"))
	     (:file "rest"))
    :depends-on (:my-utils
		 :gsll
		 :physics-constants
		 :lisp-unit))
