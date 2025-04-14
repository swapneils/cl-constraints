(uiop:define-package :cl-constraints
  (:mix
   :fset
   :alexandria
   :series :iterate
   :serapeum
   :uiop
   :cl
   :cltl2
   :defstar
   :cl-form-types)
  (:nicknames "constraints")
  (:export #:declare-property #:declare-property*
           #:undeclare-property
           #:constrain))
