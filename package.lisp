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
  (:export #:define-property #:define-property*
           #:undefine-property
           #:constrain
           #:declare-constraint))
