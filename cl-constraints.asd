(defsystem "cl-constraints"
  :author "Swapneil Singh <swapneil.singh@gmail.com>"
  :maintainer "Swapneil Singh <swapneil.singh@gmail.com>"
  :mailto "swapneil.singh@gmail.com"
  :license "MIT"
  :version "0.0.1"
  :depends-on
  ("alexandria"
   "serapeum"
   "iterate"
   "series"
   "fset"
   "trivial-cltl2"
   "cl-form-types"
   "defstar"
   "str")
  :components
  ((:file "package")
   (:file "cl-constraints" :depends-on ("package")))
  :in-order-to ((test-op (test-op "cl-constraints/test"))))


(defsystem "cl-constraints/test"
  :author "Swapneil Singh <swapneil.singh@gmail.com>"
  :maintainer "Swapneil Singh <swapneil.singh@gmail.com>"
  :mailto "swapneil.singh@gmail.com"
  :license "MIT"
  :version "0.0.1"
  :depends-on
  ("cl-constraints")
  :components
  ((:file "test"))
  :perform (test-op (o c) (uiop:symbol-call :cl-constraints/test :basic-constraints)))
