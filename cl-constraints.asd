(defsystem "cl-constraints"
  :author "Swapneil Singh <swapneil.singh@gmail.com>"
  :maintainer "Swapneil Singh <swapneil.singh@gmail.com>"
  :mailto "swapneil.singh@gmail.com"
  :license "MIT"
  :depends-on
  ("alexandria"
   "serapeum"
   "trivia"
   "iterate"
   "series"
   "fset"
   "trivial-cltl2"
   "cl-form-types"
   "defstar")
  :components
  ((:file "package")
   (:file "cl-constraints" :depends-on ("package"))))
