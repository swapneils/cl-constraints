(uiop:define-package :cl-constraints/test
  (:mix :cl-constraints :try))

(in-package :cl-constraints/test)

(deftest basic-constraints ()
  (let ((a (list 1 2)) (b (list 3 4))) (time (constrain non-consing nil (identity (nconc a b))))))
