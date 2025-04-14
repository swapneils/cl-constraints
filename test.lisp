(uiop:define-package :cl-constraints/test
  (:mix :cl-constraints :try))

(in-package :cl-constraints/test)

(deftest basic-constraints ()
  ;; Does not throw a warning
  (is
   (equal (let ((a (list 1 2)) (b (list 3 4)))
            (constrain non-consing nil (identity (nconc a b))))
          '(1 2 3 4)))
  (is (handler-case
          (macroexpand '(constrain non-consing nil (identity (nconc a b))))
        (simple-warning nil)))
  (is
   (handler-case
       (macroexpand '(constrain non-consing nil (let ((a 3)) (declare (fixnum a)) (+ 3 2))))
     (simple-warning nil))))
