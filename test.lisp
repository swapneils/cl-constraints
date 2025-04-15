(uiop:define-package :cl-constraints/test
  (:mix :cl-constraints :try))

(in-package :cl-constraints/test)

;;; FIXME: tests fail even though REPL testing succeeds...
(deftest basic-constraints ()
  ;; Does not throw a warning
  (is
   (equal (let ((a (list 1 2)) (b (list 3 4)))
            (constrain :non-consing nil
              (identity (nconc a b))))
          '(1 2 3 4)))
  (is (handler-case
          (macroexpand '(constrain :non-consing nil
                         (identity (nconc '(1 2) '(3 4)))))
        (simple-warning (w) (print "failed")
          (warn w) nil)))
  (is (equal
       (constrain :non-mutating nil
         (let ((a 3))
           (declare (ignore a))
           (prog1
               (+ 3 2)
             (- 3 2))))
       5))
  (is
   (handler-case
       (macroexpand '(constrain :non-mutating nil
                      (let ((a 3))
                        (declare (ignore a))
                        (prog1
                            (+ 3 2)
                          (- 3 2)))))
     (simple-warning (w) (print "failed")
       (warn w) nil)))
  (is (equal
       (constrain :non-mutating nil
         (let ((a 3))
           (declare (ignore a))
           (prog2 20
               (+ 3 2)
             (- 3 2))))
       5))
  (is
   (handler-case
       (macroexpand '(constrain :non-mutating nil
                      (let ((a 3))
                        (declare (ignore a))
                        (prog2 20
                            (+ 3 2)
                          (- 3 2)))))
     (simple-warning (w) (print "failed")
       (warn w) nil))))
