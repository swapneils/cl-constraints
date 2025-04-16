(uiop:define-package :cl-constraints/test
  (:mix :cl-constraints :try :alexandria :serapeum))

(in-package :cl-constraints/test)

(defmacro no-constrain-errors (form)
  `(handler-case (and (macroexpand ',form) t)
     (simple-warning (w)
       (print "failed")
       (warn w)
       nil)))

;;; FIXME: tests fail even though REPL testing succeeds...
(deftest basic-constraints ()
  ;; Does not throw a warning
  (is
   (equal (let ((a (list 1 2)) (b (list 3 4)))
            (constrain :non-consing nil
              (identity (nconc a b))))
          '(1 2 3 4)))
  (is
   (no-constrain-errors
    (constrain :non-consing nil
      (identity (nconc '(1 2) '(3 4))))))
  (is (equal
       (constrain :non-mutating nil
         (let ((a 3))
           (declare (ignore a))
           (prog1
               (+ 3 2)
             (- 3 2))))
       5))
  (is
   (no-constrain-errors
    (constrain :non-mutating nil
      (let ((a 3))
        (declare (ignore a))
        (prog1
            (+ 3 2)
          (- 3 2))))) )
  (is (equal
       (constrain :non-mutating nil
         (let ((a 3))
           (declare (ignore a))
           (prog2 20
               (+ 3 2)
             (- 3 2))))
       5))
  (is
   (no-constrain-errors
    (constrain :non-mutating nil
      (let ((a 3))
        (declare (ignore a))
        (prog2 20
            (+ 3 2)
          (- 3 2))))))
  (is (equal
       (let ((a (iota 3))
             (b (iota 3)))
         (constrain :non-mutating nil (let ((c (append a b))) (the list c))))
       '(0 1 2 0 1 2)))
  (is
   (no-constrain-errors
    (constrain :non-mutating nil (let ((c (append a b))) (the list c)))))
  (is
   (not
    (no-constrain-errors
     (constrain :non-consing nil (let ((c (append a b))) (the list c))))))
  (is
   (no-constrain-errors
    (constrain :non-mutating nil (let ((c (append a b))) (prog1 c)))))
  (is
   (not
    (no-constrain-errors
     (constrain :non-consing nil (let ((c (append a b))) (prog1 c))))))
  (is (equal
       (let ((a 3) (b 3) (threshold 0.0001))
         (let ((c (- a b)))
           (constrain :non-consing nil
             (serapeum:nest
              (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) a)
              (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) b)
              (or (= a b)
                  ;; For floats, also allow them to be "close enough"
                  (and (floatp a) (floatp b)
                       (<= (assert-constraint :non-consing (abs c)) threshold)))))))
       t))
  (is
   (not
    (no-constrain-errors
     (constrain :non-consing nil
       (serapeum:nest
        (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) a)
        (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) b)
        (or (= a b)
            ;; For floats, also allow them to be "close enough"
            (and (floatp a) (floatp b)
                 (<= (assert-constraint (:non-consing :value nil) (abs c)) threshold))))))))
  (is
   (no-constrain-errors
    (constrain :non-consing nil
      (serapeum:nest
       (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) a)
       (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) b)
       (or (= a b)
           ;; For floats, also allow them to be "close enough"
           (and (floatp a) (floatp b)
                (<= (assert-constraint (:non-consing) (abs c)) threshold)))))))
  (is
   (no-constrain-errors
    (constrain :non-consing nil
      (serapeum:nest
       (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) a)
       (serapeum:with-subtype-dispatch number (fixnum ratio single-float double-float) b)
       (or (= a b)
           ;; For floats, also allow them to be "close enough"
           (and (floatp a) (floatp b)
                (<= (assert-constraint :non-consing (abs c)) threshold))))))))
