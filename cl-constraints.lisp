(in-package :cl-constraints)

;;; Utility functions
(declaim (inline strip-values-from-type))
(defun strip-values-from-type (form-type)
  (declare (optimize (speed 3) (space 3)))
  (if (and (listp form-type)
           (eql (first form-type) 'values))
      (second form-type)
      form-type))

(defun make-ignore-compare-fn (ignored-syms)
  (lambda (v1 v2 c)
    (if (member (lookup c :current-symbol) ignored-syms)
        v2
        ;; If propagating from `progn' to somewhere else,
        ;; we want to at least consider non-nil values,
        ;; but if it's nil then we want to ignore it
        ;; to reduce chaos.
        (or v2 v1))))


;; Each symbol has a map by default
(defparameter *symbol-db* (map :default
                               (map :default
                                    (map (:expand t)
                                         (:propagation-spec (lambda (form env)
                                                              (declare (ignore env))
                                                              (cdr form)))
                                         (:propagation-type t)))))

(defparameter *defaults* (map :default
                              (map (:expand t)
                                   (:propagation-spec (lambda (form env)
                                                        (declare (ignore env))
                                                        (cdr form)))
                                   (:propagation-type t))))

(defun* get-property (property (target (or symbol list)))
  (:returns map)
  (let* ((target-props (lookup *symbol-db* target))
         (found-target (nth-value 1 (lookup *symbol-db* target)))
         (prop-defaults (lookup *defaults* property))
         (target-prop
           ;; If there's an explicit definition for this property,
           ;; use it
           (cond ((nth-value 1 (lookup target-props property))
                  (lookup target-props property))
                 ((nth-value 1 (lookup target-props nil))
                  (lookup target-props nil))
                 (t (map)))))
    ;; Special cases and returning output
    (cond
      ;; Use the `:symbol' default config
      ;; if we don't have a special config
      ;; for this symbol.
      ((and
        (listp target)
        ;; Specific symbol
        (eql (first target) :symbol)
        ;; Symbol not found
        (not found-target))
       ;; Return the value for `:symbol'
       (get-property property :symbol))
      ;; If no special case, merge with the defaults
      ;; to get the final config.
      (t (map ($ prop-defaults) ($ target-prop))))))

(defun* get-property-value (property (target (or symbol list)) &rest funargs)
  "Gets the value of a property for a specified symbol-spec as stored
in `*symbol-db*'.
First checks for `:value', and if that's unavailable attempts to call
`:value-fn' (if it exists) to produce a value.
If provided, `funargs' are passed to the `:value-fn' function, should
it exist and be called."
  (let ((prop (get-property property target)))
    (list 'found (lookup prop :value))
    (or (lookup prop :value)
        (and (lookup prop :value-fn)
             (apply (lookup prop :value-fn) funargs)))))

(defmacro declare-property (property symbols
                            &key
                              (value t value-provided-p)
                              (value-fn nil value-fn-provided-p)
                              (propagates :up)
                              (compare-fn nil compare-fn-provided-p)
                              (expand t)
                              (propagation-spec nil propagation-spec-provided-p)
                              (propagation-type t))
  "Declares `symbols' to have property `property', which applies
to every form starting with a symbol in `symbols'.

If `property' is `nil' it is assigned as a \"default\" value, to be
used for `symbols' when they do not have a specific property declaration.

If `symbols' includes the keyword `:symbol', this is used to determine
the default assumption for bare symbols, should they be processed by
`constrain'.
If `symbols' includes the keyword `:atom', this is used to determine
the default assumption for atoms, should they be processed by `constrain'.

`propagates' determines how this property is propagated through code
from each of `symbols'. It is either a keyword, or a list of keywords
which will all be applied to the form.
- `:up' propagates from a form to its parents until a parent has
a value taking priority over the one being propagated.
- `:down' propagates from a form to its arguments until a child
has a value taking priority over the one being propagated.

`value' is the value assigned to this property for this form.
Default to `t'.

`compare-fn' determines, if there is a conflict between two
definitions of `property' for a form (e.g. the default definition
and the propagated one), which value takes precedence. If non-`nil',
it should be a 3-argument function.
`compare-fn' is called for both the symbol whose property is being
propagated and the symbol which is contradicting that propagation. Its
inputs are the property value of the 'current' form (i.e. the form whose
symbol `compare-fn' is associated with), the property value from the other
source, and a context object.
The results of both `compare-fn' executions are compared by `fset:compare',
which returns whichever output is considered `:greater' (or the parent form's
value if they are `:equal'/`:unequal').
Defaults to (lambda (a b c) a), i.e. returning the property value of its
associated symbol.

`expand' determines, for macros in `symbols', whether those macros are
expanded in `constrain'. Defaults to `t'.
Conflicts between the properties of the top-level form produced by
macroexpansion and the original top-level form are resolved via
`compare-fn'.

We currently do not support expanding local macro definitions or treating
them specially. Behavior in the presence of such definitions is undefined.

`propagation-type' determines which children of a form are considered
for propagation. It contains the type of the runtime value this form is
expected to have. The type of the form is inferred via `cl-form-types'
and tested via `subtypep' to conform to this spec.
Defaults to `t'

`propagation-spec' determines which children of a form are considered
for propagation. It contains a function which takes in `form' and `env'
and returns a list of subforms.
Defaults to the `cdr' of `form'"
  `(let ((new-map (map (:propagates ,propagates)
                       (:expand ,expand)
                       (:propagation-type ',propagation-type)))
         (symbols (apply
                   #'concatenate 'list
                   ;; Explicit symbols
                   ',(filter #'symbolp symbols)
                   (list ,@(filter (lambda (s) (not (symbolp s))) symbols)))))
     (declare (ignorable symbols))
     (cond
       (,(or value-provided-p
             (and symbols (not value-fn-provided-p)))
        (setf (lookup new-map :value) ,value))
       (,(and symbols (not value-provided-p))
        (setf (lookup new-map :value-fn) ,value-fn)))
     (when ,compare-fn-provided-p
       (setf (lookup new-map :compare-fn) ,compare-fn))
     (when ,propagation-spec-provided-p
       (setf (lookup new-map :propagation-spec) ,propagation-spec))
     (setf new-map (map ($ (lookup *defaults* ',property)) ($ new-map)))
     ,(if symbols
          `(iter (for sym in symbols)
             (setf (lookup (lookup *symbol-db* sym) ',property) new-map))
          `(setf (lookup *defaults* ',property) new-map))))

;;; FIXME: check this works for the special-cases
;;; in `declare-property'
(defmacro undeclare-property (property symbols)
  "Undeclares `symbols' to have property `property'."
  `(iter (for sym in ',symbols)
     (callf #'less (lookup *symbol-db* sym) ,property)))

(defmacro declare-property* (declarations)
  "Accepts multiple different argument lists, each of which is called as in `declare-property'"
  (when declarations
    `(progn
       ,(iter (for dec in declarations)
          (collecting `(declare-property ,@dec))))))

(defparameter *constraint-context* (map)
  "Tracks the context within a `constrain' form")

(defmacro assert-constraint (property &body body)
  "Used to assert a constraint so `constrain' calls matching that constraint don't check `body'"
  (declare (ignore property))
  `(progn ,@body))

(defmacro constrain (&whole form property (&key (report :warn)) &body body &environment env)
  "Constrains `body' to have property `property'.
`body' is considered to be an implicit progn form. Note that this `progn' is taken
into account for property propagation.

`report' determines how to report the results of `constrain':
- `:warn' emits a warning at compile time when the constraint is null
- `:error' emits a warning at compile time and an error at runtime when
the constraint is null
- If a 1-arg function, that function is called with the top level value of the
property to create a response. The return values of this function are ignored.
Note that if `body' has multiple forms, the result of `compare-fn'
the properties and "
  (let* ((constrain-form (if (> (length body) 1)
                             `(progn ,@body)
                             (first body)))
         ;; Refresh `*constraint-context*'
         (*constraint-context* (map (:property property)
                                    (:forms (map))))
         (valid (constrain-internal property (map) constrain-form env)))
    (if (functionp report)
        (funcall report valid)
        (unless valid
          (case report
            ;; TODO: Make a custom warning class
            (:warn (warn "failed to constrain property ~A~%Form:~%~A~%Subforms with property not known to be true:~%~{~A~%~}~%"
                         property form
                         (sort (convert 'list (filter (op (not _2)) (lookup *constraint-context* :forms)) :pair-fn (op _2 _1))
                               #'<
                               :key (op (length (format nil "~A" _))))))
            ;; TODO: Make a custom error class
            (:error (error "failed to constrain property ~A~%Form:~%~A~%Subforms with property not known to be true:~%~{~A~%~}~%"
                           property form
                           (sort (convert 'list (filter (op (not _2))
                                                        (lookup *constraint-context* :forms))
                                          :pair-fn (op _2 _1))
                                 #'<
                                 :key (op (length (format nil "~A" _))))))
            (otherwise
             (warn "Invalid report configuration for `constrain' form!~%Report config: ~A~%Validity: ~A~%Form:~%~A~%"
                   report
                   valid
                   form)))))
    constrain-form))
(defun* constrain-internal ((property symbol) (config map) original-form env
                            &optional propagate-down propagate-down-value
                            &aux
                            (form original-form)
                            (macro-original-sym nil)
                            (sym nil)
                            ;; Make a dynamically-local version of the
                            ;; variable for this case, to allow adding
                            ;; lexical information
                            (*symbol-db* *symbol-db*))
  (declare (ignorable config))
  ;; (print (list property form (when (listp form) (get-property-value property form form env))))

  ;; Special cases
  (block constrain-internal
    (cond
      ((symbolp form)
       (let* ((prop (get-property property :symbol))
              (up-prop (lookup prop :propagates))
              (up-prop (or (eql up-prop :up)
                           (and (listp up-prop)
                                (member :up up-prop))))
              (target `(:symbol ,form)))
         (return-from constrain-internal (if up-prop
                                             (values (get-property-value property target form env) target)
                                             (get-property-value property target form env)))))
      ((or
        (atom form)
        (and (listp form) (eql (first form) 'quote)))
       (let* ((prop (get-property property :atom))
              (up-prop (lookup prop :propagates))
              (up-prop (or (eql up-prop :up)
                           (and (listp up-prop)
                                (member :up up-prop)))))
         (return-from constrain-internal
           (if up-prop
               (values (get-property-value property :atom form env) :atom)
               (get-property-value property :atom form env)))))
      ((not (listp form)) (return-from constrain-internal (values nil nil))))

    (iter
      (setf sym (first form))
      (for expand-prop = (lookup (get-property property sym) :expand))
      (for expand-func = (or (and (functionp expand-prop) expand-prop)
                             (and (symbolp sym) (macro-function sym))))
      (cond
        ;; Ignore constrain forms in `form'
        ((eql sym 'constrain)
         ;; Process the `constrain' form's
         ;; form as a `progn'
         (setf form `(progn ,@(cddr form))))
        ;; Macroexpand macros if they're configured to do so for this proeprty
        ((and expand-prop expand-func)
         ;; Track the top-level non-`constrain' macro
         ;; so we can compare its properties with the final macroexpansion
         ;; result.
         (setf macro-original-sym (or macro-original-sym sym))
         (let ((new-form (funcall expand-func form env)))
           (if (equal new-form form)
               (return)
               (setf form new-form))))
        ;; Use the new value of form
        (t (return))))

    (nest
     (flet* ((compare-properties
              ((s1 (or symbol list)) v1 (s2 (or symbol list)) v2 &key (direction nil))
              (*let ((p1 map (get-property property s1))
                     (p2 map (get-property property s2))
                     (c1 (or function null) (lookup p1 :compare-fn))
                     (c2 (or function null) (lookup p2 :compare-fn))
                     (context map
                              (map (:current-symbol s1)
                                   (:propagated-symbol s2)
                                   (:direction direction))))
                ;; Redefine v1 and v2 in parallel
                (let ((v1 (if c1
                              (funcall c1 v1 v2 context)
                              v1))
                      (v2 (if c2
                              (funcall c2 v2 v1 context)
                              v2)))
                  ;; Compare the results
                  (cond
                    ((eql :greater (compare v1 v2))
                     v1)
                    ((eql :greater (compare v2 v1))
                     v2)
                    ((equal? v1 v2)
                     v1)
                    (t
                     (warn "encountered unequal but incomparable values for property ~A!~%sym: ~A, value: ~A~%sym: ~A, value: ~A~%~%"
                           property
                           s1 v1 s2 v2)
                     v1)))))))
     (*let ((prop map (get-property property sym))
            (current-prop-value (get-property-value property sym form env))
            (current-prop-value
             (if propagate-down
                 (compare-properties
                  sym current-prop-value
                  propagate-down propagate-down-value)
                 ;; Don't modify if there's no down-propagation going on
                 current-prop-value))
            (propagation-spec (lookup prop :propagation-spec))
            (subforms list (multiple-value-bind (subforms new-env)
                               (funcall propagation-spec form env)
                             (when new-env
                               (setf env new-env))
                             subforms))
            (propagation-type (lookup prop :propagation-type))
            (valid-subforms
             list
             (filter (op
                       ;; Check propagation type
                       (let ((form-type (strip-values-from-type (form-type _1 env))))
                         (subtypep form-type propagation-type env)))
                     subforms))
            (propagates (lookup prop :propagates))
            (propagate-down?
             (etypecase propagates
               (keyword (eql :down propagates))
               (list (member :down propagates))))
            (propagate-up?
             (etypecase propagates
               (keyword (eql :up propagates))
               (list (member :up propagates))))
            (recurse-results
             list
             (collect 'list
               (map-fn 'list
                       (lambda (subform)
                         (multiple-value-list
                          (if propagate-down?
                              (constrain-internal property config subform env
                                                  sym current-prop-value)
                              (constrain-internal property config subform env))))
                       (scan 'list valid-subforms))))
            ;; (_ (print (list "recurse" recurse-results)))
            (current-prop-value
             (collect-fn
              t (constantly current-prop-value)
              (lambda (curr result)
                (compare-properties
                 sym curr
                 ;; Returned symbol
                 (second result)
                 ;; Returned value
                 (first result)))
              (choose-if #'second (scan 'list recurse-results)))))
       (setf (lookup (lookup *constraint-context* :forms) original-form) current-prop-value)
       (if propagate-up?
           (values current-prop-value sym)
           (values current-prop-value))))))

;;; Utilities
(defun cut-compare-fn (v1 v2 context)
  (declare (ignore context))
  (and v1 v2))

(defun let-propagation-spec (form env)
  (let* ((let-args (second form))
         (let-body (cddr form))
         ;; Apply the declaration
         (let-declare (when
                          (and
                           ;; List check to avoid errors
                           ;; when invoking `first'
                           (listp (first let-body))
                           (eql (first (first let-body)) 'declare))
                        (prog1 (rest (first let-body))
                          (setf let-body (rest let-body)))))
         (let-names (image #'first let-args))
         (let-forms (image #'second let-args))
         (let-form-types (image (lambda (let-form)
                                  (strip-values-from-type (form-type let-form env)))
                                let-forms)))
    (setf env
          (augment-environment env
                               :variable let-names
                               :declare (concat
                                         (iter
                                           (for name in let-names)
                                           (for form-type in let-form-types)
                                           (collecting `(type ,form-type ,name)))
                                         let-declare)))
    (values
     (concat
      (image #'second (filter #'listp let-args))
      ;; TODO: Figure out how to add the environment into the
      ;; return string rather than a separate return value. The
      ;; current approach has issues with the edited environment
      ;; being used for the let argument bodies instead of just
      ;; the let body
      let-body)
     ;; Return the modified environment
     env)))

(defun let*-expansion (form env)
  (declare (ignore env))
  (let ((let-args (second form))
        (let-body (cddr form)))
    (case (length let-args)
      (0 `(progn ,@let-body))
      (1 `(let ,let-args ,@let-body))
      (otherwise `(let (,(first let-args))
                    (let* (,@(rest let-args))
                      ,@let-body))))))

;;; TODO: Figure out how `lambda-propagation-spec'
;;; should work. `lambda' functions aren't immediately
;;; evaluated, but there has to be some way to track
;;; variables referring to them and then convert the
;;; type spec into the funcall spec.
;;; NOTE: This would also unblock tracking properties
;;; through local function definitions like `flet'/`labels'.
;;; TODO: Use this utility to apply constraints to unknown
;;; functions by retrieving their `function-lambda-expression'
;;; and validating the constraint on that
(defun lambda-propagation-spec (form env)
  (*let ((lambda-args list (second form))
         ((:values required-args
                   optional-args
                   rest-arg
                   keyword-args
                   _
                   aux-args
                   _)
          (parse-ordinary-lambda-list lambda-args))
         ((:values lambda-body
                   lambda-declare
                   _)
          (parse-body (cddr form) :documentation t)))
    (*let (
           ;; Extract declarations from the `declare' form
           (lambda-declare (cdar lambda-declare))
           ;; Get non-aux argument names
           (non-aux-arg-names
            (concat required-args
                    (image #'first optional-args)
                    rest-arg
                    (image #'cadar keyword-args))))
      ;; Update the environment with the existence of non-aux arguments
      ;; NOTE: This needs to be done before processing the forms for
      ;; the aux arguments
      (setf env (augment-environment env :variable non-aux-arg-names))
      ;; Update `env' with the types for the aux args
      ;; FIXME: Augment the environment incrementally
      ;; with each aux type. Preferably while evaluating
      ;; the forms one by one somehow, rather than within
      ;; the propagation spec...
      (iter
        (for aux-spec in aux-args)
        (for aux-name = (first aux-spec))
        (for aux-form = (second aux-spec))
        (for aux-form-type = (strip-values-from-type (form-type aux-form env)))
        (setf env
              (augment-environment
               env
               :variable `(,aux-name)
               :declare `((type ,aux-form-type ,aux-name)))))

      ;; Apply the explicit declarations from the lambda
      (setf env (augment-environment env :declare (cdar lambda-declare)))

      ;; Return the lambda body and modified environment
      (values lambda-body env))))

;; Just ignore the type spec for the contents of `the' forms
;; NOTE: Not overriding the expansion because we want to use
;; the type information when processing the parent form
(defun the-propagation-spec
    (form env)
  (declare (ignore env))
  (list (third form)))

;;; FIXME: Seeing errors in SBCL
(defun locally-propagation-spec
    (form env)
  ;; (print form)
  (*let (((:values body declaration _) (parse-body (cdr form))))
    (when declaration
      ;; (augment-environment env :declare '((type integer a)))
      ;; (print "don1")
      ;; (augment-environment env :declare (cdar declaration))
      ;; (print "don")
      (setf env
            (augment-environment env
                                 :declare (cdar declaration))))
    (values body env)))
;;; NOTE: Hack to ignore locally entirely since it's causing issues
(defun locally-expansion
    (form env)
  (declare (ignore env))
  (*let (((:values body _ _) (parse-body (cdr form))))
    `(progn ,@body)))

(defun cond-propagation-spec (form env)
  (declare (ignore env))
  ;; Return a list of all the sub-forms converted to progns
  ;; NOTE: Ideally we'd have some way to nest type propagation
  ;; so we could modify the env when within a specific
  ;; sub-case. Too much work though.
  (image (op `(progn ,@_)) (cdr form)))

;; NOTE: Doesn't account for type information!
;; (defun typecase-propagation-spec
;;     (form env)
;;   (declare (ignore env))
;;   (*let ((targ-form (second form))
;;          (type-forms (image
;;                       ;; Replace the type spec with `progn'
;;                       (op (cons 'progn (cdr _)))
;;                       (cddr form))))
;;     ;; Return both the target form and all the dispatched forms
;;     (cons targ-form type-forms)))

(defun typecase-expansion (form env)
  (declare (ignore env))
  (*let ((targ-form (second form))
         (type-forms (image
                      ;; Replace the type spec with `progn'
                      (op (cons 'progn (cdr _)))
                      (cddr form))))
    ;; Return both the target form and all the dispatched forms
    ;; as a single `progn'
    `(progn ,targ-form ,@type-forms)))

;;; Default declarations
(declare-property nil (assert-constraint)
                  ;; Replicate the behavior for `progn'
                  ;; :value t
                  :value-fn
                  (lambda (form env)
                    (declare (ignore env))
                    (*let ((asserted-prop (second form))
                           (asserted-value t)
                           (value t))
                      (when (listp asserted-prop)
                        (let ((keys-plist (rest asserted-prop)))
                          ;; Get the property from the first element of the config
                          (callf #'first asserted-prop)
                          ;; Get the intended value if it's configured
                          (setf asserted-value (getf keys-plist :value
                                                     ;; Default to the current value
                                                     asserted-value))))
                      (when (eql asserted-prop (lookup *constraint-context* :property))
                        (setf value asserted-value))
                      value))
                  :expand nil
                  :propagation-spec
                  (lambda (form env)
                    (declare (ignore env))
                    (*let ((asserted-prop (second form)))
                      (when (listp asserted-prop)
                        (callf #'first asserted-prop))
                      ;; When the assertion equals the property asserted,
                      ;; we don't care about the body
                      (unless (eql asserted-prop (lookup *constraint-context* :property))
                        ;; Otherwise return the body
                        (cddr form)))))
(declare-property nil (progn prog1 prog2)
                  :value t)
(declare-property nil (locally)
                  :value t
                  ;; :propagation-spec #'locally-propagation-spec
                  :expand #'locally-expansion)
(declare-property nil (let)
                  :expand nil
                  :propagation-spec #'let-propagation-spec)
(declare-property nil (let)
                  :expand #'let*-expansion)
(declare-property nil (the)
                  :value t
                  :propagation-spec #'the-propagation-spec)
(declare-property nil (typecase etypecase)
                  :value t
                  ;; :propagation-spec #'typecase-propagation-spec
                  :expand #'typecase-expansion)
(declare-property nil (cond)
                  :value t
                  :propagation-spec #'cond-propagation-spec)

;; Serapeum default declarations
(declare-property nil (serapeum::truly-the)
                  :value t
                  :propagation-spec #'the-propagation-spec)
(defun with-subtype-dispatch-expansion (form env)
  (declare (ignore env))
  (let ((var (fourth form))
        (known-type (second form))
        (possible-types (third form))
        (body (nthcdr 4 form)))
    (if (not possible-types)
        ;; No typecase if we don't do dispatch
        `(locally
             (declare (type ,known-type ,var))
           ,@body)
        ;; Copy the body for each dispatched type
        ;; if we are doing dispatch
        `(locally
             (declare (type ,known-type ,var))
           ;; Body is identical across variants, and
           ;; expansions ignore local type info, so
           ;; we may as well get rid of the duplication
           (progn ,var ,@body)
           ;; (etypecase ,var
           ;;   ,@(iter
           ;;       (for type in (append possible-types `(,known-type)))
           ;;       (collecting `(,type ,@body))))
           ))))
(declare-property nil (with-subtype-dispatch)
                  :expand #'with-subtype-dispatch-expansion)

(declare-property :non-mutating nil :compare-fn #'cut-compare-fn)
(declare-property :non-mutating (:atom))
(declare-property :non-mutating (:symbol))
(declare-property :non-mutating (print) :value-fn (lambda (form env) (declare (ignore env)) (if (third form) nil t)))
(declare-property :non-mutating (
                                 ;; Control flow functions
                                 identity
                                 ;; List predicates
                                 null
                                 ;; Sequence predicates
                                 emptyp
                                 fset:sort
                                 ;; Numeric predicates
                                 < <= > >= =
                                 abs
                                 ;; Boolean predicates
                                 and or if when unless
                                 ;; Type predicates
                                 listp consp
                                 vectorp arrayp
                                 integerp rationalp floatp realp numberp
                                 symbolp packagep
                                 typep
                                 ;; List operators
                                 first cl:first second third fourth fifth sixth seventh eighth ninth tenth
                                 cl:last fset:last
                                 rest
                                 nth nthcdr
                                 ;; car and friends
                                 (iter outer (for len from 1 to 5)
                                   (iter
                                     (iter:with num-to-string-format =
                                                (str:concat "~" (write-to-string len) ",'0b"))
                                     (for i below (expt 2 len))

                                     (for i-str = (format nil num-to-string-format i))
                                     (for mid-str = (~>> i-str
                                                         (str:replace-all "0" "a")
                                                         (str:replace-all "1" "d")))
                                     (for sym = (find-symbol (str:upcase (str:concat "c" mid-str "r")) :cl))
                                     (when sym
                                       (in outer (collecting sym)))))
                                 append concatenate
                                 ;; Array operators
                                 make-array aref
                                 ;; Sequence operators
                                 length elt
                                 ;; Numeric operators
                                 + - * /
                                 ))
(declare-property :non-mutating (let) :expand nil :propagation-spec #'let-propagation-spec)

(declare-property :non-consing nil :compare-fn #'cut-compare-fn)
(declare-property :non-consing (:atom))
(declare-property :non-consing (:symbol))
(declare-property :non-consing (
                                ;; Control flow functions
                                identity
                                ;; Destructive :non-consing operators
                                nreverse
                                nconc nreconc
                                delete delete-if delete-if-not delete-duplicates
                                nsubstitute nsubstitute-if nsubstitute-if-not
                                nsublis nsubst nsubst-if nsubst-if-not
                                nbutlast
                                nsplice-seq nsplice-seqf
                                ;; List predicates
                                null
                                ;; Numeric predicates
                                < <= > >= =
                                ;; Boolean predicates
                                and or if when unless
                                ;; Type predicates
                                listp consp
                                vectorp arrayp
                                integerp rationalp floatp realp numberp
                                symbolp packagep
                                ;; List operators
                                cl:first second third fourth fifth sixth seventh eighth ninth tenth
                                cl:last
                                rest
                                nth nthcdr
                                ;; car and friends
                                (iter outer (for len from 1 to 5)
                                  (iter
                                    (iter:with num-to-string-format =
                                               (str:concat "~" (write-to-string len) ",'0b"))
                                    (for i below (expt 2 len))

                                    (for i-str = (format nil num-to-string-format i))
                                    (for mid-str = (~>> i-str
                                                        (str:replace-all "0" "a")
                                                        (str:replace-all "1" "d")))
                                    (for sym = (find-symbol (str:upcase (str:concat "c" mid-str "r")) :cl))
                                    (when sym
                                      (in outer (collecting sym)))))
                                ;; Array operators
                                aref
                                ;; Sequence operators
                                elt
                                ;; length ;; Is this really non-consing?
                                ))
;;; Arithmetic operators are only non-consing for fixnum inputs and outputs
;;; NOTE: Is this fully correct?
(declare-property :non-consing (+ - *)
                  :value-fn
                  (lambda (form env)
                    (let ((form-type (strip-values-from-type (form-type form env)))
                          (arg-types (nest
                                      (collect 'list)
                                      (map-fn '(or symbol list null) #'strip-values-from-type)
                                      (map-fn '(or symbol list null) (rcurry #'form-type env))
                                      (scan 'list)
                                      (rest form))))
                      (nest
                       ;; If any type is invalid, return false
                       (not)
                       (collect 'list)
                       ;; Find types which are INVALID for :non-consing
                       (choose-if (op (not (subtypep _ 'fixnum env))))
                       (scan 'list)
                       (cons form-type arg-types)))))
(declare-property :non-consing (abs)
                  :value-fn
                  (lambda (form env)
                    (*let ((targ (second form))
                           (targ-type (strip-values-from-type (form-type targ env))))
                      ;; If the expected type of the argument is a `fixnum',
                      ;; it can be negated without consing
                      (subtypep targ-type 'fixnum env))))
(declare-property :non-consing (let) :expand nil :propagation-spec #'let-propagation-spec)


(comment
  ;;; FIXME: there is some kind of contagion between failures in nested `constrain' forms.
  ;;; NOTE: Is this just re-evaluation of the same `constrain' form?
  (let ((a 1) (b 2))
    (time (constrain :non-consing nil
            (constrain :non-mutating nil
              (+ 1 2))
            (constrain :non-mutating nil
              (identity (+ a b))))))
  ;; FIXME: there is something wrong with how we're processing `non-mutating'
  (let ((a 1) (b 2))
    (time (constrain :non-mutating nil
            (constrain :non-mutating nil
              (identity (+ a b))))))
  ;; TODO: Add facilities for specific forms to define which elements to look
  ;; at to get their subforms.
  ;; NOTE: I think this is already done via `:propagation-spec'
  ;; TODO: Merge configs with the stack of default configs, rather than
  ;; using a switch based on whether values are available or not
  ;; NOTE: This is partly done, but I don't think we incorporate *both*
  ;; of the symbol default and the property default
  )
