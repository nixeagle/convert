(defpackage #:convert
  (:use :cl)
  (:export #:conv
           #:convert
           #:define-convert))

(in-package :convert)
(defgeneric convert (object result-type input-type &key &allow-other-keys)
  (:documentation "Convert OBJECT to RESULT-TYPE.

Specify what the type of OBJECT should be interpreted as with
INPUT-TYPE. For example if you want to `convert' something to an array of
hexidecimal numbers, you might define a method on `convert' where the
RESULT-TYPE is `array' and the INPUT-TYPE is `hexadecimal-digit'."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun valid-type-specifier-p (type-specifier &key environment (default t))
    "True if TYPE-SPECIFIER is valid.

This is pretty implementation specific and for now will work only on sbcl.

Returns DEFAULT is if the lisp implementation is not supported."
    #+ (or sbcl ccl) (declare (ignore default))
    #+ccl (declare (ignore environment))
    #+sbcl (sb-ext:valid-type-specifier-p type-specifier environment)
    #+ccl (ccl:type-specifier-p type-specifier)
    #-(or ccl sbcl) default))

(defmacro conv (object result-type &rest keys)
  "Helps implementation figure out OBJECT's new TYPE.

This is just a helper macro to make declaring the result type simpler and
inline with what is expected of `coerce'."
  `(the (values ,(if (valid-type-specifier-p result-type)
                     result-type
                     t) &optional)
     (convert ,object ,(if (valid-type-specifier-p result-type)
                           `',(if (listp result-type)
                                  (car result-type)
                                  result-type)
                           result-type)
              ,@(cond
                 ((keywordp (car keys))
                  (cons t keys))
                 ((valid-type-specifier-p (car keys))
                  (cons `',(car keys) (cdr keys)))
                 (t keys)))))


(defmacro define-convert ((object result-type &optional input-type &rest keys)
                          &body body)
  `(defmethod convert ((,object ,object) (,result-type (eql ',result-type))
                       ,(if (and input-type
                                 (not (member input-type '(t &key &rest)
                                              :test #'eq)))
                            `(,input-type (eql ',input-type))
                            `(,(gensym) t)) ,@(if (member input-type '(t &key &rest)
                                                          :test #'eq)
                            `(,input-type ,@keys)
                            (or keys (list '&key))))
     ,@body))

#+ ()
(defclass convertable-slots-metaclass (standard-class) ())
#+ ()
(defmethod validate-superclass ((class convertable-slots-metaclass)
                                (super standard-class))
  "convertable-slots classes may inherit from standard classes."
  t)

#+ ()
(defmethod direct-slot-definition-class ((class convertable-slots-metaclass) &rest initargs)
  #+ () (find-class 'convertable-direct-slot-definition))

#+ ()
(defmethod validate-superclass ((class standard-class)
                                (super convertable-slots-metaclass))
  "Standard classes may inherit from convertable-slots classes.

This is legit to do as no class mechanics are modified other then how
`make-instance' treats initargs if the argument is not of the specified
type but _can_ become the specified type vie `convert'.

If you inherit from a convertable class to a standard class, all the slots
will not be convertable, including those slots defined on the convertable
class."
  t)
#+ ()
(defmethod (setf slot-value-using-class) :around
    (new-value (class convertable-slots-metaclass)
               object (slot standard-slot-definition))
  (let ((slot-type (slot-definition-type slot)))
    (call-next-method (if (typep new-value slot-type)
                          new-value
                          (convert new-value slot-type nil))
                      class object slot)))

;;; These define convert forms that just call coerce.
(define-convert (sequence list)
  "Coerce SEQUENCE to a list."
  (coerce sequence 'list))
(define-convert (sequence vector)
  "Coerce SEQUENCE to a vector."
  (coerce sequence 'vector))
(define-convert (string character)
  "Coerce STRING of length 1 to a CHARACTER."
  (coerce string character))
(define-convert (real single-float)
  "Coerce REAL to a SINGLE-FLOAT"
  (coerce real 'single-float))
(define-convert (symbol function)
  "Coerce SYMBOL to a FUNCTION if it is globally bound."
  (coerce symbol function))