(cl:in-package :islisp-impl)

(defun islisp:property (symbol indicator)
  (get symbol indicator))

(defun islisp:set-property (obj symbol indicator)
  (setf (get symbol indicator) obj))

(defun (setf islisp:property) (obj symbol indicator)
  (islisp:set-property (obj symbol indicator)))

(defun islisp:remove-property (symbol indicator)
  (if (eql (get symbol indicator :failure) :failure)
    nil
    (remprop symbol indicator)))

(defun islisp:quotient (dividend &rest divisors)
  (apply #'/ (float dividend) divisors))

(defun islisp:reciprocal (dividend divisor)
  (/ (float dividend) divisor))

(defconstant *pi* (float pi))

(defconstant *most-positive-float* most-positive-single-float)

(defconstant *most-negative-float most-negative-single-float)

(defun islisp:floor (x)
  (values (floor x)))

(defun islisp:ceiling (x)
  (values (ceiling x)))

(defun islisp:truncate (x)
  (values (truncate x)))

(defun islisp:round (x)
  (values (round x)))

(defun islisp:div (x y)
  (values (floor (/ x y))))

(defun islisp:set-car (obj cons)
  (setf (car cons) obj))

(defun islisp:set-cdr (obj cons)
  (setf (cdr cons) obj))

(defun islisp:create-list (i &optional initial-element)
  (cond
    ((= i 0) '())
    (t (cons
         initial-element
         (islisp:create-list (- i 1) initial-element)))))

