(in-package :ffavor)

(defun now ()
  (get-universal-time))

(defun n-days-ago (n)
  (assert (and (not (minusp n)) (integerp n)))
  (- (get-universal-time) (* 60 60 24 n)))
