(in-package :ffavor)

(defun now ()
  (get-universal-time))

(defun n-days-ago (n)
  (assert (and (not (minusp n)) (integerp n)))
  (- (get-universal-time) (* 60 60 24 n)))

(defun mkhash (&rest keys-and-values &aux (table (make-hash-table :test #'equal)))
  "Convenience function for `literal' hash table definition."
  (loop for (key val) on keys-and-values by #'cddr do (setf (gethash key table) val)
     finally (return table)))

(defun hashget (hash &rest keys)
  "Convenience function for recursively accessing hash tables."
  (reduce (lambda (h k) (gethash k h)) keys :initial-value hash))

(define-compiler-macro hashget (hash &rest keys)
  (if (null keys) hash
      (let ((hash-sym (make-symbol "HASH"))
            (key-syms (loop for i below (length keys)
                         collect (make-symbol (format nil "~:@(~:R~)-KEY" i)))))
        `(let ((,hash-sym ,hash)
               ,@(loop for key in keys for sym in key-syms
                    collect `(,sym ,key)))
           ,(reduce (lambda (hash key) `(gethash ,key ,hash))
                    key-syms :initial-value hash-sym)))))

(defun (setf hashget) (new-value hash key &rest more-keys)
  "Uses the last key given to hashget to insert NEW-VALUE into the hash table
returned by the second-to-last key.
tl;dr: DWIM SETF function for HASHGET."
  (if more-keys
      (setf (gethash (car (last more-keys))
                     (apply #'hashget hash key (butlast more-keys)))
            new-value)
      (setf (gethash key hash) new-value)))

