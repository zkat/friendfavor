(in-package :ffavor)

;;;
;;; Metrics
;;;

;;; Generics
(defgeneric personal-favor (observer specimen from to)
  (:documentation "Favor rating for SPECIMEN based on OBSERVER's own transactions."))

(defgeneric global-favor (user from to)
  (:documentation "Returns the global favor accumulated by user between FROM and TO."))

(defgeneric friend-favor (observer specimen from to)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. friend favor is a measurement of what those that you have positive opinions of, and their
positive connections, think of SPECIMEN."))

(defgeneric enemy-favor (observer specimen from to)
  (:documentation "Retuns the enemy favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. enemy favor is a measurement of what those that you have negative opinions of, and their
positive connections, think of SPECIMEN."))

;;; Personal favor
(defparameter *repeated-favor-decay* 4/5)

(defun transaction-count (judge-id target-id from to favorp)
  (query (:select (:count :*) :from 'transaction
                  :where (:and (:> 'timestamp from)
                               (:> to 'timestamp)
                               (:= 'target-id target-id)
                               (:= 'source-id judge-id)
                               (:= 'favorp favorp)))
         :single))

(defun favor-count (judge target from to)
  (transaction-count judge target from to t))

(defun disfavor-count (judge target from to)
  (transaction-count judge target from to nil))

(defun geometric-sum (first-term common-ratio num-terms)
  "sum of a + ar + ar^2 + ... + ar^(n-1)"
  (/ (* first-term (- 1 (expt common-ratio num-terms)))
     (- 1 common-ratio)))

(defmethod personal-favor ((judge integer) (target integer) from to)
  (let ((favor-value (geometric-sum 1 *repeated-favor-decay*
                                    (favor-count judge target from to)))
        (disfavor-value (geometric-sum 1 *repeated-favor-decay*
                                       (disfavor-count judge target from to))))
    (- favor-value disfavor-value)))

(defmethod personal-favor ((judge user) (target user) from to)
  "Calculates the personal favor from JUDGE to TARGET. *REPEATED-FAVOR-DECAY* represents how quickly repeated
favor/disfavors might decay in value. When *REPEATED-FAVOR-DECAY* < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (personal-favor (user-id judge) (user-id target) from to))

;;; Global Favor
(defmethod global-favor ((user user) from to)
  (reduce #'+ (mapcar (lambda (txn)
                        (personal-favor (find-user (source-id txn))
                                        user from to))
                      (remove-duplicates
                       (select-dao 'transaction (:and (:> 'timestamp from)
                                                      (:> to 'timestamp)
                                                      (:= (user-id user) 'target-id)))
                       :key #'source-id))))

;;; Relative Favor
(defun get-all-relevant-users (user target from to personal-favor-qualifier)
  (loop for friend-id in (query (:select 'target-id :from 'transaction
                                         :where (:and (:= (user-id user) 'source-id)
                                                      (:>= to 'timestamp)
                                                      (:<= from 'timestamp)))
                                :column)
     with friends = nil
     when (and (funcall personal-favor-qualifier
                        (personal-favor (user-id user) friend-id from to))
               (query (:select 'target-id :from 'transaction
                               :where (:and (:= friend-id 'source-id)
                                            (:= (user-id target) 'target-id)
                                            (:>= to 'timestamp)
                                            (:<= from 'timestamp))) :single))
     do (pushnew friend-id friends)
     finally (return (mapcar #'find-user friends))))

(defun relevant-favor (observer specimen from to personal-favor-qualifier)
  (let ((observer-friends (get-all-relevant-users observer specimen from to personal-favor-qualifier)))
    (reduce #'+
            (mapcar
             (lambda (friend)
               (personal-favor friend specimen from to))
             observer-friends))))

(defmethod friend-favor ((observer user) (specimen user) from to)
  (relevant-favor observer specimen from to #'plusp))

(defmethod enemy-favor ((observer user) (specimen user) from to)
  (relevant-favor observer specimen from to #'minusp))

(defun 4-arg-global-favor (source target from to)
  (declare (ignore source))
  (global-favor target from to))

(defun favor-by-type (type observer specimen from to)
  (let* ((type->function '(("personal" . personal-favor)
                           ("friend" . friend-favor)
                           ("enemy" . enemy-favor)
                           ("global" . 4-arg-global-favor)))
         (function (cdr (assoc type type->function :test #'string-equal))))
    (if function
        (funcall function (find-user observer) (find-user specimen) from to)
        0)))
