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
    (values (* 10 (+ 5 (- favor-value disfavor-value))) 1)))

(defmethod personal-favor ((judge user) (target user) from to)
  "Calculates the personal favor from JUDGE to TARGET. *REPEATED-FAVOR-DECAY* represents how quickly repeated
favor/disfavors might decay in value. When *REPEATED-FAVOR-DECAY* < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (personal-favor (user-id judge) (user-id target) from to))

;;; Global Favor
(defmethod global-favor ((user user) from to)
  (let* ((txns (remove-duplicates
                (select-dao 'transaction (:and (:> 'timestamp from)
                                               (:> to 'timestamp)
                                               (:= (user-id user) 'target-id)))
                :key #'source-id))
         (n-txns (length txns)))
    (if (zerop n-txns)
        (values 50 0)
        (values (/ (reduce #'+ (mapcar (lambda (txn)
                                         (personal-favor (find-user (source-id txn))
                                                         user from to))
                                       txns))
                   n-txns)
                n-txns))))

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
  (let* ((observer-friends (get-all-relevant-users observer specimen from to personal-favor-qualifier))
         (n-observers (length observer-friends)))
    (if (zerop n-observers)
        (values 50 0)
        (values (/ (reduce #'+
                           (mapcar
                            (lambda (friend)
                              (personal-favor friend specimen from to))
                            observer-friends))
                   n-observers)
                n-observers))))

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
        (values 0))))

(defun humane-favor-by-type (type source target from to)
  (multiple-value-bind (favor n-judges)
      (favor-by-type type source target from to)
    (if (plusp n-judges)
        (favor->prose favor type target n-judges)
        (let ((preamble (cond ((string-equal type "friend")
                               "No one you like has")
                              ((string-equal type "enemy")
                               "No one you dislike has")
                              ((string-equal type "personal")
                               "You don't really have"))))
          (format nil "~A anything to say about ~A." preamble target)))))

(defun favor->prose (favor type target n-judges)
  (format nil "According to ~A, ~A's reputation is ~A."
          (cond ((string-equal type "friend")
                 (format nil "~A ~A you like" n-judges (if (> n-judges 1) "people" "person")))
                ((string-equal type "enemy")
                 (format nil "~A ~A you dislike" n-judges (if (> n-judges 1) "people" "person")))
                ((string-equal type "personal")
                 "your personal opinion"))
          target (favor-percentage->rating favor)))

(defun favor-percentage->rating (amount)
  (cond ((<= 0 amount 4)
         "dysmal")
        ((<= 5 amount 14)
         "terrible")
        ((<= 15 amount 24)
         "bad")
        ((<= 25 amount 34)
         "poor")
        ((<= 35 amount 44)
         "lackluster")
        ((<= 45 amount 55)
         "neutral")
        ((<= 56 amount 65)
         "lukewarm")
        ((<= 66 amount 75)
         "positive")
        ((<= 76 amount 85)
         "very good")
        ((<= 86 amount 95)
         "excellent")
        ((<= 96 amount 100)
         "astonishingly good")))

;;;
;;; Exporting
;;;
(defun generate-full-graph (favor-func lowball highball from to)
  `(s-dot::graph ((s-dot::ratio "auto") (s-dot::ranksep "0.1") (s-dot::nodesep "0.1"))
                 ,@(loop for node in (select-dao 'user)
                      collect `(s-dot::node
                                ((s-dot::id ,(username node))
                                 (s-dot::label ,(format nil "~A - WG: ~A"
                                                        (username node)
                                                        (coerce (global-favor node
                                                                              from
                                                                              to)
                                                                'float))))))
                 ,@(loop with edges = nil
                      for source in (select-dao 'user)
                      do (loop for target in (select-dao 'user)
                            unless (= (user-id source) (user-id target))
                            do (let ((favor (coerce (funcall favor-func source target from to) 'float)))
                                 (unless (< lowball favor highball)
                                   (push `(s-dot::edge ((s-dot::from ,(username source))
                                                        (s-dot::to ,(username target))
                                                        (s-dot::label ,(princ-to-string favor))
                                                        (s-dot::color ,(if (plusp favor)
                                                                           "#348017" ;; green
                                                                           "#C11B17" ;; red
                                                                           ))))
                                         edges))))
                      finally (return edges))))

(defun generate-user-graph (source-user favor-func from to)
  (let ((relevant-user-alist (loop for target in (select-dao 'user)
                                for favor-amount = (funcall favor-func source-user target from to)
                                unless (zerop favor-amount)
                                collect (cons target (coerce favor-amount 'float)))))
    `(s-dot::graph ((s-dot::ratio "auto") (s-dot::ranksep "0.1") (s-dot::nodesep "0.1"))
                   ,@(loop for (node . ignore) in (cons (cons source-user 0.0) relevant-user-alist)
                        collect `(s-dot::node
                                  ((s-dot::id ,(username node))
                                   (s-dot::label ,(format nil "~A - WG: ~A"
                                                          (username node)
                                                          (coerce (global-favor node
                                                                                from
                                                                                to)
                                                                  'float))))))
                   ,@(loop for (node . favor-value) in relevant-user-alist
                        collect `(s-dot::edge ((s-dot::from ,(username source-user))
                                               (s-dot::to ,(username node))
                                               (s-dot::label ,(princ-to-string favor-value))
                                               (s-dot::color ,(if (plusp favor-value)
                                                                  "#348017" ;; green
                                                                  "#C11B17" ;; red
                                                                  ))))))))

(defun export-to-png (filename favor-func lowball highball from to)
  (s-dot::render-s-dot filename "png" (generate-full-graph favor-func lowball highball from to)))
