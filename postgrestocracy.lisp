(defpackage #:postgrestocracy
  (:use :cl :postmodern)
  (:nicknames :pgtocracy))

(in-package :pgtocracy)

(declaim (optimize (debug 3)))
;;;
;;; Users
;;;
(defclass user ()
  ((user-id :col-type bigserial :accessor user-id)
   (username :initarg :username
         :initform (error "User must be initialized with a username.")
         :col-type string
         :accessor username))
  (:metaclass dao-class)
  (:keys user-id))

(defmethod print-object ((obj user) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A (~A)" (username obj) (user-id obj))))

(defun find-user-by-id (user-id)
  (car (select-dao 'user (:= user-id 'user-id))))

(defun find-user-by-username (username)
  (car (select-dao 'user (:= username 'username))))

(defun user= (user1 user2)
  (= (user-id user1) (user-id user2)))

(defgeneric find-user (identifier)
  (:method ((id integer))
    (find-user-by-id id))
  (:method ((id string))
    (find-user-by-username id)))

;;;
;;; Transactions
;;;
(defclass transaction ()
  ((transaction-id :col-type bigserial :accessor transaction-id)
   (favorp :initarg :favorp :initform t
           :col-type boolean
           :accessor favorp)
   (source-id :initform (error "Transaction must be given a source user.")
              :initarg :source-id
              :col-type integer
              :accessor source-id)
   (target-id :initform (error "Transaction must be given a target user.")
              :initarg :target-id
              :col-type integer
              :accessor target-id)
   (time :initform (get-universal-time)
         :initarg :time
         :col-type bigint)
   (description :initarg :description
                :col-type string
                :col-default "No description"))
  (:metaclass dao-class)
  (:keys transaction-id source-id target-id))

(defmethod print-object ((obj transaction) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A (~A -> ~A)" (if (favorp obj)
                                       "Favor" "Disfavor")
            (username (find-user (source-id obj))) (username (find-user (target-id obj))))))

(defun disfavorp (transaction)
  (not (favorp transaction)))

;;; Granting favor/disfavor
(defgeneric @favor (actor target &optional description)
  (:documentation "Registers one favor from ACTOR to TARGET. Accepts an optional description of why
it was granted.")
  (:method ((actor user) (target user) &optional (description "No description."))
    (if (user= actor target)
        (error "No, you can't @favor yourself!")
        (insert-dao (make-instance 'transaction
                                   :description description
                                   :source-id (user-id actor)
                                   :target-id (user-id target))))))

(defgeneric @disfavor (actor target &optional description)
  (:documentation "Registers one disfavor from ACTOR to TARGET. Accepts an optional description of
why it was granted.")
  (:method ((actor user) (target user) &optional (description "No description."))
    (if (user= actor target)
        (error "No, you can't @disfavor yourself!")
        (insert-dao (make-instance 'transaction
                                   :favorp nil
                                   :description description
                                   :source-id (user-id actor)
                                   :target-id (user-id target))))))

;;;
;;; Importing
;;;
(defun ensure-user (username)
  (or (find-user-by-username username)
      (insert-dao (make-instance 'user :username username))))

(defun unix-time->universal-time (unix-time)
  (+ unix-time 2208988800))

(defun import-transaction-from-hash (hash)
  (let ((type (gethash "type" hash))
        (source (gethash "source" hash))
        (target (gethash "target" hash))
        (timestamp (gethash "date" hash)))
    (insert-dao (make-instance 'transaction
                               :favorp (equalp type "positive")
                               :source-id (user-id (ensure-user source))
                               :target-id (user-id (ensure-user target))
                               :time (unix-time->universal-time timestamp)))))

;;;
;;; Metrics
;;;

;;; Generics
(defgeneric global-favor (user from-date to-date)
  (:documentation "Returns the global favor accumulated by user between FROM and TO."))

(defgeneric friend-favor (observer specimen from to)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. friend favor is a measurement of what those that you have positive opinions of, and their
positive connections, think of SPECIMEN."))

(defgeneric enemy-favor (observer specimen from to)
  (:documentation "Retuns the enemy favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. enemy favor is a measurement of what those that you have negative opinions of, and their
positive connections, think of SPECIMEN."))

;;; Implementations
(defparameter *distance-decay-factor* 1)
(defparameter *repeated-favor-decay* 4/5)

(defun path-favor (path from to transaction-decay)
  "Given a path, calculates its total value. TRANSACTION-DECAY measures how quickly repeated
favor/disfavors decay in value."
  (if (null path)
      (error "Invalid path ~S" path)
      ;; We only care about the actual opinion of the second-to-last node in the path.
      (let ((judge (car (last (butlast path))))
            (target (car (last path))))
        (let ((unweighted (personal-favor judge target from to transaction-decay))
              (weight (* *distance-decay-factor* (1- (length path)))))
          (/ unweighted weight)))))

(defun transaction-count (judge target from to favorp)
  (query (:select (:count :*) :from 'transaction
                  :where (:and (:> 'time from)
                               (:> to 'time)
                               (:= 'target-id (user-id target))
                               (:= 'source-id (user-id judge))
                               (:= 'favorp favorp)))
         :single))

(defun favor-count (judge target from to)
  (transaction-count judge target from to t))

(defun disfavor-count (judge target from to)
  (transaction-count judge target from to nil))

(defun positive-favor-p (judge target from to)
  (plusp (- (favor-count judge target from to)
            (disfavor-count judge target from to))))

(defun geometric-sum (first-term common-ratio num-terms)
  "sum of a + ar + ar^2 + ... + ar^(n-1)"
  (/ (* first-term (- 1 (expt common-ratio num-terms)))
     (- 1 common-ratio)))

(defun personal-favor (judge target from to &optional (decay-factor *repeated-favor-decay*))
  "Calculates the personal favor from JUDGE to TARGET. DECAY-FACTOR represents how quickly repeated
favor/disfavors might decay in value. When DECAY-FACTOR < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (let ((favor-value (geometric-sum 1 decay-factor
                                    (favor-count judge target from to)))
        (disfavor-value (geometric-sum 1 decay-factor
                                       (disfavor-count judge target from to))))
    (- favor-value disfavor-value)))

(defmethod global-favor ((user user) from to)
  (reduce #'+ (mapcar (lambda (txn)
                        (personal-favor (find-user (source-id txn))
                                        user from to))
                      (remove-duplicates
                       (select-dao 'transaction (:and (:> 'time from)
                                                      (:> to 'time)
                                                      (:= (user-id user) 'target-id)))
                       :key #'source-id))))

;;; Path-finding
(defun node-neighbors (node-id neighbor-func exclusion-list)
  "Returns a list of neighbors of NODE. A user is a neighbor iff there is a transaction from node to
that user, and the transaction passes INCLUSION-FUNCTION. INCLUSION-FUNCTION should be a two-argument
function that accepts a user (the current node we're getting neighbors for), and a transaction object,
and returns a generalized boolean that answers whether that transaction's target should be in the
list of NODE's neighbors."
  (remove-if (lambda (user)
               (member (user-id user) exclusion-list))
             (funcall neighbor-func node-id)))

(defun dijkstra (graph source-id neighbor-func exclusion-list)
  "Mostly standard implementation of Dijkstra's algorithm. Returns a hash table of distances and
a hash table with shortest paths. INCLUSION-FUNC is used by #'NODE-NEIGHBORS."
  (let ((distances (make-hash-table))
        (previous (make-hash-table)))

    (loop for node in graph do
         (setf (gethash node distances) nil))

    ;; Distance from source to source
    (setf (gethash source-id distances) 0)

    (flet ((smallest-distance ()
             (let (smallest smallest-value)
               (maphash (lambda (k v)
                          (when (find k graph)
                            (cond ((and (numberp v)
                                        (numberp smallest-value)
                                        (> smallest-value v))
                                   (setf smallest k smallest-value v))
                                  ((and (numberp v)
                                        (null smallest-value))
                                   (setf smallest k smallest-value v))
                                  ((null smallest)
                                   (setf smallest k smallest-value v))
                                  (t nil))))
                        distances)
               smallest)))
      (loop while graph
         for node = (smallest-distance)
         do (setf graph (remove node graph))
         when (gethash node distances)
         do (loop for neighbor in (mapcar #'user-id (node-neighbors node neighbor-func exclusion-list))
               do (let ((node-distance (gethash node distances))
                        (neighbor-distance (gethash neighbor distances)))
                    (when (or (not (or node-distance neighbor-distance))
                              (and node-distance (null neighbor-distance))
                              (< (1+ node-distance)
                                 neighbor-distance))
                      (setf (gethash neighbor distances) node-distance
                            (gethash neighbor previous) node)))))
      (values distances previous))))

(defun shortest-indirect-path (source target neighbor-func exclusion-list)
  "Finds the shortest *INDIRECT* path between SOURCE and TARGET. That is, SOURCE->TARGET is not
considered a valid path."
  (multiple-value-bind (distances previous)
      (dijkstra (query (:select 'user-id :from 'user) :column) (user-id source) neighbor-func exclusion-list)
    (declare (ignore distances))
    (loop with list = nil
       with u = (user-id target)
       while (gethash u previous)
       do (push u list)
         (setf u (gethash u previous))
       finally (return list))))

(defun all-indirect-paths (source target neighbor-func)
  "Finds all indirect shortest paths between SOURCE and TARGET."
  (loop with exclusion-list = nil
     for shortest = (shortest-indirect-path source target
                                            neighbor-func exclusion-list)
     while shortest
     do (pushnew (car (last (butlast shortest))) exclusion-list)
       #+nil(format t "Exclusion list: ~A~%" exclusion-list)
     collect shortest))

(defun relative-favor (observer specimen from to neighbor-finder)
  (reduce #'+ (mapcar (lambda (path) (path-favor (mapcar #'find-user path) from to *repeated-favor-decay*))
                      (all-indirect-paths observer specimen neighbor-finder))))

(defun transactions-from (node)
  (select-dao 'transaction (:= 'source-id (user-id node))))

(defun friend-find-node-func (observer specimen from to)
  (lambda (node-id)
    (mapcar (lambda (txn)
              (find-user 
               (target-id txn)))
            (remove-duplicates
             (remove-if-not (lambda (txn)
                              (if (eq (user-id specimen)
                                      (target-id txn))
                                  t
                                  (plusp (personal-favor (find-user (source-id txn))
                                                         (find-user (target-id txn))
                                                         from to))))
                            (select-dao 'transaction
                                        (:and (:= 'source-id node-id)
                                              (:>= to 'time)
                                              (:<= from 'time)
                                              (:not (:and
                                                     (:= (user-id observer) 'source-id)
                                                     (:= (user-id specimen) 'target-id))))))
             :key #'target-id
             :test #'=)))
  
  #+nil(lambda (node txn)
         (and (eq node (slot-value txn 'source))
              (if (eq specimen (slot-value txn 'target))
                  t
                  (plusp (personal-favor node (slot-value txn 'target) *repeated-favor-decay*)))
              (not (and (eq observer (slot-value txn 'source))
                        (eq specimen (slot-value txn 'target)))))))


(defmethod friend-favor ((observer user) (specimen user) from to)
  (relative-favor observer specimen
                  from to
                  (friend-find-node-func observer specimen from to)))

;; (defmethod enemy-favor ((observer pc) (specimen pc) (from time) (to time))
;;   (relative-favor observer specimen from to
;;                   (lambda (node txn)
;;                     (if (eq observer (slot-value txn 'source))
;;                         (unless (eq specimen (slot-value txn 'target))
;;                           (minusp (personal-favor node (slot-value txn 'target) *repeated-favor-decay*)))
;;                         (and (eq node (slot-value txn 'source))
;;                              (if (eq specimen (slot-value txn 'target))
;;                                  t
;;                                  (plusp (personal-favor node (slot-value txn 'target) *repeated-favor-decay*)))
;;                              (not (and (eq observer (slot-value txn 'source))
;;                                        (eq specimen (slot-value txn 'target)))))))))


(defvar *min-time*)
(defvar *max-time*)

(defun test-setup-time ()
  (setf *min-time* (query (:select (:min 'time) :from 'transaction) :single)
        *max-time* (query (:select (:max 'time) :from 'transaction) :single))
  t)

#+nil(defun generate-full-graph (favor-func lowball highball)
  `(s-dot::graph ((s-dot::ratio "auto") (s-dot::ranksep "0.1") (s-dot::nodesep "0.1"))
                 ,@(loop for node in (select-dao 'user)
                      collect `(s-dot::node
                                ((s-dot::id ,(username node))
                                 (s-dot::label ,(format nil "~A - WG: ~A" 
                                                        (username node)
                                                        (coerce (global-favor node
                                                                              *min-time*
                                                                              *max-time*)
                                                                'float))))))
                 ,@(loop with edges = nil
                      for source in (select-dao 'user)
                      do (loop for target in (select-dao 'user)
                            unless (= (user-id source) (user-id target))
                            do (let ((favor (coerce (funcall favor-func source target *min-time* *max-time*) 'float)))
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