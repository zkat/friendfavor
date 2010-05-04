(defpackage #:pgtocracy
  (:use :cl :postmodern))
(in-package :pgtocracy)

;;;
;;; Time utils
;;;
(defstruct (time (:constructor %make-time
                               (universal-time second minute
                                hour date month year day daylight-p zone)))
  universal-time second minute hour date month year day daylight-p zone)

(defun make-time (&optional (universal-time (get-universal-time)))
  (multiple-value-call #'%make-time universal-time
                       (decode-universal-time universal-time)))

;;;
;;; Users
;;;
(defclass user ()
  ((user-id :col-type bigserial :accessor user-id)
   (name :initarg :name
         :initform (error "User must be initialized with a name.")
         :accessor name))
  (:metaclass dao-class)
  (:keys user-id))

(defmethod print-object ((obj user) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'name))))

(defun find-user-by-id (user-id)
  (select-dao 'user (:= user-id 'user-id)))

(defun find-user-by-name (name)
  (select-dao 'user (:= name 'name)))

(defun user= (user1 user2)
  (= (user-id user1) (user-id user2)))

;;;
;;; Transactions
;;;
(defclass transaction ()
  ((transaction-id :col-type bigserial :accessor transaction-id)
   (type-id :initarg :type-id :initform (error "Must provide a transaction type.")
            :col-type integer
            :accessor type-id)
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
         :col-type integer)
   (description :initarg :description
                :col-type string
                :col-default "No description"))
  (:metaclass dao-class)
  (:keys transaction-id source-id target-id))

(defmethod print-object ((obj transaction) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((from (slot-value (slot-value obj 'source)'name))
          (to  (slot-value (slot-value obj 'target)'name)))
      (format stream "From: ~A, To: ~A" from to))))

(defparameter *favor-id* 0)
(defparameter *disfavor-id* 1)

;; Todo - these are dumb
(defun @favorp (transaction)
  (= *favor-id* (type-id transaction)))

(defun @disfavorp (transaction)
  (= *disfavor-id* (type-id transaction)))

;;; Granting favor/disfavor
(defgeneric @favor (actor target &optional description)
  (:documentation "Registers one favor from ACTOR to TARGET. Accepts an optional description of why
it was granted.")
  (:method ((actor user) (target user) &optional (description "No description."))
    (if (user= actor target)
        (error "No, you can't @favor yourself!")
        (insert-dao (make-instance 'transaction
                                   :type-id *favor-id*
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
                                   :type-id *disfavor-id*
                                   :description description
                                   :source-id (user-id actor)
                                   :target-id (user-id target))))))

;;;
;;; Path-finding
;;;
(defun node-neighbors (node inclusion-function)
  "Returns a list of neighbors of NODE. A user is a neighbor iff there is a transaction from node to
that user, and the transaction passes INCLUSION-FUNCTION. INCLUSION-FUNCTION should be a two-argument
function that accepts a user (the current node we're getting neighbors for), and a transaction object,
and returns a generalized boolean that answers whether that transaction's target should be in the
list of NODE's neighbors."
  ;; TODO - SQL-ify this.
  (remove-duplicates
   (mapcar #'target-id
           (remove-if-not (lambda (txn)
                            (funcall inclusion-function node txn))
                          (transactions-from node)))
   :key #'transaction-id
   :test #'=))

(defun transactions-from (node)
  (select-dao 'transaction (:= 'source-id (user-id node))))

(defun dijkstra (graph source inclusion-func)
  "Mostly standard implementation of Dijkstra's algorithm. Returns a hash table of distances and
a hash table with shortest paths. INCLUSION-FUNC is used by #'NODE-NEIGHBORS."
  (let ((distances (make-hash-table))
        (previous (make-hash-table)))

    (loop for node in graph do
         (setf (gethash node distances) nil))

    ;; Distance from source to source
    (setf (gethash source distances) 0)

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
         do (loop for neighbor in (node-neighbors node inclusion-func)
               do (let ((node-distance (gethash node distances))
                        (neighbor-distance (gethash neighbor distances)))
                    (when (or (not (or node-distance neighbor-distance))
                              (and node-distance (null neighbor-distance))
                              (< (1+ node-distance)
                                 neighbor-distance))
                      (setf (gethash neighbor distances) node-distance
                            (gethash neighbor previous) node)))))
      (values distances previous))))

(defun shortest-indirect-path (graph source target inclusion-func)
  "Finds the shortest *INDIRECT* path between SOURCE and TARGET. That is, SOURCE->TARGET is not
considered a valid path."
  (multiple-value-bind (distances previous)
      (dijkstra graph source inclusion-func)
    (declare (ignore distances))
    (loop with list = nil
       with u = target
       while (gethash u previous)
       do (push u list)
         (setf u (gethash u previous))
       finally (return (when list (cons source list))))))

(defun all-indirect-paths (graph source target inclusion-func)
  "Finds all indirect shortest paths between SOURCE and TARGET."
  (loop for shortest = (shortest-indirect-path graph source target inclusion-func)
     while shortest
     do (setf graph (remove (car (last (butlast shortest))) graph))
     collect shortest))

;;;
;;; Metrics
;;;

;;; Generics
(defgeneric global-favor (pc from-date to-date)
  (:documentation "Returns the global favor accumulated by user between FROM and TO."))

(defgeneric right-handed-favor (observer specimen from to)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. Right-handed favor is a measurement of what those that you have positive opinions of, and their
positive connections, think of SPECIMEN."))

(defgeneric left-handed-favor (observer specimen from to)
  (:documentation "Retuns the left-handed favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. Left-handed favor is a measurement of what those that you have negative opinions of, and their
positive connections, think of SPECIMEN."))

;;; Implementations
(defparameter *distance-decay-factor* 1)
(defparameter *repeated-favor-decay* 4/5)

(defun path-favor (path transaction-decay)
  "Given a path, calculates its total value. TRANSACTION-DECAY measures how quickly repeated
favor/disfavors decay in value."
  (if (or (null path) (= 1 (length path)))
      (error "Invalid path ~S" path)
      ;; We only care about the actual opinion of the second-to-last node in the path.
      (let ((judge (car (last (butlast path))))
            (target (car (last path))))
        (let ((unweighted (direct-favor judge target transaction-decay))
              (weight (* *distance-decay-factor* (1- (length path)))))
          (/ unweighted weight)))))

(defun direct-favor (judge target decay-factor)
  "Calculates the direct favor from JUDGE to TARGET. DECAY-FACTOR represents how quickly repeated
favor/disfavors might decay in value. When DECAY-FACTOR < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (flet ((geometric-sum (first-term common-ratio num-terms)
           "sum of a + ar + ar^2 + ... + ar^(n-1)"
           (/ (* first-term (- 1 (expt common-ratio num-terms)))
              (- 1 common-ratio))))
    (let* ((favor-count (query (:select (:count :*) :from 'transaction
                                        :where (:and (:= 'target-id (user-id target))
                                                     (:= 'source-id (user-id judge))
                                                     (:= 'type-id *favor-id*)))))
           (disfavors (query (:select (:count :*) :from 'transaction
                                      :where (:and (:= 'target-id (user-id target))
                                                   (:= 'source-id (user-id judge))
                                                   (:= 'type-id *disfavor-id*)))))
           (favor-value (geometric-sum 1 decay-factor favor-count))
           (disfavor-value (geometric-sum 1 decay-factor favor-count)))
      (- favor-value disfavor-value))))

(defun relevant-time-p (time upper-bound lower-bound)
  "Evaluates to true if TIME is between UPPER-BOUND and LOWER-BOUND."
  (> upper-bound time lower-bound))

(defun clamp-transactions (txn-list from to)
  "Returns a list of transactions from TXN-LIST whose time is between FROM and TO."
  (remove-if-not (lambda (txn)
                   (relevant-time-p (slot-value txn 'time) to from))
                 txn-list))

(defmethod global-favor ((pc pc) (from-date time) (to-date time))
  (let* ((*all-transactions* (clamp-transactions *all-transactions* from-date to-date))
         (relevant-transactions (remove-if-not
                                 (lambda (txn)
                                   (eq pc (slot-value txn 'target)))
                                 *all-transactions*)))
    (reduce #'+ (mapcar (lambda (txn)
                          (direct-favor (slot-value txn 'source)
                                        pc
                                        *repeated-favor-decay*))
                        (remove-duplicates relevant-transactions
                                           :key (lambda (txn)
                                                  (slot-value txn 'source)))))))

(defun relative-favor (observer specimen from to inclusion-function)
  (let ((*all-transactions* (clamp-transactions *all-transactions* from to)))
    (reduce #'+ (cons (direct-favor observer specimen *repeated-favor-decay*)
                      (mapcar (lambda (path) (path-favor path *repeated-favor-decay*))
                              (all-indirect-paths *all-pcs* observer specimen
                                                  inclusion-function))))))

(defmethod right-handed-favor ((observer pc) (specimen pc) (from time) (to time))
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (and (eq node (slot-value txn 'source))
                         (if (eq specimen (slot-value txn 'target))
                             t
                             (plusp (direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                         (not (and (eq observer (slot-value txn 'source))
                                   (eq specimen (slot-value txn 'target))))))))



(defmethod left-handed-favor ((observer pc) (specimen pc) (from time) (to time))
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (if (eq observer (slot-value txn 'source))
                        (unless (eq specimen (slot-value txn 'target))
                          (minusp (direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                        (and (eq node (slot-value txn 'source))
                             (if (eq specimen (slot-value txn 'target))
                                 t
                                 (plusp (direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                             (not (and (eq observer (slot-value txn 'source))
                                       (eq specimen (slot-value txn 'target)))))))))

