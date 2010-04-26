(defpackage #:meritocracy
  (:use :cl))
(in-package :meritocracy)

;;;
;;; Time utils
;;;
(defstruct (time (:constructor %make-time
                               (universal-time second minute
                                hour date month year day daylight-p zone)))
  universal-time second minute hour date month year day daylight-p zone)

(defun make-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (%make-time universal-time second minute hour date month year day daylight-p zone)))

;;;
;;; Players
;;;
(defvar *all-pcs* '()
  "A list of all PCs (player characters) in the system.")

(defclass pc ()
  ((name :initarg :name
         :initform (error "PC must be initialized with a name."))))

(defmethod initialize-instance :after ((pc pc) &key)
  (if (find (slot-value pc 'name) *all-pcs* :key (lambda (pc) (slot-value pc 'name))
            :test #'string-equal)
      (error "PC already exists!")
      (push pc *all-pcs*)))

(defmethod print-object ((obj pc) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'name))))

(defun find-pc (name)
  (find name *all-pcs* :key (lambda (pc) (slot-value pc 'name))
        :test #'string-equal))

;;;
;;; Transactions
;;;
(defvar *all-transactions* '()
  "A list of all transactions in the system.")

(defclass transaction ()
  ((time :initform (make-time)
         :initarg :time)
   (description :initform "No description"
                :initarg :description)
   (source :initform (error "Transaction must be given a source PC.")
           :initarg :source)
   (target :initform (error "Transaction must be given a target PC.")
           :initarg :target)))

(defmethod print-object ((obj transaction) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((from (slot-value (slot-value obj 'source)'name))
          (to  (slot-value (slot-value obj 'target)'name)))
      (format stream "From: ~A, To: ~A" from to))))

(defclass @favor (transaction) ())
(defgeneric @favorp (obj)
  (:method ((obj t)) nil)
  (:method ((obj @favor)) t))

(defclass @disfavor (transaction) ())
(defgeneric @disfavorp (obj)
  (:method ((obj t)) nil)
  (:method ((obj @disfavor)) t))

;;;
;;; Path-finding
;;;
(defun node-neighbors (node inclusion-function)
  "Returns a list of neighbors of NODE. A PC is a neighbor iff there is a transaction from node to
that PC, and the transaction passes INCLUSION-FUNCTION. INCLUSION-FUNCTION should be a two-argument
function that accepts a PC (the current node we're getting neighbors for), and a transaction object,
and returns a generalized boolean that answers whether that transaction's target should be in the
list of NODE's neighbors."
  (remove-duplicates
   (mapcar (lambda (txn) (slot-value txn 'target))
           (remove-if-not (lambda (txn)
                            (funcall inclusion-function node txn))
                          *all-transactions*))))

(defun dijkstra (graph source inclusion-func)
  "Mostly standard implementation of Dijkstra's algorithm. Returns a hash table of distances and
a hash table with shortest paths. INCLUSION-FUNC is used by #'NODE-NEIGHBORS."
  (let ((distances (make-hash-table :test 'eq))
        (previous (make-hash-table :test 'eq)))

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
  (:documentation "Returns the global favor accumulated by PC between FROM and TO."))

(defgeneric right-handed-favor (observer specimen from to)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. Right-handed favor is a measurement of what those that you have positive opinions of, and their
positive connections, think of SPECIMEN."))

(defgeneric left-handed-favor (observer specimen from to)
  (:documentation "Retuns the left-handed favor for SPECIMEN as seen by OBSERVER, between FROM and
TO. Left-handed favor is a measurement of what those that you have negative opinions of, and their
positive connections, think of SPECIMEN."))

(defgeneric @favor (actor target &optional description)
  (:documentation "Registers one favor from ACTOR to TARGET. Accepts an optional description of why
it was granted.")
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @favor yourself!")
        (push (make-instance '@favor
                             :description description
                             :source actor
                             :target target)
              *all-transactions*))))

(defgeneric @disfavor (actor target &optional description)
  (:documentation "Registers one disfavor from ACTOR to TARGET. Accepts an optional description of
why it was granted.")
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @disfavor yourself!")
        (push (make-instance '@disfavor
                             :description description
                             :source actor
                             :target target)
              *all-transactions*))))

;;; Implementations
(defparameter *distance-decay-factor* 1)
(defparameter *repeated-favor-decay* 1/2)

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
  (flet ((relevant-transaction-p (transaction)
           (when (and (eq (slot-value transaction 'target) target)
                      (eq (slot-value transaction 'source) judge))
             t))
     (geometric-sum (first-term common-ratio num-terms)
       "sum of a + ar + ar^2 + ... + ar^(n-1)"
       (/ (* first-term (- 1 (expt common-ratio num-terms)))
          (- 1 common-ratio))))
    (let* ((relevant-transactions (remove-if-not #'relevant-transaction-p
                                                 *all-transactions*))
           (favors (remove-if-not #'@favorp relevant-transactions))
           (disfavors (remove-if-not #'@disfavorp relevant-transactions))
       (favor-value (geometric-sum 1 decay-factor (length favors)))
       (disfavor-value (geometric-sum 1 decay-factor (length disfavors))))
      (- favor-value disfavor-value))))

(defun relevant-time-p (time upper-bound lower-bound)
  "Evaluates to true if TIME is between UPPER-BOUND and LOWER-BOUND."
  (apply #'> (mapcar #'time-universal-time (list upper-bound time lower-bound))))

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
    ;; To include someone's direct favor, (cons (direct-favor observer specimen 1/2 (mapcar ... )))
    (reduce #'+ (mapcar (lambda (path) (path-favor path *repeated-favor-decay*))
                        (all-indirect-paths *all-pcs* observer specimen
                                            inclusion-function)))))

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

;;;
;;; Testing
;;;
(defparameter *favors*
  '(("Opal" "Taraji" "Taraji" "veritas")
    ("Taraji" "Veritas" "Veritas" "Veritas")
    ("Veritas" "Opal" "Taraji")
    ("flynn" "veritas")))

(defparameter *disfavors*
  '(("Veritas" "Flynn" "Flynn")
    ("Opal" "Flynn")))

(defun test-init ()
  (flet ((make-pc (name)
           (make-instance 'pc :name name))
         (favor (name1 name2)
           (@favor (find-pc name1) (find-pc name2)))
         (disfavor (name1 name2)
           (@disfavor (find-pc name1) (find-pc name2))))
    (setf *all-pcs* nil
          *all-transactions* nil)
    (mapcar #'make-pc '("Opal" "Taraji" "Veritas" "Flynn"))
    (loop for (char . targets) in *favors*
         do (loop for target in targets do
                 (favor char target)))
    (loop for (char . targets) in *disfavors*
         do (loop for target in targets do
                 (disfavor char target)))))

(defun test-right-handed (observer specimen)
  (coerce (right-handed-favor (find-pc observer) (find-pc specimen)
                              (make-time (- (get-universal-time) 3600)) (make-time))
          'float))
(defun test-left-handed (observer specimen)
  (coerce (left-handed-favor (find-pc observer) (find-pc specimen)
                      (make-time (- (get-universal-time) 3600)) (make-time))
          'float))
(defun test-global (specimen)
  (coerce (global-favor (find-pc specimen)
                        (make-time (- (get-universal-time) 3600)) (make-time))
          'float))
