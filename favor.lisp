(defpackage #:favor
  (:use :cl))
(in-package :favor)

(defvar *all-pcs* '())

(defstruct (time (:constructor %make-time
                               (universal-time second minute
                                hour date month year day daylight-p zone)))
  universal-time second minute hour date month year day daylight-p zone)

(defun make-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (%make-time universal-time second minute hour date month year day daylight-p zone)))

(defclass pc ()
  ((name :initarg :name
         :initform (error "PC must be initialized with a name."))
   (transactions :initform nil)))
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

(defclass transaction ()
  ((time :initform (make-time)
         :initarg :time)
   (description :initform "No description"
                :initarg :description)
   (source :initform (error "Transaction must be given a source PC.")
           :initarg :source)
   (target :initform (error "Transaction must be given a target PC.")
           :initarg :target)))

(defclass @favor (transaction) ())

(defclass @disfavor (transaction) ())

(defgeneric global-favor (pc from-date to-date)
  (:documentation "Returns the global favor accumulated by PC between FROM-DATE and TO-DATE."))

(defgeneric relative-favor (specimen observer from-date to-date)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER,
 between FROM-DATE and TO-DATE."))

(defgeneric @favor (actor target &optional description)
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @favor yourself!")
        (push (make-instance '@favor
                             :description description
                             :source actor
                             :target target)
              (slot-value actor 'transactions)))))

(defgeneric @disfavor (actor target &optional description)
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @disfavor yourself!")
        (push (make-instance '@disfavor
                             :description description
                             :source actor
                             :target target)
              (slot-value actor 'transactions)))))

(defgeneric node-neighbors (node)
  (:method ((node pc))
    (mapcar (lambda (transaction)
              (slot-value transaction 'target))
            (remove-duplicates (slot-value node 'transactions)
                               :key (lambda (txn) (slot-value txn 'target))))))

(defun relevant-time-p (time upper-bound lower-bound)
  (apply #'> (mapcar #'time-universal-time (list upper-bound time lower-bound))))

(defun shortest-path (graph source target)
  ;; todo
  )

(defun all-paths (graph source target)
  (loop for shortest = (shortest-path graph source target)
     while shortest
     do (setf graph (remove (car (last (butlast shortest))) graph))
     collect shortest))

(defun path-favor (path transaction-decay)
  (if (or (null path) (= 1 (length path)))
      (error "Invalid path ~S" path)
      (let ((judge (car (last (butlast path))))
            (target (car (last path))))
        (let ((unweighted (direct-favor judge target transaction-decay))
              (weight (1- (length path))))
          (/ unweighted weight)))))

(defun direct-favor (judge target decay-factor)
  (flet ((relevant-transaction-p (transaction)
           (when (eq (slot-value transaction 'target) target)
             t))
         (transaction-value (transaction)
           (ecase (class-name (class-of transaction))
             (@favor 1)
             (@disfavor -1)))
	 (geometric-sum (a r n) "sum of a + ar + ar^2 + ... + ar^(n-1)"
	   (/ (* a (- 1 (expt r n))) (- 1 r))))
    (let* ((relevant-transactions (remove-if-not #'relevant-transaction-p
                                                 (slot-value judge 'transactions)))
           (favors (remove-if-not (lambda (txn) (eq '@favor (class-name (class-of txn))))
                                  relevant-transactions))
           (disfavors (remove-if-not (lambda (txn) (eq '@disfavor (class-name (class-of txn))))
                                     relevant-transactions))
	   (favor-value (geometric-sum 1 decay-factor (length favors)))
	   (disfavor-value (geometric-sum 1 decay-factor (length disfavors))))
      (- favor-value disfavor-value))))

(defmethod global-favor ((pc pc) (from-date time) (to-date time))
  ;; todo
  )

(defmethod relative-favor ((specimen pc) (observer pc) (from time) (to time))
  ;; todo
  )

(defun test-init ()
  (flet ((make-pc (name)
           (make-instance 'pc :name name))
         (favor (name1 name2)
           (@favor (find-pc name1) (find-pc name2)))
         (disfavor (name1 name2)
           (@disfavor (find-pc name1) (find-pc name2))))
    (mapcar #'make-pc '("Opal" "Taraji" "Veritas" "Flynn"))
    (favor "Opal" "Taraji")
    (favor "Opal" "Taraji")
    (favor "Taraji" "Veritas")
    (favor "Taraji" "Veritas")
    (favor "Taraji" "Veritas")
    (disfavor "Veritas" "Flynn")
    (disfavor "Veritas" "Flynn")
    (disfavor "Opal" "Flynn")))
