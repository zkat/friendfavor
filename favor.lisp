(defpackage #:favor
  (:use :cl))
(in-package :favor)

(defvar *all-pcs* '())
(defvar *all-transactions* '())

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

(defgeneric global-favor (pc from-date to-date)
  (:documentation "Returns the global favor accumulated by PC between FROM-DATE and TO-DATE."))

(defgeneric right-handed-favor (observer specimen from-date to-date)
  (:documentation "Returns the relative favor for SPECIMEN as seen by OBSERVER,
 between FROM-DATE and TO-DATE."))
(defgeneric left-handed-favor (observer specimen from to))

(defgeneric @favor (actor target &optional description)
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @favor yourself!")
        (push (make-instance '@favor
                             :description description
                             :source actor
                             :target target)
              *all-transactions*))))

(defgeneric @disfavor (actor target &optional description)
  (:method ((actor pc) (target pc) &optional (description "No description."))
    (if (eq actor target)
        (error "No, you can't @disfavor yourself!")
        (push (make-instance '@disfavor
                             :description description
                             :source actor
                             :target target)
              *all-transactions*))))

(defun node-neighbors (node inclusion-function)
  (remove-duplicates
   (mapcar (lambda (txn) (slot-value txn 'target))
           (remove-if-not (lambda (txn)
                            (funcall inclusion-function node txn))
                          *all-transactions*))))

(defun relevant-time-p (time upper-bound lower-bound)
  (apply #'> (mapcar #'time-universal-time (list upper-bound time lower-bound))))

(defun dijkstra (graph source inclusion-func)
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
  (loop for shortest = (shortest-indirect-path graph source target inclusion-func)
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

(defun clamp-transactions (txn-list from to)
  (remove-if-not (lambda (txn)
                   (> (time-universal-time to)
                      (time-universal-time (slot-value txn 'time))
                      (time-universal-time from)))
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
                                        1/2))
                        (remove-duplicates relevant-transactions
                                           :key (lambda (txn)
                                                  (slot-value txn 'source)))))))

(defun relative-favor (observer specimen from to inclusion-function)
  (let ((*all-transactions* (clamp-transactions *all-transactions* from to)))
    (reduce #'+ (cons (direct-favor observer specimen 1/2)
                      (mapcar (lambda (path) (path-favor path 1/2))
                              (print (all-indirect-paths *all-pcs* observer specimen
                                                         inclusion-function)))))))

(defmethod right-handed-favor ((observer pc) (specimen pc) (from time) (to time))
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (and (eq node (slot-value txn 'source))
                         (if (eq specimen (slot-value txn 'target))
                             t
                             (plusp (direct-favor node (slot-value txn 'target) 1/2)))
                         (not (and (eq observer (slot-value txn 'source))
                                   (eq specimen (slot-value txn 'target))))))))



(defmethod left-handed-favor ((observer pc) (specimen pc) (from time) (to time))
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (if (eq observer (slot-value txn 'source))
                        (unless (eq specimen (slot-value txn 'target))
                          (minusp (direct-favor node (slot-value txn 'target) 1/2)))
                        (and (eq node (slot-value txn 'source))
                             (if (eq specimen (slot-value txn 'target))
                                 t
                                 (plusp (direct-favor node (slot-value txn 'target) 1/2)))
                             (not (and (eq observer (slot-value txn 'source))
                                       (eq specimen (slot-value txn 'target)))))))))

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
