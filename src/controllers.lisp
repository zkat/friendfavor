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

(defun transaction-count (judge target from to favorp)
  (query (:select (:count :*) :from 'transaction
                  :where (:and (:> 'timestamp from)
                               (:> to 'timestamp)
                               (:= 'target-id (user-id target))
                               (:= 'source-id (user-id judge))
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

(defmethod personal-favor ((judge user) (target user) from to)
  "Calculates the personal favor from JUDGE to TARGET. *REPEATED-FAVOR-DECAY* represents how quickly repeated
favor/disfavors might decay in value. When *REPEATED-FAVOR-DECAY* < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (let ((favor-value (geometric-sum 1 *repeated-favor-decay*
                                    (favor-count judge target from to)))
        (disfavor-value (geometric-sum 1 *repeated-favor-decay*
                                       (disfavor-count judge target from to))))
    (- favor-value disfavor-value)))

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
     collect shortest))

(defparameter *distance-decay-factor* 1)

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
                                              (:>= to 'timestamp)
                                              (:<= from 'timestamp)
                                              (:not (:and
                                                     (:= (user-id observer) 'source-id)
                                                     (:= (user-id specimen) 'target-id))))))
             :key #'target-id
             :test #'=))))

(defun enemy-find-node-func (observer specimen from to)
  (lambda (node-id)
    (mapcar (lambda (txn)
              (find-user
               (target-id txn)))
            (remove-duplicates
             (remove-if-not (lambda (txn)
                              (if (= (user-id observer) (source-id txn))
                                  (unless (eq (user-id specimen)
                                              (target-id txn))
                                    (minusp (personal-favor (find-user (source-id txn))
                                                            (find-user (target-id txn))
                                                            from to)))
                                  (if (= (user-id specimen) (target-id txn))
                                      t
                                      (plusp (personal-favor (find-user (source-id txn))
                                                             (find-user (target-id txn))
                                                             from to)))))
                            (select-dao 'transaction
                                        (:and (:= 'source-id node-id)
                                              (:>= to 'timestamp)
                                              (:<= from 'timestamp)
                                              (:not (:and
                                                     (:= (user-id observer) 'source-id)
                                                     (:= (user-id specimen) 'target-id))))))
             :key #'target-id
             :test #'=))))

(defmethod friend-favor ((observer user) (specimen user) from to)
  (relative-favor observer specimen
                  from to
                  (friend-find-node-func observer specimen from to)))

(defmethod enemy-favor ((observer user) (specimen user) from to)
  (relative-favor observer specimen from to (enemy-find-node-func
                                             observer specimen from to)))

