(in-package :ffavor)

;;;
;;; Users
;;;
(defclass user ()
  ((user-id :col-type bigserial :accessor user-id)
   (username :initarg :username
             :initform (error "User must be initialized with a username.")
             :col-type string
             :accessor username)
   (password :initarg :password
             :initform nil
             :col-type string
             :accessor password))
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

(defun encrypt-string (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 string)))

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
   (timestamp :initform (get-universal-time)
              :initarg :timestamp
              :col-type bigint
              :accessor timestamp)
   (reason :initarg :description
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
(defgeneric make-transaction (actor target favorp &optional reason)
  (:documentation "Registers one transaction of from ACTOR to TARGET. If FAVORP is true, then the
  transaction will be a favor. Otherwise, it will be a disfavor. Optionally, a reason may be
  provided.")
  (:method ((actor user) (target user) favorp &optional (description "No description."))
    (if (user= actor target)
        (error "The actor and target of a transaction cannot be the same.")
        (insert-dao (make-instance 'transaction
                                   :description description
                                   :favorp (if favorp t nil)
                                   :source-id (user-id actor)
                                   :target-id (user-id target))))))

(defun favor (actor target &optional reason)
  (make-transaction actor target t reason))

(defun disfavor (actor target &optional reason)
  (make-transaction actor target nil reason))

;;;
;;; Importing
;;;
(defun ensure-user (username)
  (or (find-user-by-username username)
      (insert-dao (make-instance 'user :username username))))

(defparameter +unix-time-difference+ 2208988800
  "Difference in time between zero-hour in Lisp's universal time and unix time.")

(defun import-transaction-from-hash (hash &optional (time-adjustment +unix-time-difference+))
  (let ((type (gethash "type" hash))
        (source (gethash "source" hash))
        (target (gethash "target" hash))
        (timestamp (gethash "date" hash)))
    (insert-dao (make-instance 'transaction
                               :favorp (equalp type "positive")
                               :source-id (user-id (ensure-user source))
                               :target-id (user-id (ensure-user target))
                               :timestamp (+ timestamp time-adjustment)))))

(defun import-json (filespec &optional (time-adjustment +unix-time-difference+))
  "Imports a list of JSON objects from a file into transactions, automatically creating users."
  (mapcar (rcurry 'import-transaction-from-hash time-adjustment)
          (with-open-file (s filespec)
            (json:parse s)))
  t)

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

(defun export-to-png (filename favor-func lowball highball from to)
  (s-dot::render-s-dot filename "png" (generate-full-graph favor-func lowball highball from to)))
