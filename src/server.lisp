(in-package :ffavor)

(defun start-server (db-name port)
  (connect-toplevel db-name "zkat" "" "localhost")
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port port)))
