(in-package :ffavor)

(defun start-server (db-name port)
  (connect-toplevel db-name "zkat" "" "localhost")
  (start (make-instance 'acceptor :port port)))
