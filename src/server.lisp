(in-package :ffavor)

(defun start-server (db-name port)
  (connect-toplevel db-name "zkat" "" "localhost")
  (push (create-folder-dispatcher-and-handler "/theme/" "theme/") *dispatch-table*)
  (start (make-instance 'acceptor :port port)))
