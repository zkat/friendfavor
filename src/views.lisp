(in-package :ffavor)

(defvar *title*)
;; web pages go here
(defmacro with-template (title &body body)
  `(yaclml:with-yaclml-output-to-string
     (<:html
      (<:head
       (<:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
       (<:title (<:ah ,title))
       (<:link :rel "stylesheet" :type "text/css" :href "styles.css"))
      (<:body
       (<:div :id "links" :class "menu"
              (<:ul
               (loop for (url name) in '(("/"  "Home")
                                         ("/login" "Log In")
                                         ("/favors" "Check Favors"))
                  do (<:li (<:a :href url (<:ah name))))))
       (<:div :id "content" :class "content"
              ,@body)))))

(defmacro defhandler (description title lambda-list &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     (with-template ,title
       ,@body)))

(defhandler (home :uri "/") "Home, Sweet Home" ()
  (if (and hunchentoot:*session* (hunchentoot:session-value 'username))
      (<:h1 (<:ah (format nil "Hello, ~A!" (hunchentoot:session-value 'username))))
      (progn
        (hunchentoot:start-session)
        (<:h1 "Welcome!")
        (<:p "Please log in.")
        (<:form :action "/login" :method "post"
                (<:input :type "text" :name "username")
                (<:input :type "text" :name "password")
                (<:input :type "submit" :value "Log In")))))

(defhandler (login :uri "/login") "Log In" (username password)
  (when hunchentoot:*session*
    (when (and username password)
      (setf (hunchentoot:session-value 'username) username)
      (<:p (<:ah (format nil "Tried to log on as '~A', using '~A' as your password." username password))))))

(defhandler (favors :uri "/favors") "Check your favors" ()
  (<:h1 "Various favors shall appear here!")
  (when hunchentoot:*session*
    (when (hunchentoot:session-value 'username)
      (<:p (<:ah (format nil "Welcome, ~A!" (hunchentoot:session-value 'username))))
      (let ((user (find-user (hunchentoot:session-value 'username))))
        (when user
          (<:p (<:ah (format nil "Current global favor: ~A"
                             (coerce (global-favor user
                                                   (query (:select (:min 'timestamp) :from 'transaction) :single)
                                                   (query (:select (:max 'timestamp) :from 'transaction) :single))
                                     'float)))))))))
