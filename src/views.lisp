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
  `(define-easy-handler ,description ,lambda-list
     (with-template ,title
       ,@body)))

(defhandler (home :uri "/") "Home, Sweet Home" ()
  (if (and *session* (session-value 'username))
      (<:h1 (<:ah (format nil "Hello, ~A!" (session-value 'username))))
      (progn
        (<:h1 "Welcome!")
        (<:a :href "/login" (<:ah "Log in.")))))

(defhandler (login :uri "/login") "Log In" (username password)
  (if (and username password)
      (progn (start-session)
             (setf (session-value 'username) username)
             (<:p
              (<:ah (format nil "Logged on as '~A', using '~A' as your password."
                            username password)))
             (<:p "You can return to the " (<:a :href "/" "home page") "."))
      (progn (<:form :action "/login" :method "post" :name "loginForm"
                     (<:input :type "text" :name "username" :value "Username"
                              :onclick (ps:ps (setf (ps:@ this value) ""))
                              :onblur (ps:ps (unless (> (length (ps:@ this value)) 0)
                                               (setf (ps:@ this value) "Username"))))
                     (<:input :type "text" :name "password" :value "Password"
                              :onclick (ps:ps (setf (ps:@ this value) ""))
                              :onblur (ps:ps (unless (> (length (ps:@ this value)) 0)
                                               (setf (ps:@ this value) "Password"))))
                     (<:input :type "submit" :value "Log In")))))

(defhandler (logout :uri "/logout") "Log Out" ()
  (when *session*
    (remove-session *session*))
  (redirect "/"))

(defhandler (favors :uri "/favors") "Check your favors" ()
  (<:h1 "Various favors shall appear here!")
  (when *session*
    (when (session-value 'username)
      (<:p (<:ah (format nil "Welcome, ~A!" (session-value 'username))))
      (let ((user (find-user (session-value 'username))))
        (when user
          (<:p (<:ah (format nil "Current global favor: ~A"
                             (coerce (global-favor user
                                                   (query (:select (:min 'timestamp) :from 'transaction) :single)
                                                   (query (:select (:max 'timestamp) :from 'transaction) :single))
                                     'float)))))))))

(defhandler (check-favor :uri "/check-favor") "Favor checking" (favor-type target)
  (<:h1 (<:ai (format nil "~A favor for ~A: ~A" favor-type target
                      (coerce (favor-by-type favor-type (hunchentoot:session-value 'username) target
                                             (query (:select (:min 'timestamp) :from 'transaction) :single)
                                             (query (:select (:max 'timestamp) :from 'transaction) :single))
                              'float)))))
