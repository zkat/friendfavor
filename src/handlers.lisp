(in-package :ffavor)

(defvar *title*)
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

(defhandler (favors :uri "/favor") "Check your favor" (favor-type source target)
  (when *session*
    (when (session-value 'username)
      (<:p (<:ah (format nil "Welcome, ~A!" (session-value 'username))))
      (let ((user (find-user (session-value 'username))))
        (when user
          (<:p (<:ah (format nil "Current global favor: ~1,2f%"
                             (coerce (global-favor user
                                                   (n-days-ago 60)
                                                   (now))
                                     'float)))))
        (when (and favor-type source target)
          (<:p (<:ah (humane-favor-by-type favor-type source target (n-days-ago 60) (now)))))))))

(define-easy-handler (favor->json :uri "/favor.json") (favor-type source target)
  (setf (content-type*) "application/json")
  (let ((from (n-days-ago 60))
        (to (now)))
    (multiple-value-bind (favor n-judges)
        (favor-by-type favor-type source target from to)
      (with-output-to-string (s)
        (json:encode (mkhash "source" source
                             "target" target
                             "type" favor-type
                             "from" (- from +unix-time-difference+)
                             "to" (- to +unix-time-difference+)
                             "favor" favor
                             "n-judges" n-judges)
                     s)))))
