(in-package :ffavor)

(defvar *title*)
;; web pages go here
(defmacro with-template (title &body body)
  `(yaclml:with-yaclml-output-to-string
     (<:html
      (<:head
       (<:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
       (<:title ,title)
       (<:link :rel "stylesheet" :type "text/css" :href "styles.css"))
      (<:body ,@body))))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (with-template "Home, Sweet Home"
    (<:h1 "Hello, world!")))
