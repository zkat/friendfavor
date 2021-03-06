(asdf:defsystem friendfavor
  :version "0"
  :description "Merit-based economic system."
  :maintainer "Kat Marchán <zkat@Dagon>"
  :author "Kat Marchán <zkat@Dagon>"
  :licence "AGPLv3"
  :depends-on (alexandria postmodern yaclml hunchentoot s-dot yason ironclad parenscript)
  :serial t
  ;; components likely need manual reordering
  :components 
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             (:file "models")
             (:file "favor")
             (:file "handlers")
             (:file "server")))))

#+nil(asdf:defsystem meritocracy-tests
  :depends-on (eos)
  :serial t
  :components ((:file "meritocracy-tests")))
