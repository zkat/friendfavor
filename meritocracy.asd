(asdf:defsystem meritocracy
  :version "0"
  :description "Merit-based economic system."
  :maintainer "Kat Marchán <zkat@Dagon>"
  :author "Kat Marchán <zkat@Dagon>"
  :licence "Public-domain"
  :depends-on (s-dot)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "meritocracy"))
  ;; :long-description ""
  )

(asdf:defsystem meritocracy-tests
  :depends-on (eos)
  :serial t
  :components ((:file "meritocracy-tests")))
