(defsystem "conformist"
  :description "conformist is a pattern matching library"
  :version "0.1"
  :author "Centrix14"
  :license "GNU GPL v3"
  :depends-on (:asdf)
  :components ((:file "conformist")
               (:file "src/util")
               (:file "src/std-collection" :depends-on ("src/util"))
               (:file "src/pattern-system" :depends-on ("src/std-collection"))
               (:file "src/compositor" :depends-on ("src/pattern-system"))
               (:file "src/shipper" :depends-on ("src/compositor"))))
