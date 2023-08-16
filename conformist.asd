(defsystem "conformist"
  :description "conformist is a pattern matching library"
  :version "0.1"
  :author "Centrix14"
  :license "GNU GPL v3"
  :depends-on (:asdf)
  :components ((:file "src/pattern-system")
               (:file "src/util")
               (:file "src/shipper" :depends-on ("src/pattern-system"
                                                 "src/util"))
               (:file "src/std-collection" :depends-on ("src/util"))))
