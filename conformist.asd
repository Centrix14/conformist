(defsystem "conformist"
  :description "conformist is a pattern matching library"
  :version "0.1"
  :author "Centrix14"
  :license "GNU GPL v3"
  :depends-on (:asdf)
  :components ((:file "conformist")
               (:file "placeholders" :depends-on ("conformist"))
               (:file "matching" :depends-on ("conformist"))
               ))

(defsystem "conformist/examples"
  :description "Examples for conformist library"
  :depends-on (:asdf :conformist)
  :components ((:file "examples")))
