;;;; wo-graph.asd

(asdf:defsystem #:wo-graph
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "wo-graph")))

