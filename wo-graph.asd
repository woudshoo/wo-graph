;;;; wo-graph.asd

(asdf:defsystem #:wo-graph
  :serial t
  :author "Willem Rein Oudshoorn"
  :version "0.0.2"
  :license "LGPL"
  :description "Graph Infrastructure"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "wo-graph")))

