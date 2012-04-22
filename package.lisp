;;;; package.lisp

(defpackage #:wo-graph
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table #:hash-table-keys)
  (:export 
   #:outgoing-edges
   #:incoming-edges
   #:source-vertex
   #:target-vertex
   #:sources-of-vertex
   #:targets-of-vertex
   #:neighbors-of-vertex
   #:get-vertex-marker
   #:get-mark
   #:all-vertices
   #:add-vertex
   #:add-edge
   #:remove-vertex
   #:remove-edge
   #:copy

   #:simple-graph))

