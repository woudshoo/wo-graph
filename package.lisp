;;;; package.lisp

(defpackage #:wo-graph
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table #:hash-table-keys)
  (:export 
   ;; Basic
   #:outgoing-edges
   #:incoming-edges
   #:source-vertex
   #:target-vertex

   #:all-vertices

   #:add-vertex
   #:add-edge
   #:remove-vertex
   #:remove-edge

   #:copy

   ;; Basic Marker
   #:get-vertex-marker
   #:get-mark

   ;; Derived
   #:sources-of-vertex
   #:targets-of-vertex
   #:neighbors-of-vertex

   ;; Implementation
   #:simple-graph))

