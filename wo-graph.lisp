;;; wo-graph.lisp

(in-package #:wo-graph)

;;; Basic graph inquiry options

(defgeneric all-vertices (graph)
  (:documentation "Returns in a list all vertices of the `graph'."))

(defgeneric outgoing-edges (vertex graph)
  (:documentation "A list of all edges which have as source vertex
  `vertex' in the `graph'."))

(defgeneric incoming-edges (vertex graph)
  (:documentation "A list of all edges which have as target vertex
  `vertex' in the `graph'."))

(defgeneric source-vertex (edge graph)
  (:documentation "A list of source vertices for the `edge' in the
`graph'.  Note that for all normal graphs this will return a list of
length one.  However for hyper graphs (?)  this can return more than
one element."))

(defgeneric target-vertex (edge graph)
  (:documentation "A list of target vertices for the `edge' in the
`graph'.  Note that for all normal graphs this will return a list of
length one.  However for hyper graphs (?)  this can return more than
one element."))

(defgeneric sources-of-vertex (vertex graph)
  (:documentation "A list of all source vertices of `vertex' in
`graph'.  A vertex `a' is a source of `vertex' if there is an edge
which has `vertex' as a target s and `a' as a source.  The collection
can contain duplicates if there are multiple edges connecting the
source with `vertex'."))

(defgeneric targets-of-vertex (vertex graph)
  (:documentation "A list of all target vertices of `vertex' in
`graph'.  A vertex `b' is a target of `vertex' if there is an edge
which has `vertex' as a source s and `b' as a target.  The collection
can contain duplicates if there are multiple edges connecting the
`vertex' with a target."))
(defgeneric neighbors-of-vertex (vertex graph))

(defgeneric get-vertex-marker (graph)
  (:documentation "Returns a vertex marker for the graph.  A vertex
  marker is a data structure that associates auxiliary data with
  vertices of `graph'.  The twomethods that are available on a vertex
  marker are `get-mark' and `(setf get-mark)'."))

(defgeneric get-mark (object marker &optional default-value)
  (:documentation "Gets the associated data of `object' (typically a
  vertex of a graph) by the datastructure `marker'.  If it is not
  found it will return `default-value' (or nil if the default value is
  not specified."))
(defgeneric update-mark (object marker &rest default-and-value))
(defsetf get-mark update-mark)


;; API For manipulating graph

(defgeneric add-vertex (vertex graph))
(defgeneric add-edge (source target edge graph))

(defgeneric remove-vertex (vertex graph))
(defgeneric remove-edge (edge graph))

;; API High level manipulating graph

(defgeneric copy (graph))
(defgeneric edge-test (graph))
(defgeneric vertex-test (graph))

;;;
;;; Implementation
;;;
(defclass simple-graph ()
  (outgoing-edge-map
   incoming-edge-map
   source-vertex-map
   target-vertex-map
   (edge-test :accessor edge-test)
   (vertex-test :accessor vertex-test)))

(defclass marker ()
  ((table :accessor table :initarg :with-table)))

(defclass vertex-marker (marker) ())
(defclass edge-marker (marker) ())


(defmethod initialize-instance :after  ((instance simple-graph) 
					&key (test nil)
					  (vertex-test (or test #'eql))
					  (edge-test (or test #'equalp))
					  &allow-other-keys)
  (with-slots (outgoing-edge-map incoming-edge-map 
				 source-vertex-map target-vertex-map)
      instance
    (setf (slot-value instance 'edge-test) edge-test)
    (setf (slot-value instance 'vertex-test) vertex-test)

    (setf outgoing-edge-map (make-hash-table :test edge-test))
    (setf incoming-edge-map (make-hash-table :test edge-test))
    (setf source-vertex-map (make-hash-table :test vertex-test))
    (setf target-vertex-map (make-hash-table :test vertex-test))))


(defun copy-hash-table-slot (target source slot)
  (setf (slot-value target slot)
	(copy-hash-table (slot-value source slot))))

(defmethod copy ((instance simple-graph))
  (let ((result (make-instance (class-of instance) 
			       :edge-test (edge-test instance)
			       :vertex-test (vertex-test instance))))
    (copy-hash-table-slot result instance 'outgoing-edge-map)
    (copy-hash-table-slot result instance 'incoming-edge-map)
    (copy-hash-table-slot result instance 'source-vertex-map)
    (copy-hash-table-slot result instance 'target-vertex-map)
    result))
      
(defmethod get-vertex-marker ((graph simple-graph))
  (make-instance 'vertex-marker 
		 :with-table  (make-hash-table :test (vertex-test graph))))

(defmethod get-mark ((object t) (marker vertex-marker) &optional default-value)
  (gethash object (table marker) default-value))

(defmethod update-mark ((object t) (marker vertex-marker) &rest default-and-value)
  (setf (gethash object (table marker)) (car (last default-and-value))))

(defmethod add-vertex ((vertex t) (graph simple-graph))
  (with-slots (outgoing-edge-map incoming-edge-map) graph
      (unless (nth-value 1 (gethash vertex incoming-edge-map))
	(setf (gethash vertex incoming-edge-map) (list)))
      (unless (nth-value 1 (gethash vertex outgoing-edge-map))
	(setf (gethash vertex outgoing-edge-map) (list)))))


(defmethod remove-vertex ((vertex t) (graph simple-graph))
    (loop :for edge :in (outgoing-edges vertex graph) :do (remove-edge edge graph))
    (loop :for edge :in (incoming-edges vertex graph) :do (remove-edge edge graph))
    
    (with-slots (outgoing-edge-map incoming-edge-map) graph
      (remhash vertex outgoing-edge-map)
      (remhash vertex incoming-edge-map)))

(defmethod remove-edge ((edge t) (graph simple-graph))
  (with-slots (source-vertex-map target-vertex-map) graph
    (remhash edge source-vertex-map)
    (remhash edge target-vertex-map)))
    
(defmethod add-edge ((source t) (target t) (edge t) (graph simple-graph))
  (add-vertex source graph)
  (add-vertex target graph)
  (with-slots (outgoing-edge-map incoming-edge-map source-vertex-map target-vertex-map 
				 edge-test vertex-test) 
      graph
    (let ((edge (or edge (make-array 2 :initial-contents (list source target)))))
      (pushnew edge (gethash target incoming-edge-map (list)) :test edge-test)
      (pushnew edge (gethash source outgoing-edge-map (list)) :test edge-test)
      (pushnew source (gethash edge source-vertex-map (list)) :test vertex-test)
      (pushnew target (gethash edge target-vertex-map (list)) :test vertex-test))))

(defmethod all-vertices ((graph simple-graph))
  (with-slots (incoming-edge-map) graph
    (hash-table-keys incoming-edge-map)))

(defmethod outgoing-edges ((vertex t) (graph simple-graph))
  (with-slots (outgoing-edge-map) graph
    (gethash vertex outgoing-edge-map)))

(defmethod incoming-edges ((vertex t) (graph simple-graph))
  (with-slots (incoming-edge-map) graph
    (gethash vertex incoming-edge-map)))

(defmethod source-vertex ((edge t) (graph simple-graph))
  (with-slots (source-vertex-map) graph
    (gethash edge source-vertex-map)))

(defmethod target-vertex ((edge t) (graph simple-graph))
  (with-slots (target-vertex-map) graph
    (gethash edge target-vertex-map)))



;;;; Generic graph functions


(defmethod source-vertex ((edge-list list) (graph t))
  (loop :for edge :in edge-list
     :append (source-vertex edge graph)))

(defmethod target-vertex ((edge-list list) (graph t))
  (loop :for edge :in edge-list 
     :append (target-vertex edge graph)))

(defmethod targets-of-vertex ((vertex t) (graph t))
  (target-vertex (outgoing-edges vertex graph) graph))

(defmethod sources-of-vertex ((vertex t) (graph t))
  (source-vertex (incoming-edges vertex graph) graph))


(defmethod neighbors-of-vertex (vertex graph)
  (concatenate 'list
	       (sources-of-vertex vertex graph)
	       (targets-of-vertex vertex graph)))
