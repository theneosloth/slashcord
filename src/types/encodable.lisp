(in-package :slashcord/types)

(defclass json-encodable ()
  ()
  (:documentation "A class that can be directly encoded into json"))

(defmethod to-json ((encodable json-encodable))
  (with-output-to-string (json)
    (json-mop:encode encodable json)))

(defmethod from-json ((input sequence) class &rest initargs)
  (declare (ignore initargs))
  (etypecase input
    (list
     (mapcar (lambda (element)
               (json-mop:json-to-clos element class))
             input))
    (vector
     (map 'vector
          (lambda (element)
            (json-mop:json-to-clos element class))
          input))))

(defmethod from-json ((input hash-table) class &rest initargs)
  (declare (ignore initargs))
  (json-mop:json-to-clos input class))
