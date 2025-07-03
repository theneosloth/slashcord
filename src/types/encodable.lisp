(in-package :slashcord/types)

(defclass json-encodable ()
  ()
  (:documentation "A class with a json-mop metaclass."))

(defmethod to-json ((encodable json-encodable))
  "String result of json-mop:encode on ENCODABLE."
  (with-output-to-string (json)
    (json-mop:encode encodable json)))

(defmethod from-json ((input sequence) class &rest initargs)
  "Converts an INPUT sequence of json strings to CLASS."
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
  "Convert INPUT hash table to a CLASS."
  (declare (ignore initargs))
  (json-mop:json-to-clos input class))
