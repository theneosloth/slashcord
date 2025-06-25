(in-package :slashcord/types)

(defclass interaction-callback (json-encodable)
  ((type :type interaction-type :initarg :type :initform +pong-callback+ :json-key "type")
   (data :initarg :data :type interaction-callback-data :json-type interaction-callback-data :json-key "data"))
  (:metaclass json-serializable-class))

(defparameter pong (make-instance 'interaction-callback :type +pong-callback+))
