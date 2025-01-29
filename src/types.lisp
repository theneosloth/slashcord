(in-package :slashcord)

(defparameter *interaction-hash*
  (dict
   1 :ping
   2 :application-command
   3 :message-component
   4 :application-command-autocomplete
   5 :modal-submit))

(-> interactionp (t) boolean)
(defun interactionp (thing)
  (and (integerp thing)
       (gethash thing *interaction-hash*)
       t))

(deftype interaction ()
  `(satisfies interactionp))

(defclass interaction-response ()
  ((type :type interaction :initarg :type :initform 1)))

(defmethod print-object  ((interaction-response interaction-response) stream)
  (print-unreadable-object (interaction-response stream)
    (format stream
            "~a"
            (jzon:stringify interaction-response))))

(defparameter ping (make-instance 'interaction-response :type 1))
