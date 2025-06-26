(defpackage slashcord/types
  (:use :cl)
  (:import-from :serapeum :-> :dict)
  (:import-from :easy-routes :defroute)
  (:import-from :json-mop :json-serializable-class)
  (:export
   :application-command-post
   :to-json
   :from-json
   :make-option
   :make-command
   :make-text-callback
   :get-interaction-id
   :interaction-type))
