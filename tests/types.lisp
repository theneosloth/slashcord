(defpackage slashcord-tests
  (:use :cl :fiveam :slashcord/types)
  (:export :run! :all-tests))

(in-package :slashcord-tests)

(def-suite all-tests
    :description "Run all slashcord tests")

(in-suite all-tests)

(test encode-test
  (let* ((instance (make-instance 'application-command-post
                                 :name "Hello"
                                 :description "Test Command"
                                 :options #()
                                 ))
         (json-stream (make-string-output-stream)))
    (yason:encode instance json-stream)
    (is (string= (get-output-stream-string json-stream) "{\"name\":\"Hello\",\"description\":\"Test Command\",\"options\":[],\"nsfw\":false}"))))


(run-all-tests)
