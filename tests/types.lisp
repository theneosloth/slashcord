(defpackage slashcord-tests
  (:use :cl :fiveam :slashcord-types)
  (:export :all-tests))

(in-package :slashcord-tests)

(setf fiveam:*run-test-when-defined* t)

(def-suite all-tests
    :description "Run all slashcord tests")

(in-suite all-tests)

(test encode-test
  (let* ((instance (make-instance 'application-command-post
                                 :name "Hello"
                                 :description "Test Command"
                                 :options #()))
         (json-stream (make-string-output-stream)))
    (yason:encode instance json-stream)
    (is (string= (get-output-stream-string json-stream) "{\"name\":\"Hello\",\"description\":\"Test Command\",\"options\":[]}"))))

(test choices-command
  (let* ((animal-choice-1 (make-instance 'command-choice :name "Dog" :value "animal_dog"))
         (animal-choice-2 (make-instance 'command-choice :name "Cat" :value "animal_cat"))
         (animal-choice-3 (make-instance 'command-choice :name "Penguin" :value "animal_penguin"))
         (animal-option (make-instance 'command-option
                                       :type +option-string+
                                       :name "animal"
                                       :description "The type of animal"
                                       :required t
                                       :choices (list animal-choice-1 animal-choice-2 animal-choice-3)))
         (only-small-option (make-instance 'command-option
                                           :type +option-boolean+
                                           :name "only_smol"
                                           :description "Whether to show only baby animals"
                                           :required nil))
         (blep-command (make-instance 'application-command-post
                                      :name "blep"
                                      :type +command-chat-input+
                                      :description "Send a random adorable animal photo"
                                      :options (list animal-option only-small-option)))
         (json (to-json blep-command)))
    (is (string= json "{\"type\":1,\"name\":\"blep\",\"description\":\"Send a random adorable animal photo\",\"options\":[{\"type\":3,\"name\":\"animal\",\"description\":\"The type of animal\",\"required\":true,\"choices\":[{\"name\":\"Dog\",\"value\":\"animal_dog\"},{\"name\":\"Cat\",\"value\":\"animal_cat\"},{\"name\":\"Penguin\",\"value\":\"animal_penguin\"}]},{\"type\":5,\"name\":\"only_smol\",\"description\":\"Whether to show only baby animals\",\"required\":false}]}"))))
