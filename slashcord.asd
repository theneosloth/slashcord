(asdf:defsystem "slashcord"
  :version "0.0.1"
  :author "Stefan Kuznetsov"
  :license "MIT"
  :depends-on ("alexandria" "lispcord" "ironclad" "flexi-streams" "hunchentoot" "easy-routes" "serapeum" "com.inuoe.jzon")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "types")
                 (:file "discord"))))
  :description "")
