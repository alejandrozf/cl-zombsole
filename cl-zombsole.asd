;;;; cl-zombsole.asd

(asdf:defsystem #:cl-zombsole
  :description "Port of zombsole to CL"
  :author "Alejandro Zamora Fonseca <ale2014.zamora@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ansi-text #:unix-opts #:alexandria #:slynk #:swank)
  :components ((:file "package")
               (:file "cl-zombsole")
               (:file "players")
               (:file "game")))
