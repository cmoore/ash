
(asdf:defsystem #:ash
  :description "The science officer on the Nostromo."
  :author "Clint Moore <clint@ivy.io>"
  :license "MIT"
  :serial t
  
  :depends-on (#:drakma
               #:parenscript
               #:yason
               #:optima)
  
  :components (:file "ash"))
