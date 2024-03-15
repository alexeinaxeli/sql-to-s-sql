(defsystem :SQL-TO-S-SQL
  :author "Alexane Jouglar"
  :license ""
  :serial t
  :depends-on (:utils :fcg :irl :postmodern)
  :components ((:file "package")
               (:file "predicate-translation")
               (:file "general-functions")
               (:file "string-to-s-sql"))
  :description "A Common Lisp package for manipulating Postmodern queries")
