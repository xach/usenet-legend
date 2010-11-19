;;;; usenet-legend.asd

(asdf:defsystem #:usenet-legend
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "utils")
               (:file "date-parser")
               (:file "tokens")
               (:file "article")
               (:file "split")
               (:file "phrase-search")
               (:file "io")
               (:file "serialization")
               (:file "tbv-file")
               (:file "cdb")
               (:file "corpus")
               (:file "search-query")
               (:file "results")))
