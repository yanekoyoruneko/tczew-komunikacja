;;;; tczew-komunikacja.asd

(asdf:defsystem #:tczew-transit
  :description "Describe tczew-komunikacja here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:scrapycl #:alexandria)
  :components ((:file "package")
               (:file "scrape/graph")
               (:file "scrape/defreq")
               (:file "scrape/scrape")
               (:file "scrape/process")
               (:file "solve/query")))
