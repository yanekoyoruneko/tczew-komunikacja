;;;
;;; Requests and spider definitions.
;;;
(in-package #:pl.tczew.transit)

(defparameter +url-root+ "https://komunikacja.tczew.pl/"
  "Website contains bus lines numbers, directions and links to time tables.")

(defclass root-request (scrapycl:request)
  ()
  (:documentation "Start scraping."))

(defclass bus-spider (scrapycl:spider)
  ()
  (:documentation  "Scrapes the bus time-tables into directed weighted graph of stations.")
  (:default-initargs
   :initial-requests (list (make-instance 'root-request :url +url-root+))))

(defclass arrive-time-spider (scrapycl:spider)
  ()
  (:documentation "Scrapes from kurs.php the arrive time to the next destinations."))

(defclass route-table-request (scrapycl:request)
  ((line-nr :initarg :line-nr
    	    :type int
    	    :reader bus-nr
            :documentation "Line number of bus line")
   (route-direction :initarg :direction
                    :type string
                    :reader bus-dir
                    :documentation "The direction string from root page."))
  (:documentation "Request to scrape route-table of bus-line."))


(defclass time-table-request (route-table-request)
  ((station-name :initarg :name
                 :type string
                 :reader station-name
                 :documentation "Station name of time table for bus line.")
   (departure :initarg :departure
              :accessor departure
              :documentation "The next destination of bus line.")
   (time-suffix :initarg :time-suffix
                :type string
                :reader time-suffix
                :documentation "Filter times that don't match this suffix."))
  (:documentation "Request to scrape time-table of station from route-table."))


(defclass arrive-time-request (scrapycl:request)
  ()
  (:documentation "Request to scrape arrive time from kurs.php."))


(defun make-route-table-req (href direction)
  "Make new route-table-request from link route-table and bus-line direction."
  (make-instance 'route-table-request
                 :url href
                 ;; the time-table href of bus-line contains its line number
                 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" href))
                 :direction (str:trim direction)))
