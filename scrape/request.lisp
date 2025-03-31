(in-package #:tczew-transit)

(defparameter +url-root+ "https://komunikacja.tczew.pl/"
  "Website contains bus lines numbers, directions and links to time tables.")


(defclass bus-lines-spider (scrapycl:spider)
  ()
  (:documentation  "The first requests extracts information about bus lines.")
  (:default-initargs
   :initial-requests (list (make-instance 'root-request :url +url-root+))))


(defclass arrive-time-spider (scrapycl:spider)
  ()
  (:documentation "Extracts from rozklad.php the arrive times to the following station."))


(defclass root-request (scrapycl:request)
  ())


(defclass bus-line-request (scrapycl:request)
  ((line-nr :initarg :line-nr
    	    :type int
    	    :reader bus-nr)
   (direction :initarg :direction
    	      :type string
    	      :reader bus-dir))
  (:documentation "Request to scrape the time-table of bus line."))



(defclass station-request (bus-line-request)
  ((name :initarg :name
	 :type string
	 :reader name)
   (departure :initarg :departure
	      :accessor departure)))


(defclass arrive-time-request (scrapycl:request)
  ())


(defun make-bus-line-req (page direction)
  "The time-table of bus line contains its line number which gets extracted here."
  (make-instance 'bus-line-request
    		 :url page
    		 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" page))
    		 :direction (str:trim direction)))
