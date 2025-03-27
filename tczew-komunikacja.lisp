    ;;;; tczew-komunikacja.lisp

(in-package #:tczew-komunikacja)


(defparameter *url-root* "https://komunikacja.tczew.pl/")

(defclass root-request (scrapycl:request)
  ())

(defclass bus-line-request (scrapycl:request)
  ((line-nr :initarg :line-nr
    	    :type int
    	    :reader bus-nr)
   (direction :initarg :direction
    	      :type string
    	      :reader bus-dir)))

(defclass station-request (bus-line-request)
  ((station :initarg :station
	    :type string
	    :reader bus-station)))

(defclass bus-lines-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'root-request :url *url-root*))))

(defun make-bus-line-req (page direction)
  (make-instance 'bus-line-request
    		 :url page
    		 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" page))
    		 :direction (str:trim direction)))

(defun make-station-req (bus-line-req station-name station-page)
  (make-instance 'station-request
    		 :url station-page
    		 :station station-name
    		 :line-nr (bus-nr bus-line-req)
    		 :direction (bus-dir bus-line-req)))

(defmethod print-object ((obj station-request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "STATION ~a "
	    (bus-station obj)))
  (call-next-method))

(defmethod print-object ((obj bus-line-request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NR ~A URL ~A DIR ~A"
	    (bus-nr obj)
	    (scrapycl:request-url obj)
	    (bus-dir obj))))

(defun parse-root-to-stations-req (lines-req url)
  "Extract from root page url of bus lines route tables"
  (lquery:$ (initialize lines-req)
	    "#routes" ".list-type" "a"
	    (combine
	     (lquery:$1 (attr :href) (merge-url-with *url-root*))
	     (attr "title"))
	    (map-apply (lambda (route-page direction)
    			 (make-bus-line-req route-page direction)))))

;; kurs url
;; kurs.php?kat=001_20250301&kier=1&nr=1&kurs=2&a=1

(defmethod scrapycl:process ((spider bus-lines-spider)
    			     (request root-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (print (coerce (parse-root-to-stations-req data url) 'list))))


(defun get-route-table-stations (bus-req table)
  "Extract from route table station name and the url for its time table"
  (lquery:$ table "a"
	    (combine
	     (text)
	     (lquery:$1 (attr :href) (merge-url-with *url-root*)))
	    (map-apply (lambda (name href)
			 (make-station-req bus-req name href)))))

(defmethod scrapycl:process ((spider bus-lines-spider)
			     (bus-req bus-line-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider bus-req)
    (let* ((route-tables (lquery:$ (initialize data) ".route" (gt 1))))
      (list (first (coerce (get-route-table-stations bus-req (aref route-tables 0)) 'list))))))


(defclass station ()
  ((station :initarg :station :accessor station)))

(defmethod print-object ((obj station) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "station: ~A" (station obj))))

(defun get-departure-time (time-table)
  (lquery:$ (initialize time-table) ".table-three" "tr" (gt 1)
	    (map (lambda (el)
		   (list (lquery:$1 el "td" (first) (text))
			 (lquery:$ el "td" (eq 1) (map (lambda (el)
							 (lquery:$ el (text))))))))))


(defmethod scrapycl:process ((spider bus-lines-spider)
			     (st-req station-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider st-req)
    (log:info url)
    (log:info (get-departure-time data))
    (values)))

(scrapycl:start (make-instance 'bus-lines-spider) :wait t)
