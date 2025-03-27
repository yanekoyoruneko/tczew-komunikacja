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
  (lquery:$ (initialize lines-req)
	    "#routes" ".list-type" "a"
	    (combine (lquery:$ (attr :href) (merge-url-with url)) (attr "title"))
	    (map-apply (lambda (route-page direction)
    			 (make-bus-line-req (aref route-page 0) direction)))))

;; kurs url
;; kurs.php?kat=001_20250301&kier=1&nr=1&kurs=2&a=1

(defmethod scrapycl:process ((spider bus-lines-spider)
    			     (request root-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (coerce (parse-root-to-stations-req data url) 'list)))

(defun get-route-table-stations (table)
  (lquery:$ table "a" (combine (attr :href) (text))))

(defmethod scrapycl:process ((spider bus-lines-spider)
			     (request bus-line-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (let* ((route-tables (lquery:$ (initialize data) ".route" (gt 1)))
	   (stations (coerce (get-route-table-stations (aref route-tables 0)) 'list))
	   (req '()))
      (log:info (bus-nr request))
      (dolist (st stations)
	(push (make-station-req request (second st) (first st)) req))
      req)))


(defclass station ()
  ((station :initarg :station :accessor station)))

(defmethod print-object ((obj station) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "station: ~A" (station obj))))

(defmethod scrapycl:process ((spider bus-lines-spider)
			     (request station-request))
  (list (make-instance 'station :station (bus-station request))))

(scrapycl:start (make-instance 'bus-lines-spider) :wait t)
