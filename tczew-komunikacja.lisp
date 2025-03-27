    ;;;; tczew-komunikacja.lisp

(in-package #:tczew-komunikacja)


(defparameter *url-root* "https://komunikacja.tczew.pl/")

(defclass bus-lines-request (scrapycl:request)
  ())

(defclass station-request (scrapycl:request)
  ((line-nr :initarg :line-nr
    	    :type int
    	    :reader bus-nr)
   (direction :initarg :direction
    	      :type string
    	      :reader bus-dir)))

(defclass bus-lines-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'bus-lines-request :url *url-root*))))

(defun make-station-req (page direction)
  (make-instance 'station-request
    		 :url page
    		 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" page))
    		 :direction (str:trim direction)))

(defmethod print-object ((obj station-request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NR ~A ROUTES ~A AT ~A"
	    (bus-nr obj)
	    (scrapycl:request-url obj)
	    (bus-dir obj))))

(defun parse-root-to-stations-req (lines-req url)
  (lquery:$ (initialize lines-req)
	    "#routes" ".list-type" "a"
	    (combine (lquery:$ (attr :href) (merge-url-with url)) (attr "title"))
	    (map-apply (lambda (route-page direction)
    			 (make-station-req (aref route-page 0) direction)))))


(defmethod scrapycl:process ((spider bus-lines-spider)
    			     (request bus-lines-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (coerce (parse-root-to-stations-req data url) 'list)))

(defmethod scrapycl:process ((spider bus-lines-spider)
			     (request station-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (let ((route-tables (lquery:$ (initialize data) ".route" (gt 1))))
      (log:info (bus-nr request))
      (log:info route-tables))))

(scrapycl:start (make-instance 'bus-lines-spider) :wait t)
