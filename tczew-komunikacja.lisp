    ;;;; tczew-komunikacja.lisp

(in-package #:tczew-komunikacja)


(defparameter *url-root* "https://komunikacja.tczew.pl/")
(defparameter *stations* (make-hash-table :test 'equal))

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
  ((name :initarg :name
	 :type string
	 :reader name)
   (departure :initarg :departure
	      :accessor departure)))

(defclass bus-lines-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'root-request :url *url-root*))))


(defclass station ()
  ((cost-to :initarg :cost-to
	    :documentation "A list of costs to other stations."
	    :type list)
   (name :initarg :station
	 :type string)
   (departures :initarg :departures
	       :reader departures
	       :type list)))

(defclass station-departure ()
  ((dest :initarg :dest
	 :type station
	 :reader dest)
   (dtime :initarg :dtime
	  :type string
	  :reader dtime)))


(defun make-bus-line-req (page direction)
  (make-instance 'bus-line-request
    		 :url page
    		 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" page))
    		 :direction (str:trim direction)))

(defun make-station-req (&key bus name page)
  (make-instance 'station-request
    		 :url page
    		 :name name
    		 :line-nr (bus-nr bus)
    		 :direction (bus-dir bus)))

(defmethod print-object ((obj station-request) stream)
  (format stream "\"~a\"~C" (name obj) #\Tab)
  (call-next-method))

(defmethod print-object ((obj bus-line-request) stream)
  (format stream "[~A] href:~A to [~A]"
	  (bus-nr obj)
	  (scrapycl:request-url obj)
	  (bus-dir obj)))

(defun parse-root-to-stations-req (lines-req)
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
(defun get-route-table-stations (bus-req table)
  "Extract from route table station name and the url for its time table
Wciecie 1 are alternate routes im ignoring for now"
  (lquery:$ table "tr" "td" (not ".wciecie-1") "a"
	    (combine
	     (text)
	     (lquery:$1 (attr :href) (merge-url-with *url-root*)))
	    (map-apply (lambda (name href)
			 (make-station-req
			  :bus bus-req
			  :name name
			  :page href)))))

(defun get-last-station-name (table)
  ;; trim the ^
  (str:trim (lquery:$1 table "tr" (last) "[style*=\"font-weight: bold;\"]" (text))))


;;; eq 1 this is the table from the 3 tables
(defun row-to-time (tr)
  (let* ((hour (lquery:$1 tr "td" (first) (text)))
	 (minute-divs (lquery:$ tr "td" (eq 1) "div")))
    (when (> (length minute-divs) 0)	; some rows are empty
      (when (= (length hour) 1)		; pad with 0
	(setf hour (str:concat "0" hour)))
      (loop for minutes across (lquery:$ minute-divs (map (lambda (el) (lquery:$1 el (text)))))
	    collect (str:concat hour ":" minutes)))))


(defmethod scrapycl:process ((spider bus-lines-spider)
    			     (request root-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (declare (ignore url))
    ;; limit to one bus line
    (parse-root-to-stations-req data)))


(defmethod scrapycl:process ((spider bus-lines-spider)
			     (bus-req bus-line-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider bus-req)
    (declare (ignore url))
    ;; for now only the first route table
    ;; the second one is in the opposite direction
    (let* ((route-tables (lquery:$ (initialize data) ".route" (gt 1)))
	   (table (aref route-tables 0))
	   (stations (get-route-table-stations bus-req table))
	   (last-station (get-last-station-name table)))
      (loop
	for prev-st = st
	for st across stations
	unless (null prev-st)
	  do (setf (departure prev-st) (name st))
	finally (setf (departure st) last-station))
      (print stations)
      stations)))


(defun get-departure-time (time-table)
  ;; first tr is the name of the "three-table" table
  (alexandria:flatten (coerce (lquery:$ time-table "tr" (gt 1) (map #'row-to-time)) 'list)))


(defmethod scrapycl:process ((spider bus-lines-spider)
			     (station-req station-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider station-req)
    (log:info url)
    (let ((table (get-departure-time (lquery:$ (initialize data) ".table-three")))
	  (connection (cons (departure station-req) (bus-nr station-req))))
      (if (not (assoc (name station-req) *stations* :test 'equal))
	  (setf *stations* (acons (name station-req) `((,connection . ,table)) *stations*))
	  (let ((station-alist (assoc (name station-req) *stations* :test 'equal)))
	    (rplacd (cdr station-alist) `((,connection . ,table))))))
    (values)))

(scrapycl:start (make-instance 'bus-lines-spider) :wait t)
