    ;;;; tczew-komunikacja.lisp

(in-package #:tczew-komunikacja)


(defparameter *url-root* "https://komunikacja.tczew.pl/")
(defparameter *stations* nil)

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
    (list (aref (parse-root-to-stations-req data) 0))))


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
      stations)))

(defun get-departure-time (time-table)
  ;; first tr is the name of the "three-table" table
  (alexandria:flatten (coerce (lquery:$ time-table "tr" (gt 1) (map #'row-to-time)) 'list)))

(defclass connection ()
  ((station :initarg :station :accessor station :type string)
   (line :initarg :line :accessor line :type string)
   (time-table :initarg :time-table :accessor station-time-table :type list)))

(defun find-station (name)
  (assoc name *stations* :test 'equal))

(defun get-connections (connection station)
  (remove-if-not (lambda (con) (string= connection (caar con))) (cdr station)))

(defun add-connection (name bus-line time-table)
  (let ((entry (find-station name)))
    (push `(,bus-line . ,time-table) (cdr entry))))

(defun new-station (name)
  (setf *stations* (acons name '() *stations*)))


(defmethod scrapycl:process ((spider bus-lines-spider)
                             (station-req station-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider station-req)
    (let ((station-name (name station-req))
	  (time-table (get-departure-time (lquery:$ (initialize data) ".table-three")))
          (bus-line (cons (departure station-req) (bus-nr station-req))))
      (when (not (find-station station-name))
	(new-station station-name))
      (log:info url)
      (add-connection station-name bus-line time-table)
      (list (make-instance 'station :name station-name :time-table (cons bus-line time-table))))))

(loop for station across (scrapycl:start (make-instance 'bus-lines-spider) :wait t)
      do
	 (when (not (find-station station-name))
	   (new-station station-name))
	 (log:info url)
	 (add-connection station-name bus-line time-table))
