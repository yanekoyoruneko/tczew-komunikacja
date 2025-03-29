(in-package #:tczew-transit)

(defparameter *url-root* "https://komunikacja.tczew.pl/")

(defclass bus-lines-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'root-request :url *url-root*))))

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

(defun make-bus-line-req (page direction)
  (make-instance 'bus-line-request
    		 :url page
    		 :line-nr (parse-integer (cl-ppcre:scan-to-strings "\\d+" page))
    		 :direction (str:trim direction)))

(defun parse-root-to-stations-req (lines-req)
  "Extract from root page the route tables for each line"
  (lquery:$ (initialize lines-req)
	    "#routes" ".list-type" "a"
	    (combine
	     (lquery:$1 (attr :href) (merge-url-with *url-root*))
	     (attr "title"))
	    (map-apply (lambda (route-page direction)
			 (make-bus-line-req route-page direction)))))

(defun get-last-station-name (table)
  "The last station name is not inside <a></a> so it needs to be extracted separately."
  ;; trim the ^ from ^Station Name
  (str:trim (lquery:$1 table "tr" (last) "[style*=\"font-weight: bold;\"]" (text))))

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
			 (make-instance 'station-request
					:url href
					:name name
					:line-nr (bus-nr bus-req)
					:direction (bus-dir bus-req))))))

;;; eq 1 this is the table from the 3 tables
(defun row-to-time (tr)
  (let* ((hour (lquery:$1 tr "td" (first) (text)))
	 (minute-divs (lquery:$ tr "td" (eq 1) "div")))
    (when (> (length minute-divs) 0)	; some rows are empty
      (when (= (length hour) 1)		; pad with 0
	(setf hour (str:concat "0" hour)))
      (loop for minutes across (lquery:$ minute-divs (map (lambda (el) (lquery:$1 el (text)))))
	    collect (str:concat hour ":" minutes)))))


(defun get-departure-time (time-table)
  ;; first tr is the name of the "three-table" table
  (alexandria:flatten (coerce (lquery:$ time-table "tr" (gt 1) (map #'row-to-time)) 'list)))
