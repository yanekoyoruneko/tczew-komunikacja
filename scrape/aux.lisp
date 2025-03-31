(in-package #:tczew-transit)

(defconstant +kurs-fn-regex+ "kurs\\('([^']+)',\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*,\\s*([0-9]+)\\)"
  "Matches kurs() onclick function parameters of time table.")


(defconstant +kurs-php-req+ "kurs.php?kat=~a&kier=~a&nr=~a&kurs=~a&a=1"
  "Request template to rozklad.php.")


(defun parse-root-to-stations-req (lines-req)
  "Extract from root page the route tables for each bus line."
  (lquery:$ (initialize lines-req)
	    "#routes" ".list-type" "a"
	    (combine
	     (lquery:$1 (attr :href) (merge-url-with +url-root+))
	     (attr "title"))
	    (map-apply (lambda (route-page direction)
			 (make-bus-line-req route-page direction)))))


(defun get-last-station-name (table)
  "The last station name is not inside <a></a> so it needs to be extracted separately."
  ;; trim the ^ from ^Station Name
  (str:trim (lquery:$1 table "tr" (last) "[style*=\"font-weight: bold;\"]" (text))))


(defun get-route-table-stations (bus-req table)
  "Extract from route table station name and the url for its time-table
Wciecie 1 are alternate routes tagged with A, C after time, im ignoring for now."
  ;; There is also wciecie 2 that omiting links the Konarskiego station for line 7
  ;; TCZEW-TRANSIT> (get-connections "Konarskiego" "Konarskiego")
  ;; ((("Konarskiego" . 7) "07:45" "10:45" "13:45"))
  (lquery:$ table "tr" "td" (not ".wciecie-1") "a"
	    (combine
	     (text)
	     (lquery:$1 (attr :href) (merge-url-with +url-root+)))
	    (map-apply (lambda (name href)
			 (make-instance 'station-request
					:url href
					:name name
					:line-nr (bus-nr bus-req)
					:direction (bus-dir bus-req))))))


(defun make-arrive-time-req (arrive-table-links)
  "Extracts from the links onclick attribute a, b, c, d parameters to query rozklad.php
for the following station arrive time. "
  (let ((onclick (lquery:$ arrive-table-links
			   (map(lambda (el) (lquery:$1 el (attr "onclick")))))))
    (lquery:$ onclick (map (lambda (link)
			     (ppcre:register-groups-bind (a b c d)
				 (+kurs-fn-regex+ link)
			       (format nil +kurs-php-req+ a b c d))))
	      (merge-url-with +url-root+)
	      (map (lambda (url) (make-instance 'arrive-time-request :url url))))))


(defun scrape-arrive-times (arrive-table-links)
  "Extracts time from the arrive-time objects returned by spider.
Example query made by spider: kurs.php?kat=001_20250301&kier=1&nr=1&kurs=2&a=1"
  (mapcar #'arrive-time-minutes
	  (scrapycl:start (make-instance
			   'arrive-time-spider
			   :initial-requests
			   (coerce (make-arrive-time-req arrive-table-links) 'list))
			  :wait t)))


(defun row-to-time (tr)
  "Extracts the departure and arrive times from the row of bus line time-table."
  (let* ((hour (lquery:$1 tr "td" (first) (text)))
	 (minute-divs (lquery:$ tr "td" (eq 1) "div"))
	 (arrive-table-links (lquery:$ tr "td" (eq 1) "div" "a")))
    (when (< 0 (length minute-divs))	; some rows are empty
      (when (= 1 (length hour))		; pad with 0
	(setf hour (str:concat "0" hour)))
      (loop for minutes across (lquery:$ minute-divs (map (lambda (el) (lquery:$1 el "a" (text)))))
	    for arrive in (scrape-arrive-times arrive-table-links)
	    collect (cons (str:concat hour ":" minutes) arrive)))))


(defun get-departure-times (time-table)
  "For each row of the time-table extracts the departure and arrive time.
row-to-time needs flattening mapcan"
  (mapcan #'(lambda (el) el)

	  (coerce (lquery:$ time-table "tr" (gt 1) (map #'row-to-time)) 'list)))
