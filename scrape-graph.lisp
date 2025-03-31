(in-package #:tczew-transit)

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
      stations)))

(defclass connection ()
  ((start-station :initarg :start-station :accessor start-station :type string)
   (transit-line :initarg :transit-line :accessor transit-line :type string)
   (time-table :initarg :time-table :accessor time-table :type list)))

(defmethod scrapycl:process ((spider bus-lines-spider)
                             (station-req station-request))
  (multiple-value-bind (data url) (scrapycl:fetch spider station-req)
    (let ((station-name (name station-req))
	  (time-table (get-departure-time (lquery:$ (initialize data) ".table-three")))
          (bus-line (cons (departure station-req) (bus-nr station-req))))
      (log:info url)
      (list (make-instance 'connection
			   :start-station station-name
			   :transit-line bus-line
			   :time-table time-table)))))

(defclass arrive-time ()
  ((minutes :initarg :minutes :accessor arrive-time-minutes :type int)))

(defmethod scrapycl:process ((spider arrive-time-spider)
                             (arrive-time-req arrive-time-request))
  "Extracts arrive time to the next station."
  (multiple-value-bind (data url) (scrapycl:fetch spider arrive-time-req)
    (declare (ignore url))
    ;; the row with the next station is the one after the first one highlighteed
    (list (make-instance 'arrive-time :minutes
			 (parse-integer (lquery:$1 (initialize data) "tr" (has "[color=\"#000\"]") (eq 1)
						   "td" (eq 2) (text))
					:junk-allowed t)))))


(defun scrape ()
  (loop
    initially (setf *stations* nil)
    for connection in (scrapycl:start (make-instance 'bus-lines-spider) :wait t)
    do (add-connection connection)
    finally (return *stations*)))
