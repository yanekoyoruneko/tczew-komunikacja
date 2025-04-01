;;;
;;; Main scraping pipeline.
;;;

(in-package #:pl.tczew.transit)

(defun scrape ()
  (loop
    initially (setf *stations* nil)
    for connection in (scrapycl:start (make-instance 'bus-spider) :wait t)
    do (add-connection connection)
    finally
       (with-open-file (out "graph.txt" :direction :output :if-exists :supersede)
         (print *stations* out))
       (return *stations*)))

(defmethod scrapycl:process ((spider bus-spider)
                             (request root-request))
  "Return list of route-table-requests for each bus line."
  (multiple-value-bind (data url) (scrapycl:fetch spider request)
    (declare (ignore url))
    (list (aref (lquery:$ (initialize data)
                          "#routes" ".list-type" "a"
                          (combine
                           (lquery:$1 (attr :href) (merge-url-with +url-root+))
                           (attr "title"))
                          (map-apply (lambda (route-page direction)
                                       (make-route-table-req route-page direction))))
                1))))


(defmethod scrapycl:process ((spider bus-spider)
                             (route-req route-table-request))
  "Return list of time-table-reqs for each station of bus line."
  (multiple-value-bind (data url) (scrapycl:fetch spider route-req)
    (declare (ignore url))
    ;; some bus-lines are cycle and some have 2 route tables in the other direction
    (loop for table across (lquery:$ (initialize data) ".route" (gt 1))
          collect (scrape-route-table route-req table))))

(defclass connection ()
  ((start-station :initarg :start-station :accessor start-station :type string
                  :documentation "Starting station.")
   (transit-line :initarg :transit-line :accessor transit-line :type string
                 :documentation "Line number and destination.")
   (time-table :initarg :time-table :accessor time-table :type list
               :documentation "List of departure times and the cost it takes to get to the destination using this transit line"))
  (:documentation "Represents station connection on graph of transit lines."))


(defmethod scrapycl:process ((spider bus-spider)
                             (time-req time-table-request))
  "Return list of connections from departure station of time-req."
  (multiple-value-bind (data url) (scrapycl:fetch spider time-req)
    (let ((station-name (station-name time-req))
          (time-table (scrape-times (lquery:$ (initialize data) ".table-three")
                                    (departure time-req)
                                    (time-suffix time-req)))
          (bus-line (cons (departure time-req) (bus-nr time-req))))
      (format t "~a: ~a" (station-name time-req) (time-suffix time-req))
      (log:info "PROCESSING: " url)
      (list (make-instance 'connection
                           :start-station station-name
                           :transit-line bus-line
                           :time-table time-table)))))

(defclass arrive-time ()
  ((minutes :initarg :minutes :accessor arrive-time-minutes :type int))
  (:documentation "Arrive time at station."))


(defmethod scrapycl:process ((spider arrive-time-spider)
                             (arrive-req arrive-time-request))
  "Scrape arrive time at the destination. "
  (multiple-value-bind (data url) (scrapycl:fetch spider arrive-req)
    (declare (ignore url))
    (list (make-instance 'arrive-time :minutes
                         (parse-integer
                          ;; next station row is after the first one highlighted
                          ;; with color #000
                          (lquery:$1 (initialize data)
                                     "tr"
                                     (has "[color=\"#000\"]")
                                     (eq 1)
                                     "td"
                                     (eq 2)
                                     (text))
                          :junk-allowed t)))))
