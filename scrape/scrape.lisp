;;;
;;; Scrape helpers.
;;;
(in-package #:pl.tczew.transit)

(defconstant +kurs-fn-regex+ "kurs\\('([^']+)',\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*,\\s*([0-9]+)\\)"
  "Match 'kurs()' onclick function params which are used to query kurs.php.")

(defconstant +kurs-php-req+ "kurs.php?kat=~a&kier=~a&nr=~a&kurs=~a&a=1"
  "Request template to kurs.php with 4 parameters.")

(defun scrape-last-station-name (table)
  "Return the last station name from bus route-table. This is the finall destination."
  ;; The last station name is not inside <a></a> so it needs to be extracted separately.
  ;; trim the ^ from ^Station Name
  (str:trim (lquery:$1 table "tr" (last) "[style*=\"font-weight: bold;\"]" (text))))

(defun scrape-last-alt-station-name (table)
  "Return the point where alternate route joins with the primary route."
  (str:trim (lquery:$1 table "tr" ".wciecie-1" (last) (parent) (next) (text))))

(defun scrape-route-split-point (route-table)
  "Return the station name from which the route splits."
  ;; split point is the station at which there is alternate route
  ;; this is the first station before .wciecie-1 or .wciecie-2
  ;; wciecie-2 has to be first here
  (lquery:$1 route-table "tr" ".wciecie-2, .wciecie-1" (parent) (prev) "a"
             (combine
              (text)
              (lquery:$1 (attr :href) (merge-url-with +url-root+)))))

(defun scrape-primary-route (route-table-req route-table split-point)
  "Return time-table-requests for the primary route ignoring any alternate A, B routes."
  (lquery:$ route-table "tr" "td" (not ".wciecie-1") (not ".wciecie-2") "a"
            (combine
             (text)
             (lquery:$1 (attr :href) (merge-url-with +url-root+)))
            (map-apply (lambda (name href)
                         (make-instance 'time-table-request
                                        :url href
                                        :name name
                                        :line-nr (bus-nr route-table-req)
                                        :direction (bus-dir route-table-req)
                                        ;; ignore the alternate route
                                        :time-suffix (if (string= split-point name)
                                                         ""
                                                         nil))))))

(defun scrape-alternate-time-table-reqs  (route-table-req route-table split-point)
  (let ((first-station (make-instance 'time-table-request
                                      :url (second split-point)
                                      :name (first split-point)
                                      :line-nr (bus-nr route-table-req)
                                      :direction (bus-dir route-table-req)
                                      ;; ignore the alternate route
                                      :time-suffix nil)))
    (concatenate 'vector
                 first-station
                 (lquery:$ route-table "tr" "td" ".wciecie-1" "a"
                           (combine
                            (text)
                            (lquery:$1 (attr :href) (merge-url-with +url-root+)))
                           (map-apply (lambda (name href)
                                        (make-instance 'time-table-request
                                                       :url href
                                                       :name name
                                                       :line-nr (bus-nr route-table-req)
                                                       :direction (bus-dir route-table-req)
                                                       ;; ignore the alternate route
                                                       :time-suffix nil)))))))

(defun scrape-primary-time-table-reqs (route-table-req route-table)
  "Return array of time-table-requests ready to query bus time-tables."
  ;; Wciecie 1 are alternate routes tagged with A, C after time, im ignoring for now.
  ;; There is also wciecie 2 that omiting links the Konarskiego station for line 7
  ;; TCZEW-TRANSIT> (get-connections "Konarskiego" "Konarskiego")
  ;; ((("Konarskiego" . 7) "07:45" "10:45" "13:45"))
  (check-type route-table-req route-table-request)
  (check-type route-table plump:element)
  (let ((split-point (scrape-route-split-point route-table)))
    (scrape-primary-route route-table-req route-table (first split-point))))

(defun link-route-table-reqs (station-time-tables last-station-name)
  (loop
    for prev-table = time-table
    for time-table across station-time-tables
    ;; route table is vector of the following destinations
    ;; so link here the departure station for later arrive time scraping
    unless (null prev-table)
      do (setf (departure prev-table)
               (station-name time-table))
    finally (setf (departure time-table)
                  last-station-name)
            (return station-time-tables)))


(defun scrape-route-table (route-req route-table)
  "Return time-table-requests for route-table."
  (check-type route-req route-table-request)
  (check-type route-table plump:element)
  (format t "LAST ALT: ~a~%" (scrape-last-alt-station-name route-table))
  (list (link-route-table-reqs (scrape-primary-time-table-reqs route-req route-table)
                               (scrape-last-station-name route-table))
        ;; (link-route-table-reqs (scrape-alternate-time-table-reqs route-req route-table)
        ;;                        (scrape-last-alt-station-name route-table))
        ))

(defun make-arrive-time-reqs (arrive-table-links)
  "Return array of arrive-time-request objects ready to query kurs.php."
  (check-type arrive-table-links vector)
  (let ((onclick (lquery:$ arrive-table-links
                           (map(lambda (el) (lquery:$1 el (attr "onclick")))))))
    (lquery:$ onclick (map (lambda (link)
                             ;; get onclick attribute a, b, c, d
                             ;; parameters to query rozklad.php
                             (ppcre:register-groups-bind (a b c d)
                                 (+kurs-fn-regex+ link)
                               (format nil +kurs-php-req+ a b c d))))
              (merge-url-with +url-root+)
              (map (lambda (url) (make-instance 'arrive-time-request :url url))))))

(defun scrape-arrive-times (arrive-table-links)
  "Return list of arrive times. "
  (mapcar #'arrive-time-minutes
          (scrapycl:start (make-instance
                           'arrive-time-spider
                           :initial-requests
                           (coerce (make-arrive-time-reqs arrive-table-links) 'list))
                          :wait t)))

(defun process-row-to-time (tr time-suffix)
  "Return the (departure . arrive) list of cons from the row of bus line time-table."
  (let* ((hour (lquery:$1 tr "td" (first) (text)))
         ;; there are 3 columns for each type of day
         ;; for now only (eq 1) for the normal days
         ;;--- TODO: implement the rest of days
         (minute-divs (lquery:$ tr "td" (eq 1) "div"))
         (arrive-table-links (lquery:$ tr "td" (eq 1) "div" "a")))
    (when (< 0 (length minute-divs))	; some rows are empty
      (when (= 1 (length hour))		; pad with 0
        (setf hour (str:concat "0" hour)))
      (loop for minutes across (lquery:$ minute-divs (map (lambda (el) (lquery:$1 el "a" (text)))))
            for arrive in (scrape-arrive-times arrive-table-links)
            when (or (not time-suffix) (string= (subseq minutes 2) time-suffix))
              collect (cons (str:concat hour ":" minutes) arrive)))))

(defun scrape-times (time-table departure time-suffix)
  "For each row of the time-table extracts the departure and arrive time.
row-to-time needs flattening mapcan"
  (mapcan #'(lambda (el) el)
          (coerce (lquery:$ time-table
                            "tr"
                            (gt 1)
                            (map #'(lambda (el) (process-row-to-time el time-suffix))))
                  'list)))
