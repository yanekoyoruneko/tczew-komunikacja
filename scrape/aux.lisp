(in-package #:tczew-transit)

(defconstant +kurs-fn-regex+ "kurs\\('([^']+)',\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*,\\s*([0-9]+)\\)"
  "Match 'kurs()' onclick function params which are used to query kurs.php.")

(defconstant +kurs-php-req+ "kurs.php?kat=~a&kier=~a&nr=~a&kurs=~a&a=1"
  "Request template to kurs.php with 4 parameters.")

(defun get-last-station-name (table)
  "Return the last station name from bus route-table. This is the finall destination."
  ;; The last station name is not inside <a></a> so it needs to be extracted separately.
  ;; trim the ^ from ^Station Name
  (str:trim (lquery:$1 table "tr" (last) "[style*=\"font-weight: bold;\"]" (text))))

(defun get-time-table-reqs (bus-req table)
  "Return array of time-table-requests ready to query bus time-tables."
  ;; Wciecie 1 are alternate routes tagged with A, C after time, im ignoring for now.
  ;; There is also wciecie 2 that omiting links the Konarskiego station for line 7
  ;; TCZEW-TRANSIT> (get-connections "Konarskiego" "Konarskiego")
  ;; ((("Konarskiego" . 7) "07:45" "10:45" "13:45"))
  (lquery:$ table "tr" "td" (not ".wciecie-1") "a"
            (combine
             (text)
             (lquery:$1 (attr :href) (merge-url-with +url-root+)))
            (map-apply (lambda (name href)
                         (make-instance 'time-table-request
                                        :url href
                                        :name name
                                        :line-nr (bus-nr bus-req)
                                        :direction (bus-dir bus-req))))))

(defun make-arrive-time-reqs (arrive-table-links)
  "Return array of arrive-time-request objects ready to query kurs.php."
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

(defun process-row-to-time (tr)
  "Return the (departure . arrive) list of cons from the row of bus line time-table."
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
          (coerce (lquery:$ time-table "tr" (gt 1) (map #'process-row-to-time)) 'list)))
