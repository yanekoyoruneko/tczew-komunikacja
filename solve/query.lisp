;;;
;;; Query connections
;;;
(in-package #:pl.tczew.transit)

(defun time-to-int (str)
  (check-type str string)
  (+ (* (parse-integer (subseq str 0 2)) 60)
     (parse-integer (subseq str 3 5))))


(defun load-graph ()
  (with-open-file (data "scrape/graph.txt" :direction :input)
    (flet ((time-table-to-int (time-table)
             (loop :for time :in time-table
                   :collect (cons
                             (time-to-int (car time))
                             (cdr time)))))

      (loop :with graph = (make-hash-table :test #'equal)
            :for station in (read data)
            :for station-hash = (al:ensure-gethash (car station)
                                                   graph
                                                   (make-hash-table :test #'equal))
            :do (loop :for departure in (cdr station)
                      :for departure-dest = (caar departure)
                      :for departure-bus-nr = (cdar departure)
                      :for time-table = (cdr departure)
                      :do (push (cons departure-bus-nr (time-table-to-int time-table))
                                (gethash departure-dest station-hash))
                      :finally (return graph))
            :finally (return graph)))))


(defun query (graph begining target time)
  (let ((start-time (time-to-int time))
        (is-visited (make-hash-table :test #'equal))
        (distance-to (make-hash-table :test #'equal))
        (trace (make-hash-table :test #'equal)))

    (labels ((is-unvisited (station)
               (not (gethash station is-visited)))
             (mark-visited (station)
               (setf (gethash station is-visited) t))
             (closest (dist)
               (loop :for k :being :the :hash-key
                       :using (hash-value v) :of dist
                     :with min-key = nil
                     :with min-val = nil
                     :if (and (is-unvisited k) (or (null min-val) (< v min-val)))
                       :do (setf min-key k
                                 min-val v)
                     :finally (return (cons min-key min-val))))
             (next-departure (time-table at-time)
               (car (remove-if-not (lambda (el)
                                     (> (car el) at-time))
                                   time-table)))
             (transit-cost (departure at-time)
               (+ (abs (- at-time (car departure)))
                  (cdr departure)))
             (update-distance (src dest distance)
               (when (> (gethash dest distance-to) distance)
                 (setf (gethash dest distance-to) distance)
                 (setf (gethash dest trace) src))))
      ;; setup distances
      (loop :for station :being :the :hash-key :in graph
            :for dist = (if (string= begining station)
                            0
                            most-positive-fixnum)
            :do (setf (gethash station distance-to) dist))

      (loop :repeat (hash-table-count graph)
            :for (station . distance) = (closest distance-to)
            :until (string= station target)
            :for now = (+ start-time distance)
            :for possible-transits = (gethash station graph)
            :do
               (loop :for destination :being :the :hash-key
                       :using (hash-value transit-lines) :of possible-transits
                     :when (is-unvisited destination)
                       :do
                          (loop :for line :in transit-lines
                                :for line-number = (car line)
                                :for departure = (next-departure (cdr line) now)
                                :for cost = (transit-cost departure now)
                                :do (update-distance station destination cost)))
            :do (mark-visited station)
            :finally (return trace))

      (loop :for prev = (gethash target trace) then (gethash prev trace)
            :while prev
            :collect prev :into path
            :finally (return (nreverse path))))))
