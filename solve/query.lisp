;;;
;;; Query connections
;;;
(in-package #:pl.tczew.transit)

(defun time-to-int (str)
  (check-type str string)
  (+ (* (parse-integer (subseq str 0 2)) 60)
     (parse-integer (subseq str 3 5))))

(defun int-to-time (minutes)
  (check-type minutes (integer 0 *))
  (format nil "~2,'0D:~2,'0D" (floor minutes 60) (mod minutes 60)))


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


(defun query (graph begining target time &key show)
  "Return the fastest route from BEGINNING to TARGET at given TIME in transit GRAPH."
  ;; Interesting queries:
  ;; Faster route than waiting at Dworzec PKP
  ;; > (query (load-graph) "Dworzec PKP" "Wigury" "09:30" :show t)
  ;; This one pioritizes transfer despite being the same arrive time at target
  ;; > (query (load-graph) "Czyżykowo" "Jedności Narodu" "09:30" :show t)
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
                                     (>= (car el) at-time))
                                   time-table)))
             (transit-cost (departure at-time)
               (+ (abs (- at-time (car departure)))
                  (cdr departure)
                  at-time)))
      ;; setup distances
      (loop :with beg-exists = nil
            :for station :being :the :hash-key :in graph
            :for dist = (if (string= begining station)
                            (progn (setf beg-exists t)
                                   start-time)
                            most-positive-fixnum)
            :do (setf (gethash station distance-to) dist)
            :finally (or beg-exists (error "Beginning station does not exist.")))
      ;; graph structure:
      ;; [station-name] = [possible-destinations] = ((transit-line-nr . time-table) ...)
      ;; time-table is (departure-time . travel-time)
      ;; looping our graph
      (loop :repeat (hash-table-count graph)
            :for (station . now) = (closest distance-to)
            :until (string= station target)
            :do
               (loop :for destination :being :the :hash-key
                       :using (hash-value transit-lines) :of (gethash station graph)
                     :when (is-unvisited destination)
                       :do
                          (loop :for line :in transit-lines
                                :for line-number = (car line)
                                :for departure = (next-departure (cdr line) now)
                                :for cost = (and departure (transit-cost departure now))
                                ;; found better route
                                :when (and departure (> (gethash destination distance-to) cost))
                                  :do (setf (gethash destination distance-to) cost)
                                      ;; update trace
                                      (setf (gethash destination trace)
                                            (list station line-number now departure cost))))
            :do (mark-visited station)
            :finally
               (or (and (not (string= station target))
                        (error "Target station not reached."))
                   (return trace)))
      ;; backtrace
      (loop :for (prev line now dep cost) = (gethash target trace):then (gethash prev trace)
            :while prev
            :collect (list prev
                           line
                           (int-to-time now)
                           (int-to-time (car dep))
                           (int-to-time cost))
              :into path
            :finally
               (when show

                 (format t "~&~{~{~20A nr ~2A ~* at ~A -> ~A~}~%~}" (reverse path)))
               (return (nreverse path))))))
