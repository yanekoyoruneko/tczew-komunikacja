;;;
;;; Functions for building the graph represented by alist of lists
;;;
(in-package #:pl.tczew.transit)

(defparameter *stations* nil)

(defun find-station (name)
  (assoc name *stations* :test 'equal))

(defun get-connections (from &key to nr)
  (if (or to nr)
      (remove-if-not (lambda (con)
                       (and
                        (or (not to) (string= to (caar con)))
                        (or (not nr) (= nr (cdar con)))))
                     (cdr (find-station from)))
      (find-station from)))

(defun add-connection (connection)
  (check-type connection connection)
  (when (not (find-station (start-station connection)))
    (setf *stations* (acons (start-station connection) '() *stations*)))
  (push `(,(transit-line connection) . ,(time-table connection))
        (cdr (find-station (start-station connection)))))

(defun time-to-int (str)
  (check-type str string)
  (+ (* (parse-integer (subseq str 0 2)) 60)
     (parse-integer (subseq str 3 5))))

(defun count-connections ()
  (loop for station in *stations*
        for connections = (cdr station) do
          (loop for con in connections
                for to = (caar con)
                for nr = (cdar con)
                do (print (car station))
                   (format t "=> ~a -> ~a"  (car con) (length (get-connections to :nr nr))))))
