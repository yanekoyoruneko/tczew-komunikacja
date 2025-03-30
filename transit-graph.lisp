(in-package #:tczew-transit)

(defparameter *stations* nil)

(defun find-station (name)
  (assoc name *stations* :test 'equal))

(defun get-connections (from &key to nr)
  (if (or to nr)
      (remove-if-not (lambda (con) (and
			       (or (not to) (string= to (caar con)))
			       (or (not nr) (= nr (cdar con)))))
		     (cdr (find-station from)))
      (find-station from)))

(defun new-station (name)
  (setf *stations* (acons name '() *stations*)))

(defun add-connection (connection)
  (when (not (find-station (start-station connection)))
    (new-station (start-station connection)))
  (push `(,(transit-line connection) . ,(time-table connection))
	(cdr (find-station (start-station connection)))))

(defun weight-transit-cost ()
  (loop for station in *stations*
	for connections = (cdr station) do
	  (loop for con in connections
		for to = (caar con)
		for nr = (cdar con)
		do
		   (print (car station))
		   (format t "=> ~a -> ~a"  (car con) (length (get-connections to :nr nr))))))
