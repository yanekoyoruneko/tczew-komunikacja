(in-package #:tczew-transit)

(defparameter *stations* nil)

(defun find-station (name)
  (assoc name *stations* :test 'equal))

(defun get-connections (from &optional to)
  (if to
      (remove-if-not (lambda (con) (string= to (caar con))) (cdr (find-station from)))
      (find-station from)))

(defun new-station (name)
  (setf *stations* (acons name '() *stations*)))

(defun add-connection (connection)
  (when (not (find-station (start-station connection)))
    (new-station (start-station connection)))
  (push `(,(transit-line connection) . ,(time-table connection))
	(cdr (find-station (start-station connection)))))

(defun weight-transit-cost ()
  )
