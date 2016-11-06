(defpackage :cells
  (:use :cl)
  (:export :cell :set-value :add-watcher :propagate :make-cell :connect :defconnection))

(in-package :cells)

(defclass cell ()
  ((value :accessor cell-value
          :initarg :value
          :initform nil)
   (watchers :accessor cell-watchers
             :initform nil)))

(defmethod set-value ((c cell) v)
  (setf (cell-value c) v)
  (propagate c))

(defmethod add-watcher ((c cell) w)
  (push w (cell-watchers c))
  (propagate c))

(defmethod propagate ((c cell))
  (dolist (w (cell-watchers c))
    (funcall w)))

(defun make-cell (&key (value nil))
  (make-instance 'cell :value value))

(defun connect (fn output input &rest more-inputs)
  (let ((inputs (cons input more-inputs)))
    (dolist (i inputs)
      (add-watcher i #'(lambda ()
                         (let ((in-values (mapcar #'cell-value inputs)))
                           (if (some null in-values)
                               (set-value output nil)
                               (set-value output (apply fn in-values)))))))))

(defmacro defconnection (name arg-list &body body)
  `(let ((once nil))
     (defun ,name ,arg-list
       (when (not once)
         (setq once t)
         (progn
           ,@body)))))
