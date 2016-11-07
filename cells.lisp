(ql:quickload :uuid)

(defpackage :cells
  (:use :cl :uuid)
  (:export
   :cell :set-value :add-watcher :propagate :make-cell :connect
   :cell-value :cell-watchers :propagator :cond-connect :defconn))

(in-package :cells)

(defclass cell ()
  ((id :accessor cell-id
       :initform (uuid:make-v1-uuid))
   (value :accessor cell-value
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

(defun connect (fn &rest more-inputs)
  (let ((inputs (butlast more-inputs))
        (output (first (last more-inputs))))
    (dolist (i inputs)
      (add-watcher i #'(lambda ()
                         (let ((in-values (mapcar #'cell-value inputs)))
                           (if (some #'null in-values)
                               (set-value output nil)
                               (set-value output (apply fn in-values)))))))))

(defun propagator (fn &rest cells)
  (dolist (c cells)
    (add-watcher c fn)))

(defun cond-connect (pred-cell t-cell f-cell output)
  (propagator #'(lambda ()
                  (let ((pred-value (cell-value pred-cell)))
                    (if pred-value
                        (set-value output (cell-value t-cell))
                        (set-value output (cell-value f-cell)))))
              pred-cell
              t-cell
              f-cell))

(defun gen-cell-key (cells)
  (apply #'concatenate
         (cons 'string
               (mapcar #'(lambda (c) (format nil "~A" (cell-id c))) cells))))

(defmacro defconn (fname arg-list &body body)
  `(let ((h (make-hash-table :test 'equal)))
     (defun ,fname ,arg-list
       (let ((key (gensym)))
         (setq key (gen-cell-key (list ,@arg-list)))
         (if (gethash key h)
             'done
             (progn
               (setf (gethash key h) t)
               (progn
                 ,@body)))))))
