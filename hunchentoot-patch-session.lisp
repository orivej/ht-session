(in-package hunchentoot)

(export '(%session-value session-value*
          %delete-session-value delete-session-value*))

(defgeneric %session-value (symbol session)
  (:method (symbol session)
    (session-value symbol session)))
(defgeneric (setf %session-value) (value symbol session request)
  (:method (value symbol session request)
    (setf (session-value symbol session) value)))
(defgeneric %delete-session-value (symbol session)
  (:method (symbol session)
    (delete-session-value symbol session)))

(defun session-value* (symbol &optional (session *session*))
  (%session-value symbol session))
(defun (setf session-value*) (value symbol &optional (session *session*))
  (log:i session *request*)
  (setf (%session-value symbol session *request*) value))
(defun delete-session-value* (symbol &optional (session *session*))
  (%delete-session-value symbol session))
