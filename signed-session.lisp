(in-package ht-session)

(defclass signed-session-request-mixin () ())

(defclass signed-session-request (signed-session-request-mixin request) ())

(defclass signed-session ()
  ((session-data :initarg :session-data
                 :initform (make-hash-table)
                 :accessor session-data)
   (max-time :initarg :max-time
             :initform *session-max-time*
             :accessor session-max-time
             :type fixnum
             :documentation "The time \(in seconds) after which this
session expires if it's not used.")))

(defmethod session-verify ((request signed-session-request-mixin))
  (let ((session-string (decode-signed-cookie-value
                         (or (cookie-in (session-cookie-name *acceptor*) request)
                             (get-parameter (session-cookie-name *acceptor*) request)))))
    (when session-string
      (let ((session-list (read-from-string session-string)))
        (when (<= (get-universal-time) (first session-list))
          (make-instance 'signed-session :session-data (plist-hash-table(rest session-list))))))))

(defmethod session-cookie-value ((session signed-session))
  (let ((*print-pretty* nil))
    (sign-cookie-value
     (prin1-to-string
      (list* (+ (get-universal-time) (session-max-time session))
             (hash-table-plist (session-data session)))))))

(defmethod handle-request :after (acceptor (request signed-session-request-mixin))
  (when *session*
    (set-cookie (session-cookie-name acceptor)
                :value (session-cookie-value *session*)
                :path "/"
                :max-age (session-max-time *session*))))

(defun signed-session-value (symbol)
  (when *session*
    (gethash symbol (session-data *session*))))

(defun (setf signed-session-value) (value symbol)
  (unless *session*
    (setf *session* (make-instance 'signed-session)))
  (setf (gethash symbol (session-data *session*)) value))

(defun delete-signed-session-value (symbol)
  (when *session*
    (remhash symbol (session-data *session*))))
