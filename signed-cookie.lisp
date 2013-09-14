(in-package ht-session)

(defconstant +hash-size+ 32)
(defconstant +encoded-hash-size+ (* 5/4 +hash-size+))

(defvar *signing-key*)

(defun randomize-signing-key ()
  (setf *signing-key*
        (map-into (make-array +hash-size+ :element-type '(unsigned-byte 8))
                  (lambda () (random 256)))))

(defun signature (string &key (start 0))
  (unless (boundp '*signing-key*)
    (log-message* :warn "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
    (randomize-signing-key))
  (let ((digest (make-digest 'sha256)))
    (update-digest digest (babel:string-to-octets string :start start))
    (update-digest digest *signing-key*)
    (binascii:encode-z85 (produce-digest digest))))

(defun sign-cookie-value (string)
  (let ((safe-content (binascii:encode-z85 (babel:string-to-octets string))))
    (concatenate 'string (signature safe-content) safe-content)))

(defun authentic-cookie-value? (string)
  (and string
       (stringp string)
       (>= (length string) +encoded-hash-size+)
       (string= (signature string :start +encoded-hash-size+)
                string :end2 +encoded-hash-size+)))

(defun decode-signed-cookie-value (string)
  (when (authentic-cookie-value? string)
    (babel:octets-to-string (binascii:decode-z85 string :start +encoded-hash-size+))))