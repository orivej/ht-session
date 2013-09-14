;;; z85.lisp -- z85 encoding according to http://rfc.zeromq.org/spec:32

;;; This file is a copy of base85.lisp from the official distribution of
;;; binascii with base85 substituted for z85 and encode-table replaced

;;; Z85-encoded string constitutes a correct cookie value, unlike Base85
;;; which has a semicolon

;;; Licensed under the terms of BSD 3-clause:

;; Copyright (c) 2009, Nathan Froyd

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; * Neither the name of the copyright holders nor the names of
;;   contributors to this software may be used to endorse or promote
;;   products derived from this software without specific prior written
;;   permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:in-package :binascii)

(defvar *z85-encode-table*
  #.(coerce "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#" 'simple-base-string))

(defstruct (z85-encode-state
             (:include encode-state)
             (:copier nil)
             (:predicate nil)
             (:constructor make-z85-encode-state
                           (&aux (descriptor (z85-format-descriptor)))))
  ;; TODO: Clever hack for little-endian machines: fill in GROUP
  ;; back-to-front, using PENDING to count down, then use SBCL's
  ;; %VECTOR-RAW-BITS or similar to read out the group in proper
  ;; big-endian order.  We could even do the same thing on x86-64 if we
  ;; made the buffer bigger.
  ;;
  ;; For now, though, we'll fill GROUP front-to-back and PENDING will
  ;; indicate how many octets we've filled in.
  #+nil
  (group (make-array 4 :element-type '(unsigned-byte 8))
         :read-only t :type (simple-array (unsigned-byte 8) (4)))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 4))
  (output-group (make-array 5 :element-type 'base-char)
                :read-only t :type (simple-array base-char (5)))
  (output-pending 0 :type (integer 0 5))
  (table *z85-encode-table* :read-only t :type (simple-array base-char (85))))

(defun encoded-length-z85 (count)
  "Return the number of characters required to encode COUNT octets in Base85."
  (* (ceiling count 4) 5))

(declaim (inline z85-encode))
(defun z85-encoder (state output input
                       output-index output-end
                       input-index input-end lastp converter)
  (declare (type z85-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (z85-encode-state-bits state))
        (pending (z85-encode-state-pending state))
        (output-group (z85-encode-state-output-group state))
        (output-pending (z85-encode-state-output-pending state))
        (table (z85-encode-state-table state)))
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 4) pending))
    (declare (type (integer 0 5) output-pending))
    (flet ((expand-for-output (bits output-group)
             (loop for i from 0 to 4
                do (multiple-value-bind (b index) (truncate bits 85)
                     (setf bits b
                           (aref output-group i) (aref table index)))
                finally (setf output-pending 5))))
      (declare (inline expand-for-output))
      (tagbody
       PAD-CHECK
         (when (z85-encode-state-finished-input-p state)
           (go FLUSH-BITS))
       INPUT-CHECK
         (when (>= input-index input-end)
           (go DONE))
       DO-INPUT
         (when (< pending 4)
           (setf bits (ldb (byte 32 0)
                           (logior (ash bits 8) (aref input input-index))))
           (incf input-index)
           (unless (= (incf pending) 4)
             (go INPUT-CHECK)))
       EXPAND-FOR-OUTPUT
         (expand-for-output bits output-group)
       OUTPUT-CHECK
         (when (>= output-index output-end)
           (go DONE))
       DO-OUTPUT
         (when (> output-pending 0)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group (decf output-pending))))
           (incf output-index)
           (cond
             ((zerop output-pending)
              (setf bits 0)
              (setf pending 0)
              (go INPUT-CHECK))
             (t
              (go OUTPUT-CHECK))))
       DONE
         (unless lastp
           (go RESTORE-STATE))
         (setf (z85-encode-state-finished-input-p state) t)
         ;; Make it appear as though the input were padded with zeros to a
         ;; full input group.
         (let ((for-pad (- 4 pending)))
           (setf bits (ldb (byte 32 0) (ash bits (* 8 for-pad))))
           (setf pending 4)
           (expand-for-output bits output-group))
       FLUSH-BITS
         (when (zerop output-pending)
           (go RESTORE-STATE))
       FLUSH-OUTPUT-CHECK
         (when (>= output-index output-end)
           (go RESTORE-STATE))
       DO-FLUSH-OUTPUT
         (when (> output-pending 0)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group (decf output-pending))))
           (incf output-index)
           (cond
             ((zerop output-pending)
              (setf bits 0)
              (setf pending 0)
              (go RESTORE-STATE))
             (t
              (go FLUSH-OUTPUT-CHECK))))
       RESTORE-STATE
         (setf (z85-encode-state-bits state) bits
               (z85-encode-state-pending state) pending
               (z85-encode-state-output-pending state) output-pending))
      (values input-index output-index))))

(defvar *z85-decode-table* (make-decode-table *z85-encode-table*))
(declaim (type decode-table *z85-decode-table*))

(defstruct (z85-decode-state
             (:include decode-state)
             (:copier nil)
             (:predicate nil)
             (:constructor %make-z85-decode-state
                           (&aux (descriptor (z85-format-descriptor)))))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 5))
  (output-pending 0 :type (integer 0 4))
  (table *z85-decode-table* :read-only t :type decode-table))

(defun make-z85-decode-state (case-fold map01)
  (declare (ignore case-fold map01))
  (%make-z85-decode-state))

(defun z85-decoder (state output input
                       output-index output-end
                       input-index input-end lastp converter)
  (declare (type z85-decode-state state))
  (declare (type simple-octet-vector output))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (z85-decode-state-bits state))
        (pending (z85-decode-state-pending state))
        (output-pending (z85-decode-state-output-pending state))
        (table (z85-decode-state-table state)))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 5) pending))
    (declare (type (integer 0 4) output-pending))
    (tagbody
     FINISHED-CHECK
       (when (z85-decode-state-finished-input-p state)
         (go FLUSH-BITS))
     OUTPUT-AVAILABLE-CHECK
       (when (zerop output-pending)
         (go INPUT-AVAILABLE-CHECK))
     OUTPUT-SPACE-CHECK
       (when (>= output-index output-end)
         (go DONE))
     DO-OUTPUT
       (setf (aref output output-index)
             (ldb (byte 8 (* (decf output-pending) 8)) bits))
       (incf output-index)
       (cond
         ((zerop output-pending)
          (setf bits 0)
          (setf pending 0)
          (setf output-pending 0)
          (go INPUT-AVAILABLE-CHECK))
         (t
          (go OUTPUT-SPACE-CHECK)))
     INPUT-AVAILABLE-CHECK
       (when (>= input-index input-end)
         (go DONE))
     DO-INPUT
       (cond
         ((< pending 5)
          (let* ((c (aref input input-index))
                 (v (funcall converter c))
                 (d (dtref table v)))
            (when (= d +dt-invalid+)
              (error "invalid z85 character ~A at position ~D" c input-index))
            ;; FIXME: check for overflow.
            (setf bits (+ (* bits 85) d))
            (incf pending)
            (incf input-index)
            (go INPUT-AVAILABLE-CHECK)))
         (t
          (setf output-pending 4)
          (go OUTPUT-SPACE-CHECK)))
     DONE
       (unless lastp
         (go RESTORE-STATE))
       (setf (z85-decode-state-finished-input-p state) t)
       ;; We should *always* have a complete group or nothing at this
       ;; point.
     EOT-VALIDITY-CHECK
       (when (<= 1 pending 4)
         (error "invalid z85 input"))
       (setf output-pending (if (zerop pending) 0 4))
     FLUSH-BITS
       (when (zerop output-pending)
         (go RESTORE-STATE))
     FLUSH-OUTPUT-CHECK
       (when (>= output-index output-end)
         (go RESTORE-STATE))
     DO-FLUSH-OUTPUT
       (when (> output-pending 0)
         (setf (aref output output-index)
               (ldb (byte 8 (* (decf output-pending) 8)) bits))
         (incf output-index)
         (cond
           ((zerop output-pending)
            (setf bits 0)
            (setf pending 0)
            (setf output-pending 0)
            (go RESTORE-STATE))
           (t
            (go FLUSH-OUTPUT-CHECK))))
     RESTORE-STATE
       (setf (z85-decode-state-bits state) bits
             (z85-decode-state-pending state) pending
             (z85-decode-state-output-pending state) output-pending))
    (values input-index output-index)))

(defun decoded-length-z85 (length)
  (multiple-value-bind (n-groups rem) (truncate length 5)
    (unless (zerop rem)
      (error "z85 input length ~D must be a multiple of 5" length))
    (* n-groups 4)))

(define-format :z85
  :encode-state-maker make-z85-encode-state
  :decode-state-maker make-z85-decode-state
  :encode-length-fun encoded-length-z85
  :decode-length-fun decoded-length-z85
  :encoder-fun z85-encoder
  :decoder-fun z85-decoder)
