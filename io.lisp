;;;; io.lisp

(in-package #:usenet-legend)

(deftype u32 ()
  '(unsigned-byte 32))

(deftype u32-vector (&optional size)
  `(simple-array u32 (,size)))

(declaim (inline assemble-u32 disassemble-u32 get-u32 read-u32 write-u32))

(defmacro with-binary-output ((stream file
                                      &rest options
                                      &key (if-exists :supersede))
                              &body body)
  (declare (ignorable if-exists))
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists ,if-exists
                            :element-type 'octet
                            ,@options)
     ,@body))

(defmacro with-binary-io ((stream file
                                      &rest options
                                      &key (if-exists :overwrite)
                                      &allow-other-keys)
                              &body body)
  (declare (ignorable if-exists))
  `(with-open-file (,stream ,file
                            :direction :io
                            :if-exists ,if-exists
                            :element-type 'octet
                            ,@options)
     ,@body))

(defmacro with-binary-input ((stream file) &body body)
  `(with-open-file (,stream ,file :element-type 'octet)
     ,@body))



(defun assemble-u32 (b1 b2 b3 b4)
  (declare (type octet b1 b2 b3 b4)
           (optimize speed))
  (logand #xFFFFFFFF
          (logior (ash b1 24)
                  (ash b2 16)
                  (ash b3 8)
                  (ash b4 0))))

(defun disassemble-u32 (u32)
  (declare (type u32 u32)
           (optimize speed))
  (let ((b1 (ldb (byte 8 24) u32))
        (b2 (ldb (byte 8 16) u32))
        (b3 (ldb (byte 8  8) u32))
        (b4 (ldb (byte 8  0) u32)))
    (values b1 b2 b3 b4)))

(defun read-u32 (stream)
  (let ((b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream))
        (b4 (read-byte stream)))
    (assemble-u32 b1 b2 b3 b4)))

(defun write-u32 (u32 stream)
  (declare (type u32 u32))
  (multiple-value-bind (b1 b2 b3 b4)
      (disassemble-u32 u32)
    (write-byte b1 stream)
    (write-byte b2 stream)
    (write-byte b3 stream)
    (write-byte b4 stream)
    u32))

(defun get-u32 (octet-vector offset)
  (declare (optimize speed)
           (type octet-vector octet-vector)
           (type u32 offset))
  (let ((b1 (aref octet-vector (+ offset 0)))
        (b2 (aref octet-vector (+ offset 1)))
        (b3 (aref octet-vector (+ offset 2)))
        (b4 (aref octet-vector (+ offset 3))))
    (assemble-u32 b1 b2 b3 b4)))

(defun (setf get-u32) (u32 octet-vector offset)
  (declare (optimize speed)
           (type u32 u32)
           (type tid offset)
           (type octet-vector octet-vector))
  (multiple-value-bind (b1 b2 b3 b4)
      (disassemble-u32 u32)
    (setf (aref octet-vector (+ offset 0)) b1
          (aref octet-vector (+ offset 1)) b2
          (aref octet-vector (+ offset 2)) b3
          (aref octet-vector (+ offset 3)) b4)
    u32))

(defun write-u32-vector (u32-vector stream)
  (let* ((size (length u32-vector))
         (octets (make-octet-vector (* size 4))))
    (loop for i from 0 by 4
          for u32 across u32-vector
          do (setf (get-u32 octets i) u32))
    (write-sequence octets stream)
    size))

(defun read-u32-vector (u32-vector stream)
  (let* ((size (length u32-vector))
         (octets (make-octet-vector (* size 4))))
    (read-sequence octets stream)
    (loop for i from 0 by 4 below (length octets)
          for j below size
          do (setf (aref u32-vector j) (get-u32 octets i)))
    u32-vector))

(defun u32-vector-octets (u32-vector)
  (let* ((size (length u32-vector))
         (octets (make-octet-vector (* size 4))))
    (loop for i from 0 by 4
          for u32 across u32-vector
          do (setf (get-u32 octets i) u32))
    octets))

(defun all-octets (&rest u32-vectors)
  (apply #'concatenate 'octet-vector
         (mapcar #'u32-vector-octets u32-vectors)))

;;; Saving/loading bit vectors in a compact on-disk format.

(defun make-bit-vector (count &key (initial-element 0))
  "Create a bit-vector with COUNT elements."
  (make-array count :element-type 'bit :initial-element initial-element))

(defun bit-vector-octets (bv)
  (declare (type simple-bit-vector bv)
           (optimize speed))
  (let ((octets (make-array (ceiling (length bv) 8)
                            :element-type 'octet
                            :initial-element 0)))
    (loop for bit across bv
          for i from 0 below (length bv)
          do (multiple-value-bind (j k)
                 (floor i 8)
               (setf (aref octets j)
                     (logior (ash bit k) (aref octets j)))))
    (values octets
            (length bv))))

(defun octets-bit-vector (octets size)
  (declare (type array-size size)
           (type octet-vector octets))
  (let ((bv (make-bit-vector size)))
    (loop for i below size
          do (multiple-value-bind (j k)
                 (floor i 8)
               (setf (sbit bv i)
                     (ldb (byte 1 k) (aref octets j)))))

    bv))

(defun write-packed-bits (bit-vector stream)
  (write-sequence (bit-vector-octets bit-vector) stream))

(defun read-packed-bits (stream size)
  (let ((octets (make-array (ceiling size 8)
                            :element-type '(unsigned-byte 8))))
    (read-sequence octets stream)
    (octets-bit-vector octets size)))


(defun append-u32 (u32 file)
  (with-binary-output (stream file
                              :if-exists :append)
    (write-u32 u32 stream)))

(defun read-u32-record (record-id file)
  (with-binary-input (stream file)
    (file-position stream (* record-id 4))
    (read-u32 stream)))

(defun file-u32-record-count (pathname)
  (with-binary-input (stream pathname)
    (/ (file-length stream) 4)))
