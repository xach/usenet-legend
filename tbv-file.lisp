;;;; tbv-file.lisp

(in-package #:usenet-legend)

;;; term bit-vector file management

(defclass tbv-file-header ()
  ((term-count
    :initarg :term-count
    :accessor term-count)
   (bits-available
    :initarg :bits-available
    :accessor bits-available
    :documentation "The number of space available for article bits.")
   (bits-used
    :initarg :bits-used
    :accessor bits-used
    :documentation "The number of bits actually used in the available space."))
  (:default-initargs
   :term-count 0
   :bits-available 1024
   :bits-used 0))

(defgeneric buffer-octet-count (header)
  (:method ((header tbv-file-header))
    (/ (bits-available header) 8)))

(defgeneric buffer-octets (header)
  (:method (header)
    (make-octet-vector (buffer-octet-count header))))

(defvar +tbv-file-header-size+ (* 4 3))

(defmethod store ((object tbv-file-header) (stream stream))
  (write-u32 (term-count object) stream)
  (write-u32 (bits-available object) stream)
  (write-u32 (bits-used object) stream))

(defmethod restore ((object tbv-file-header) (stream stream))
  (setf (term-count object) (read-u32 stream)
        (bits-available object) (read-u32 stream)
        (bits-used object) (read-u32 stream))
  object)

(defun grow-tbv-file (file)
  (call-with-temporary-open-file
   file
   (lambda (output pathname)
     (with-binary-input (input file)
       (let* ((header (restore 'tbv-file-header input))
              (buffer (buffer-octets header))
              (padding (buffer-octets header)))
         (setf (bits-available header) (* (bits-available header) 2))
         (store header output)
         (dotimes (i (term-count header))
           (read-sequence buffer input)
           (write-sequence buffer output)
           (write-sequence padding output)))
       (rename-file pathname file)))
   :element-type 'octet))

(defun add-term-space-to-tbv-file (file &optional (n 1))
  (with-binary-io (stream file)
    (let* ((header (restore 'tbv-file-header stream))
           (padding (buffer-octets header)))
      (file-position stream :end)
      (dotimes (i n)
        (write-sequence padding stream))
      (file-position stream :start)
      (incf (term-count header) n)
      (store header stream)))
  t)

(defun term-bit-vector-position (term-id header)
  (+ +tbv-file-header-size+
     (* term-id (buffer-octet-count header))))

(defun set-article-bit (article-id term-id header stream)
  (multiple-value-bind (index bit)
      (floor article-id 8)
    (let ((position (+ (term-bit-vector-position term-id header)
                       index)))
      (file-position stream position)
      (let ((octet (read-byte stream)))
        (setf (ldb (byte 1 bit) octet) 1)
        (file-position stream position)
        (write-byte octet stream)))))

(defun add-article-to-tbv-file (article-id term-ids file)
  (with-binary-io (stream file)
    (let ((header (restore 'tbv-file-header stream)))
      (assert (<= article-id (bits-used header)))
      (map nil (lambda (term-id)
                 (set-article-bit article-id term-id header stream))
           term-ids))))

(defun increase-bits-used (file)
  (let ((header (restore 'tbv-file-header file)))
    (when (= (bits-used header) (bits-available header))
      (grow-tbv-file file)
      (restore header file))
    (incf (bits-used header))
    (with-binary-output (stream file :if-exists :overwrite)
      (store header stream))))

(defun load-term-bit-vector (term-id file)
  (with-binary-input (stream file)
    (let* ((header (restore 'tbv-file-header stream))
           (position (term-bit-vector-position term-id header)))
      (file-position stream position)
      (read-packed-bits stream (bits-used header)))))
