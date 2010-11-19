;;;; serialization.lisp

(in-package #:usenet-legend)

(defconstant +record-index-octets+ 4)

(defgeneric store (object sink)
  (:method (object (sink pathname))
    (with-binary-output (stream sink)
      (store object stream)))
  (:method (object (sink string))
    (store object (pathname sink))))

(defgeneric restore (object source)
  (:method ((class-name symbol) source)
    (restore (make-instance class-name) source))
  (:method (object (pathname pathname))
    (with-binary-input (stream pathname)
      (restore object stream)))
  (:method (object (string string))
    (restore object (pathname string))))

(defgeneric index-file-pathname (source)
  (:method ((source pathname))
    (make-pathname :type "idx"
                   :defaults source))
  (:method (source)
    (index-file-pathname (pathname source))))

(defgeneric record-file-position (record-id source)
  (:method (record-id source)
    (let ((index-file (index-file-pathname source)))
      (with-binary-input (stream index-file)
        (file-position stream (* record-id +record-index-octets+))
        (read-u32 stream)))))

(defgeneric seek-to-record (record-id source)
  (:method (record-id (stream stream))
    (file-position stream (record-file-position record-id stream))))

(defgeneric restore-record-id (object id source)
  (:method (object id (source stream))
    (seek-to-record id source)
    (restore object source))
  (:method (object id (source pathname))
    (with-binary-input (stream source)
      (restore-record-id object id stream)))
  (:method (object id (source string))
    (restore-record-id object id (pathname source))))


;;; Miscellaneous

(defmethod restore ((object (eql 'record-hash-table)) source)
  (let ((table (make-hash-table :test 'equal))
        (id 0))
    (with-open-file (stream source)
      (loop for key = (read-line stream nil)
            while key do
            (setf (gethash key table) id)
            (incf id))
      table)))

(defmethod store ((vector vector) (sink stream))
  (etypecase vector
    (octet-vector
     (write-sequence vector sink))
    (u32-vector
     (write-u32-vector vector sink))))

(defmethod store ((string string) (sink pathname))
  (with-open-file (stream sink
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string string stream)))
