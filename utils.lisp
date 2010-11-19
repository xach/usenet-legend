;;;; utils.lisp

(in-package #:usenet-legend)

;;; types

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional size)
  `(simple-array (unsigned-byte 8) (,size)))

(deftype array-size ()
  `(integer 0 ,array-dimension-limit))

(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))

(deftype code-point ()
  `(integer 0 (,char-code-limit)))

;;; Misc

(defun make-string-table (&key case-sensitive)
  (let ((test (if case-sensitive 'equal 'equalp)))
    (make-hash-table :test test)))

(defun make-adjustable-vector (&key
                               (size 10) (element-type t))
  (make-array size :fill-pointer 0 :adjustable t :element-type element-type))

(defun touch-file (pathname)
  (open pathname :direction :probe :if-does-not-exist :create)
  (probe-file pathname))

(defun append-line (string pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (write-line string stream)))

;;; Octet vectors

(defun octet-vector (&rest octets)
  (make-array (length octets)
              :element-type 'octet
              :initial-contents octets))

(defun make-octet-vector (size &key (initial-element 0))
  (make-array size :element-type 'octet :initial-element initial-element))

(defun whitespacep (char)
  (member char '(#\Space #\Tab)))

(defun file-string (file)
  "Return the contents of FILE as a string."
  (with-open-file (stream file)
    (with-output-to-string (output)
      (loop for line = (read-line stream nil)
            while line do
            (write-line (string-right-trim '(#\Return) line) output)))))

(defun store-string (string file &key (if-exists :supersede))
  "Write STRING to FILE."
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (write-string string stream)))

(defun id-file-name (id)
  "Generate a string to use as a PATHNAME-NAME value for a given integer ID."
  (format nil "~6,'0X" id))

(defun table-alist (table)
  (let ((alist '()))
    (maphash (lambda (k v)
               (setf alist (acons k v alist)))
             table)
    alist))

(defun table-values (table)
  (let ((values '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v values))
             table)
    values))

(defun load-id-table (pathname)
  "Create a hash table where each line in PATHNAME is the key and its
  line number is the value. Used for initializing the token id table
  and the message id table."
  (let ((table (make-string-table)))
    (with-open-file (stream pathname)
      (loop for id from 0
            for key = (read-line stream nil)
            while key do (setf (gethash key table) id)))
    table))

;;;

(defparameter *alphabet*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "0123456789"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun random-string (length)
  "Return a random string with LENGTH characters."
  (let ((string (make-string length)))
    (map-into string (lambda ()
                       (aref *alphabet* (random (length *alphabet*)))))))

(defun temporary-pathname (&optional (template *default-pathname-defaults*))
  (merge-pathnames
   (make-pathname :name (concatenate 'string
                                     (or (pathname-name template)
                                         "x")
                                     "-"
                                     (random-string 8))
                  :defaults template)))

(defun call-with-temporary-open-file (template fun &rest open-args
                                      &key element-type external-format)
  "Call FUN with two arguments: an open output stream and a file
 name. When it returns, the file is deleted. TEMPLATE should be a
 pathname that can be used as a basis for the temporary file's
 location."
  (declare (ignorable element-type external-format))
  (let (try stream)
    (tagbody
     :retry
       (setf try (temporary-pathname template))
       (unwind-protect
            (progn
              (setf stream (apply #'open try
                                  :if-exists nil
                                  :direction :output
                                  open-args))
              (unless stream
                (go :retry))
              (funcall fun stream try))
         (when stream
           (close stream)
           (ignore-errors (delete-file try)))))))

;;; Counter files

(defun increment-counter-file (pathname)
  (with-binary-io (stream pathname
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (if (zerop (file-length stream))
        (write-u32 0 stream)
        (let ((value (read-u32 stream)))
          (file-position stream :start)
          (write-u32 (1+ value) stream)))))

;;; Dates & times

(defun pretty-date-string (universal-time)
  "Return UNIVERSAL-TIME formatted as an ISO 8601-ish string."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D"
            year month day)))

(defun pretty-time-string (universal-time)
  "Return UNIVERSAL-TIME formatted as an ISO 8601-ish string."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defgeneric time-stamp (object)
  (:method ((integer integer))
    integer)
  (:method ((object null))
    (get-universal-time))
  (:method ((object (eql t)))
    (get-universal-time))
  (:documentation
   "Return the universal-time timestamp of OBJECT."))

(defgeneric time-stamp-string (object)
  (:method (object)
    (pretty-time-string (time-stamp object))))

(macrolet ((time-stamp-readers (&rest names)
             `(progn
                ,@(loop for i from 0
                        for name in names
                        collect `(defgeneric ,name (object &key zone)
                                   (:method (object &key (zone 0))
                                     (nth-value
                                      ,i
                                      (decode-universal-time
                                       (time-stamp object) zone))))))))
  (time-stamp-readers sec minute hour day month year day daylightp zone))


(defun string-octets (string)
  (let ((output (make-adjustable-vector :size (length string)
                                        :element-type 'octet)))
    (flet ((save (octet)
             (vector-push-extend octet output)))
      (loop for char across string
            for char-code = (char-code char)
            do
            (cond ((< char-code 128)
                   (save char-code))
                  ((< char-code 2048)
                   (save (logior #b11000000 (ldb (byte 5 6) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 0) char-code))))
                  ((< char-code 65536)
                   (save (logior #b11100000 (ldb (byte 4 12) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 6) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 0) char-code))))
                  (t
                   (save (logior #b11110000 (ldb (byte 3 18) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 12) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 6) char-code)))
                   (save (logior #b10000000 (ldb (byte 6 0) char-code)))))))
    (coerce output 'octet-vector)))
