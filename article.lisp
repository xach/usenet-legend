;;;; articles.lisp

(in-package #:usenet-legend)

(defclass article ()
  ((message-id
    :initarg :message-id
    :accessor message-id)
   (headers
    :initarg :headers
    :accessor headers
    :documentation "A data structure for looking up header values by keyword.")
   (body
    :initarg :body
    :accessor body)
   (date
    :initarg :date
    :accessor date
    :reader time-stamp
    :documentation "A universal-time.")
   (complete-message
    :initarg :complete-message
    :accessor complete-message)))

(defun parse-headers (article-string)
  "Return an alist of headers in ARTICLE-STRING."
  (with-input-from-string (stream article-string)
    (let ((keyword nil)
          (value (make-string-output-stream))
          (table ()))
      (labels ((terminatorp (line)
                 (or (null line)
                     (equal line "")))
               (value-position (line)
                 (let ((split (position #\: line)))
                   (or (position-if-not #'whitespacep line :start (1+ split))
                       (length line))))
               (continuationp (line)
                 (whitespacep (char line 0)))
               (key (line)
                 (let* ((end (position #\: line))
                        (name (subseq line 0 end)))
                   (intern (string-upcase name) :keyword)))
               (ignorablep (line)
                 (string= line "From ..."))
               (extend-value (line)
                 (if (continuationp line)
                     (write-string line value
                                   :start (1- (or (position-if-not #'whitespacep
                                                                    line)
                                                  (length line))))
                     (write-string line value
                                   :start (value-position line))))
               (save ()
                 (setf table (acons keyword
                                    (get-output-stream-string value)
                                    table))
                 (setf keyword nil)))
        (loop
          (let ((line (read-line stream nil)))
            (cond ((terminatorp line)
                   (save)
                   (return table))
                  ((continuationp line)
                   (extend-value line))
                  ((ignorablep line))
                  (t
                   (when keyword
                     (save))
                   (setf keyword (key line))
                   (extend-value line)))))))))

(defmethod slot-unbound (class (article article) (slot (eql 'headers)))
  (setf (headers article) (parse-headers (complete-message article))))

(defgeneric header (keyword article)
  (:documentation "Return the header corresponding with KEYWORD in
  ARTICLE. If there is no such header, return NIL.")
  (:method (keyword article)
    (cdr (assoc keyword (headers article)))))

(defun quoted-printable-p (article)
  "Do the headers of ARTICLE indicate the body is encoded via
quoted-printable?"
  (equalp (header :content-transfer-encoding article)
          "quoted-printable"))

(defun decode-quoted-printable (string)
  "Convert quoted-printable encoded STRING to a decoded string."
  (with-output-to-string (output)
    (with-input-from-string (input string)
      (labels ((eat-quoted ()
                 (let ((next1 (read-char input)))
                   (unless (eql next1 #\Newline)
                     (let* ((next2 (read-char input))
                            (high (digit-char-p next1 16))
                            (low (digit-char-p next2 16))
                            (char (code-char (+ (ash high 4) low))))
                       (write-char char output))))))
        (loop for char = (read-char input nil)
              while char
              if (char= char #\=) do (eat-quoted)
              else do (write-char char output))))))

(defmethod slot-unbound (class (article article) (slot (eql 'date)))
  (setf (date article)
        (parse-date (or (header :nntp-posting-date article)
                        (header :date article)))))

(defmethod slot-unbound (class (article article) (slot (eql 'message-id)))
  (cl-ppcre:register-groups-bind (message-id)
      ("^(<.*?>)"
       (header :message-id article))
    (setf (message-id article) message-id)))

(defmethod print-object ((article article) stream)
  (print-unreadable-object (article stream :type t)
    (format stream "~D. ~S ~S"
            (id article)
            (header :date article)
            (header :subject article))))

(defun string-article (string)
  "Create an article object from STRING"
  (make-instance 'article :complete-message string))

(defun load-article (file)
  "Create an article object from the contents of FILE."
  (string-article (file-string file)))

(defgeneric subject (article)
  (:method ((article article))
    (header :subject article)))

(defun initialize-body (article)
  (let* ((text (complete-message article))
         (end-of-header (search (format nil "~%~%") text))
         (body (subseq text (+ 2 end-of-header))))
    (when (quoted-printable-p article)
      (setf body (decode-quoted-printable body)))
    (setf (body article) body)))

(defmethod slot-unbound (class (article article) (slot (eql 'body)))
  (initialize-body article))

(defmethod tokens ((article article))
  (tokens (list (subject article) (body article))))

(defun map-wild-articles (fun wild)
  (dolist (file (directory wild))
    (funcall fun (load-article file))))


