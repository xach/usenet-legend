;;;; corpus.lisp

(in-package #:usenet-legend)

(defclass term ()
  ((id
    :initarg :id
    :accessor id)
   (name
    :initarg :name
    :accessor name)
   (corpus
    :initarg :corpus
    :accessor corpus))
  (:documentation "A TERM is a single word in an article. Terms are
  interned in a corpus; terms with identical NAME values have
  identical ID values."))

(defmethod print-object ((term term) stream)
  (print-unreadable-object (term stream :type t)
    (format stream "~S ~D" (name term) (id term))))

;;; Corpus articles

(defclass corpus-article (article)
  ((id
    :initarg :id
    :accessor id)
   (corpus
    :initarg :corpus
    :accessor corpus))
  (:documentation "A corpus article has a corpus-specific ID and a
  reference to its containing corpus."))

(defgeneric metadata (object)
  (:method (article)
    (list :message-id (message-id article)
          :date (date article)
          :id (id article)
          :subject (subject article))))

;;; The corpus itself

(defclass corpus ()
  ((storage-pathname
    :initarg :storage-pathname
    :accessor storage-pathname
    :documentation "This pathname is used as the base directory for
    article files and metadata about the corpus.")
   (article-count
    :initarg :article-count
    :accessor article-count
    :documentation "The number of articles in the corpus.")
   (message-id-article-id-map
    :initarg :message-id-article-id-map
    :accessor message-id-article-id-map
    :documentation "A table mapping message-ids to article ids.")
   (dates
    :initarg :dates
    :accessor dates
    :documentation "A vector of timestamps; not universal-times, just
    used for ordering purposes.")
   (terms
    :initarg :terms
    :accessor terms
    :documentation "A vector of terms, indexed by term-id.")
   (token-term-id-map
    :initarg :token-term-id-map
    :accessor token-term-id-map
    :documentation "A table mapping a string token to a term-id."))
  (:default-initargs
   :storage-pathname (error "A storage pathname is required.")
   :article-count 0
   :message-ids (make-string-table)
   :dates (make-adjustable-vector)
   :terms (make-adjustable-vector)
   :message-id-article-id-map (make-string-table)
   :token-term-id-map (make-string-table)))

(defmethod print-object ((corpus corpus) stream)
  (print-unreadable-object (corpus stream :type t :identity t)
    (format stream "~S (~D article~:P, ~D term~:P)"
            (storage-pathname corpus)
            (article-count corpus)
            (length (terms corpus)))))

(defgeneric date-order-article-ids (corpus)
  (:method (corpus)
    (let ((ids (make-array (article-count corpus)))
          (dates (dates corpus)))
      (loop for id from 0 below (length ids)
            do (setf (aref ids id) id))
      (sort ids #'< :key (lambda (id)
                           (aref dates id))))))

(defun load-u32-records (pathname)
  (with-binary-input (stream pathname)
    (let ((data (make-array (/ (file-length stream) 4) :element-type 'u32)))
      (read-u32-vector data stream))))

(defun load-corpus (storage-pathname)
  (let* ((corpus (make-instance 'corpus :storage-pathname storage-pathname)))
    (setf (article-count corpus)
          (persistent-article-count corpus))
    (setf (dates corpus)
          (load-u32-records (dates-pathname corpus)))
    corpus))

(defun corpus-relative-pathname (pathname corpus)
  (merge-pathnames pathname (storage-pathname corpus)))

(macrolet ((define-pathname-reader (name relative-pathname)
             `(defgeneric ,name (corpus)
                (:method (corpus)
                  (corpus-relative-pathname ,relative-pathname corpus)))))
  (define-pathname-reader tbv-pathname "tbv.dat")
  (define-pathname-reader term-pathname "terms.txt")
  (define-pathname-reader message-id-pathname "mids.txt")
  (define-pathname-reader psi-pathname "psi.dat")
  (define-pathname-reader article-counter-pathname "article-count.dat")
  (define-pathname-reader psi-index-pathname "psi.idx")
  (define-pathname-reader dates-pathname "dates.idx")
  (define-pathname-reader term-cdb-pathname "terms.cdb"))

(defun initialize-corpus-pathname (corpus)
  (let ((header (make-instance 'tbv-file-header)))
    (ensure-directories-exist (corpus-relative-pathname "articles/" corpus))
    (ensure-directories-exist (corpus-relative-pathname "metadata/" corpus))
    (increment-counter-file (article-counter-pathname corpus))
    (store header (tbv-pathname corpus))
    (dolist (fun (list #'term-pathname #'message-id-pathname
                       #'psi-pathname #'psi-index-pathname
                       #'dates-pathname))
      (touch-file (funcall fun corpus)))))

(defmethod initialize-instance :around ((corpus corpus) &key
                                        storage-pathname create
                                        &allow-other-keys)
  (setf corpus (call-next-method))
  (unless (probe-file storage-pathname)
    (if create
        (initialize-corpus-pathname corpus)
        (error "Corpus pathname ~S does not exist" storage-pathname)))
  corpus)

;;; Stub articles can be quickly loaded without extra parsing

(defclass stub-article ()
  ((id
    :initarg :id
    :accessor id)
   (subject
    :initarg :subject
    :accessor subject)
   (message-id
    :initarg :message-id
    :accessor message-id)
   (date
    :initarg :date
    :accessor date)))

(defmethod print-object ((article stub-article) stream)
  (print-unreadable-object (article stream :type t)
    (format stream "~D. ~S @~A"
            (id article)
            (subject article)
            (pretty-date-string (date article)))))

(defgeneric stub-storage-pathname (article)
  (:method (article)
    (corpus-relative-pathname
     (make-pathname :name (id-file-name (id article))
                    :type "sexp"
                    :directory '(:relative "metadata"))
     (corpus article))))

(defun stub-version (article)
  (apply #'make-instance 'stub-article (metadata article)))

(defmethod restore ((article stub-article) (source pathname))
  (with-open-file (stream source)
    (let ((args (read stream)))
      (apply #'reinitialize-instance article args))))

(defmethod restore-record-id ((class (eql 'stub-article))
                              id
                              (corpus corpus))
  (let ((pathname (corpus-relative-pathname
                   (make-pathname :name (id-file-name id)
                                  :type "sexp"
                                  :directory '(:relative "metadata"))
                   corpus)))
    (with-open-file (stream pathname)
      (let ((initargs (read stream)))
        (apply #'make-instance 'stub-article initargs)))))

(defmethod store ((article stub-article) (sink pathname))
  (with-open-file (stream sink
                          :direction :output
                          :if-exists :supersede)
    (write (metadata article) :stream stream :pretty nil :escape t)
    article))

;;; Finding & creating terms in a corpus

(defgeneric find-term (token lexicon))
(defgeneric add-term (token lexicon))
(defgeneric ensure-term (token lexicon))

(defmethod ensure-term (token lexicon)
  (or (find-term token lexicon)
      (add-term token lexicon)))

(defmethod find-term (token (corpus corpus))
  (let ((id (gethash token (token-term-id-map corpus))))
    (when id
      (aref (terms corpus) id))))

(defmethod add-term (token (corpus corpus))
  (let* ((terms (terms corpus))
         (id (vector-push-extend t terms))
         (term (make-instance 'term
                              :corpus corpus
                              :name (copy-seq token)
                              :id id)))
    (setf (aref terms id) term)
    (setf (gethash token (token-term-id-map corpus)) id)
    ;; Persist the new term
    (append-line (name term) (term-pathname corpus))
    (add-term-space-to-tbv-file (tbv-pathname corpus))
    term))

;;; Adding articles
;;;
;;; Adding articles is done in a few phases:
;;;
;;;   - get the in-memory article count
;;;
;;;   - check consistency: are files the right size? do they match the
;;;     current article count in the tbv file, the psi file, the date
;;;     file, etc?
;;;
;;;   - update the on-disk metadata
;;;
;;;   - update the in-memory metadata
;;;

(define-condition inconsistent-metadata-counts (error)
  ((expected
    :initarg :expected
    :reader inconsistent-metadata-expected-count)
   (actual
    :initarg :actual
    :reader inconsistent-metadata-actual-count)
   (context
    :initarg :context
    :reader inconsistent-metadata-context))
  (:report (lambda (condition stream)
             (format stream "Expected ~D, got ~D in ~A"
                     (inconsistent-metadata-expected-count condition)
                     (inconsistent-metadata-actual-count condition)
                     (inconsistent-metadata-context condition)))))

(defun persistent-article-count (corpus)
  (with-binary-input (stream (article-counter-pathname corpus))
    (read-u32 stream)))

(defun (setf persistent-article-count) (new-value corpus)
  (with-binary-output (stream (article-counter-pathname corpus))
    (write-u32 new-value stream)))

(defun check-persistent-consistency (corpus)
  "Check the in-memory metadata counts against the on-disk persistent
counts. Signal an error on any inconsistency."
  (let ((count (article-count corpus))
        (persistent-count (persistent-article-count corpus))
        (header (restore 'tbv-file-header (tbv-pathname corpus)))
        (date-count (file-u32-record-count (dates-pathname corpus)))
        (psi-count (file-u32-record-count (psi-index-pathname corpus))))
    (flet ((check (actual context)
             (unless (= actual count)
               (error 'inconsistent-metadata-counts
                      :expected count
                      :actual actual
                      :context context))))
      (check persistent-count "counter file")
      (check (bits-used header) "term bit-vector file")
      (check date-count "date file")
      (check psi-count "phrase search index file")))
  t)

(defun article-id-storage-pathname (article-id corpus)
  (corpus-relative-pathname
   (make-pathname :name (id-file-name article-id)
                  :type "txt"
                  :directory '(:relative "articles"))
   corpus))

(defmethod storage-pathname ((article article))
  (article-id-storage-pathname (id article) (corpus article)))

(defgeneric add-article (article corpus)
  (:method ((pathname pathname) corpus)
    (let ((article (load-article pathname)))
      (add-article (change-class article 'corpus-article :corpus corpus)
                   corpus)))
  (:method ((string string) corpus)
    (add-article (probe-file string) corpus)))

(defgeneric persist-article (article corpus)
  (:method ((article corpus-article) corpus)
    (store (complete-message article) (storage-pathname article))))

(defgeneric persist-article-metadata (article corpus)
  (:method ((article corpus-article) corpus)
    (check-persistent-consistency corpus)
    (increase-bits-used (tbv-pathname corpus))
    (let ((psi (make-phrase-search-index article)))
      (add-article-to-tbv-file (id article) (tid-vector psi)
                               (tbv-pathname corpus))
      (store psi (psi-pathname corpus)))
    (append-line (message-id article) (message-id-pathname corpus))
    (let ((date (logand #xFFFFFFFF (date article))))
      (append-u32 date (dates-pathname corpus))
      (vector-push-extend date (dates corpus)))
    (store (stub-version article) (stub-storage-pathname article))
    (setf (persistent-article-count corpus) (id article))))

(defgeneric reset-metadata (corpus)
  (:method (corpus)
    (setf (article-count corpus) 0)
    (setf (message-id-article-id-map corpus) (make-string-table))
    (setf (dates corpus) (make-adjustable-vector))
    (setf (terms corpus) (make-adjustable-vector))
    (setf (token-term-id-map corpus) (make-string-table))))

(defgeneric reset-persistent-metadata (corpus)
  (:method (corpus)
    (dolist (fun (list #'tbv-pathname
                       #'term-pathname
                       #'message-id-pathname
                       #'psi-pathname
                       #'article-counter-pathname
                       #'psi-index-pathname
                       #'dates-pathname))
      (ignore-errors (delete-file (funcall fun corpus))))
    (initialize-corpus-pathname corpus)))

(defmethod add-article ((article corpus-article) corpus)
  (let* ((message-id (message-id article))
         (message-id-article-id-map (message-id-article-id-map corpus))
         (next-id (article-count corpus)))
    (unless (gethash message-id message-id-article-id-map)
      (setf (id article) next-id)
      (setf (gethash message-id message-id-article-id-map) next-id)
      (persist-article article corpus)
      (persist-article-metadata article corpus)
      (incf (article-count corpus))
      (setf (persistent-article-count corpus) (article-count corpus)))))

(defun re-add-article (article corpus)
  (assert (= (id article) (article-count corpus)))
  (persist-article-metadata article corpus)
  (incf (article-count corpus))
  (setf (persistent-article-count corpus) (article-count corpus)))

(defun rebuild-metadata (corpus)
  (reset-persistent-metadata corpus)
  (reset-metadata corpus)
  (loop for id from 0
        for article = (ignore-errors (load-article-id id corpus))
        while article do (re-add-article article corpus)))

(defmethod add-article ((article article) corpus)
  (change-class article 'corpus-article :corpus corpus)
  (add-article article corpus))

(defgeneric tid-vector (object)
  (:method (object)
    (let* ((tokens (tokens object))
           (tids (make-tid-vector (length tokens)))
           (corpus (corpus object)))
      (map-into tids
                (lambda (token)
                  (id (ensure-term token corpus)))
                tokens))))

(defmethod restore ((article corpus-article) source)
  (setf (complete-message article)
        (file-string (storage-pathname article)))
  article)

(defgeneric load-article-id (id corpus)
  (:method (id corpus)
    (restore (make-instance 'corpus-article
                            :id id
                            :corpus corpus)
             t)))


(defun term-id-article-bit-vector (term-id corpus)
  (load-term-bit-vector term-id (tbv-pathname corpus)))

(defgeneric article-bit-vector (object)
  (:method ((term term))
    (term-id-article-bit-vector (id term) (corpus term))))


(defgeneric make-term-bit-vector (corpus)
  (:method (corpus)
    (make-bit-vector (article-count corpus))))


(defun find-term-id (token corpus)
  (let* ((cdb (corpus-relative-pathname "terms.cdb" corpus))
         (key (string-octets (string-upcase token)))
         (value (lookup key cdb)))
    (when value
      (let ((string (map 'string 'code-char value)))
        (values (parse-integer string :radix 16))))))

(defun make-term-cdb (corpus)
  (with-open-file (stream (term-pathname corpus))
    (with-output-to-cdb (cdb (term-cdb-pathname corpus)
                             (temporary-pathname (term-cdb-pathname corpus)))
      (loop for id from 0
            for id-key = (string-octets (format nil "~X" id))
            for line = (read-line stream nil)
            while line do
            (add-record (string-octets line) id-key cdb)))))

