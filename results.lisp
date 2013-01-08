;;;; results.lisp

(in-package #:usenet-legend)

;;; Produce a set of result ids from applying a search query to a
;;; corpus.

(defun bit-vector-ids (vector end)
  (let ((result (make-array (count 1 vector))))
    (loop with i = 0
          for id from 0 below end
          for bit across vector
          when (plusp bit)
          do
          (setf (aref result i) id)
          (incf i))
    result))

(defun search-term-results (query corpus)
  (let ((result (make-term-bit-vector corpus)))
    (fill result 1)
    (dolist (term-id (term-ids query))
      (setf result (bit-and result (term-id-article-bit-vector term-id
                                                               corpus)
                            result)))
    (dolist (term-id (excluded-term-ids query))
      (setf result (bit-andc2 result (term-id-article-bit-vector term-id
                                                                 corpus)
                              result)))
    (bit-vector-ids result (article-count corpus))))

(defun include-phrase (phrase article-ids corpus)
  (let ((result (make-array 1 :adjustable t :fill-pointer 0)))
    (map nil
         (lambda (id)
           (when (phrase-search phrase (load-phrase-search-index id
                                                                 corpus))
             (vector-push-extend id result)))
         article-ids)
    result))

(defun exclude-phrase (phrase article-ids corpus)
  (let ((result (make-array 1 :adjustable t :fill-pointer 0)))
    (map nil
         (lambda (id)
           (unless (phrase-search phrase (load-phrase-search-index id
                                                                   corpus))
             (vector-push-extend id result)))
         article-ids)
    result))

(defun filter-phrases (search-query article-ids corpus)
  (let ((result article-ids))
    (dolist (phrase (phrases search-query))
      (setf result (include-phrase phrase result corpus)))
    (dolist (phrase (excluded-phrases search-query))
      (setf result (exclude-phrase phrase result corpus)))
    result))

(defclass search-result ()
  ((article-ids
    :initarg :article-ids
    :accessor article-ids)
   (corpus
    :initarg :corpus
    :accessor corpus)))

(defmethod print-object ((object search-result) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~D result~:P"
            (length (article-ids object)))))

(defun apply-search-query (search-query)
  (let* ((corpus (corpus search-query))
         (result (search-term-results search-query corpus))
         (dates (dates corpus)))
    (setf result (filter-phrases search-query result corpus))
    (let ((article-ids (sort result #'<
                             :key (lambda (article-id)
                                    (aref dates article-id)))))
      (make-instance 'search-result
                     :article-ids article-ids
                     :corpus corpus))))

(defun search-result-page (page-number per-page search-result)
  (assert (and (plusp page-number) (plusp per-page)))
  (let* ((article-ids (article-ids search-result))
         (count (length article-ids))
         (end (* per-page page-number))
         (start (- end per-page)))
    (if (<= count start)
        #()
        (let ((page (subseq article-ids start (min end count))))
          (map 'vector
               (lambda (article-id)
                 (restore-record-id 'stub-article article-id
                                    (corpus search-result)))
               page)))))

(defun search-in-corpus (search-string corpus)
  (let ((query (make-search-query search-string corpus)))
    (if query
        (apply-search-query query)
        (make-instance 'search-result
                       :article-ids #()
                       :corpus corpus))))
