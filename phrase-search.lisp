;;;; phrase-search.lisp

(in-package #:usenet-legend)

(deftype tid ()
  "A term id."
  '(unsigned-byte 32))

(deftype tid-vector (&optional size)
  `(simple-array tid (,size)))

(defun make-tid-vector (size)
  (make-array size :element-type 'tid :initial-element 0))

(defun term-id-vector (object lexicon)
  "Produce a specialized vector of TERM-IDs from the term-vector of OBJECT."
  (map 'tid-vector #'id (term-vector object lexicon)))

(defclass phrase-search-index ()
  ((tid-vector
    :initarg :tid-vector
    :accessor tid-vector
    :documentation "A vector of term ids for an article.")
   (dtid-vector
    :initarg :dtid-vector
    :accessor dtid-vector
    :documentation "A sorted vector of distinct term ids for an
    article.")
   (dtidp-vector
    :initarg :dtidp-vector
    :accessor dtidp-vector
    :documentation "A vector of indexes into tid-vector and
    tidp-vector where terms in the dtid-vector first occur in the
    article's tid-vector.")
   (tidp-vector
    :initarg :tidp-vector
    :accessor tidp-vector
    :documentation "A vector pointing to subsequent occurrences of
    terms in tid-vector. A kind of linked-list lookup scheme.")))

(defun make-phrase-search-index (object)
  (let* ((tid-vector (tid-vector object))
         (dtid-vector (sort (remove-duplicates tid-vector) #'<))
         (dtidp-vector (make-tid-vector (length dtid-vector)))
         (tidp-vector (make-tid-vector (length tid-vector))))
    (map-into dtidp-vector
              (lambda (tid)
                (position tid tid-vector))
              dtid-vector)
    (loop for i from 0 below (1- (length tid-vector))
          for tid across tid-vector
          for p = (or (position tid tid-vector :start (1+ i))
                      0)
          do (setf (aref tidp-vector i) p))
    (make-instance 'phrase-search-index
                   :tid-vector tid-vector
                   :dtid-vector dtid-vector
                   :dtidp-vector dtidp-vector
                   :tidp-vector tidp-vector)))

(defun bposition (k vector)
  "Bisect VECTOR to find the position of K."
  (declare (type tid k)
           (type tid-vector vector))
  (let ((l 0)
        (u (1- (length vector)))
        (i 0)
        (ki -1))
    (loop
      (when (< u l)
        (return nil))
      (setf i (floor (+ l u) 2))
      (setf ki (aref vector i))
      (cond ((< k ki)
             (setf u (1- i)))
            ((< ki k)
             (setf l (1+ i)))
            ((= ki k)
             (return i))))))

(defun phrase-match (phrase-vector tid-vector position)
  "Return T if PHRASE-VECTOR matches all of TID-VECTOR at POSITION."
  (declare (type tid-vector phrase-vector tid-vector)
           (type tid position)
           (optimize speed))
  (let ((i 0)
        (j position)
        (end (+ position (length phrase-vector)))
        (n 0)
        (m 0)
        result)
    (declare (type tid i j end n m))
    (when (< end (length tid-vector))
      (tagbody
       loop
         (when (= j end)
           (setf result t)
           (go end))
         (setf n (aref phrase-vector i)
               m (aref tid-vector j))
         (when (/= n m)
           (go end))
         (incf i)
         (incf j)
         (go loop)
         end)
      result)))

(defun phrase-search (phrase-vector search-phrase-index)
  (declare (optimize speed)
           (type tid-vector phrase-vector))
  (with-slots (tid-vector dtid-vector dtidp-vector tidp-vector)
      search-phrase-index
    (declare (type tid-vector tid-vector dtid-vector dtidp-vector tidp-vector))
    (let ((start-tid (aref phrase-vector 0))
          (dtid-index 0)
          (pos 0))
      (declare (type tid start-tid dtid-index pos))
      ;; If bposition fails here, the phrase is not technically
      ;; starting at 0, but the mismatch will happen quickly
      (setf dtid-index (or (bposition start-tid dtid-vector) 0))
      (setf pos (aref dtidp-vector dtid-index))
      (loop
        (when (zerop pos)
          (return))
        (when (phrase-match phrase-vector tid-vector pos)
          (return pos))
        (setf pos (aref tidp-vector pos))))))

(defmethod store ((index phrase-search-index) (stream stream))
  (let* ((header (make-array 3 :element-type 'u32))
         (tid (tid-vector index))
         (tidp (tidp-vector index))
         (dtid (dtid-vector index))
         (dtidp (dtidp-vector index))
         (size (* 4 (+ (length header)
                       (length tid)
                       (length tidp)
                       (length dtid)
                       (length dtidp)))))
    (setf (aref header 0) size)
    (setf (aref header 1) (length tid))
    (setf (aref header 2) (length dtid))
    (dolist (vector (list header tid tidp dtid dtidp))
      (write-u32-vector vector stream))
    size))

(defmethod restore ((index phrase-search-index) (stream stream))
  (let ((header (make-array 3 :element-type 'u32)))
    (read-u32-vector header stream)
    (let* ((size (aref header 0))
           (tid-size (aref header 1))
           (dtid-size (aref header 2))
           (tid (make-tid-vector tid-size))
           (tidp (make-tid-vector tid-size))
           (dtid (make-tid-vector dtid-size))
           (dtidp (make-tid-vector dtid-size)))
      (declare (ignore size))
      (read-u32-vector tid stream)
      (read-u32-vector tidp stream)
      (read-u32-vector dtid stream)
      (read-u32-vector dtidp stream)
      (setf (tid-vector index) tid
            (tidp-vector index) tidp
            (dtid-vector index) dtid
            (dtidp-vector index) dtidp)
      index)))

(defmethod store ((index phrase-search-index) (file pathname))
  (let ((position 0))
    (with-binary-output (stream file :if-exists :append)
      (setf position (file-length stream))
      (store index stream))
    (with-binary-output (stream (index-file-pathname file)
                                :if-exists :append)
      (write-u32 position stream))))

(defun load-phrase-search-index (id corpus)
  (restore-record-id 'phrase-search-index id (psi-pathname corpus)))
