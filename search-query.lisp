;;;; search-query.lisp

(in-package #:usenet-legend)

(defun search-phrase-term-ids (search-string lexicon)
  "Parse SEARCH-STRING into a vector of terms. If some words in the
string aren't present in LEXICON, return NIL."
  (block nil
    (map 'vector
         (lambda (token)
           (let ((id (find-term-id token lexicon)))
             (if id
                 id
                 (return nil))))
         (tokens search-string))))

(defclass search-query ()
  ((search-string
    :initarg :search-string
    :reader search-string
    :documentation "The original, unparsed string that produced the
    query.")
   (term-ids
    :initarg :term-ids
    :accessor term-ids
    :initform nil
    :documentation "A list of terms ids that must be present in all results.")
   (excluded-term-ids
    :initarg :excluded-term-ids
    :accessor excluded-term-ids
    :initform nil
    :documentation "A list of terms ids that must be absent from any result.")
   (phrases
    :initarg :phrases
    :initform nil
    :accessor phrases)
   (excluded-phrases
    :initarg :excluded-phrases
    :initform nil
    :accessor excluded-phrases)
   (corpus
    :initarg :corpus
    :accessor corpus)))

(defmethod print-object ((search-query search-query) stream)
  (print-unreadable-object (search-query stream :type t)
    (format stream "~S" (search-string search-query))))

(defun parse-search-string (string)
  (let ((type nil)
        (buffer (make-string-output-stream))
        (result '()))
    (labels ((save ()
               (when type
                 (push (list type (get-output-stream-string buffer))
                       result)
                 (setf type nil)))
             (out (char)
               (case char
                 ((#\Space #\Tab #\Newline #\Return #\No-Break_Space)
                  #'out)
                 (#\-
                  #'pending-negative)
                 (#\"
                  (setf type :phrase)
                  #'in-phrase)
                 (t
                  (write-char char buffer)
                  (setf type :word)
                  #'in-word)))
             (pending-negative (char)
               (case char
                 ((#\Space #\Tab #\Newline #\Return #\No-Break_Space)
                  #'pending-negative)
                 (#\"
                  (setf type :excluded-phrase)
                  #'in-phrase)
                 (t
                  (write-char char buffer)
                  (setf type :excluded-word)
                  #'in-word)))
             (in-phrase (char)
               (case char
                 (#\\
                  #'phrase-backslash)
                 (#\"
                  (save)
                  #'out)
                 (t
                  (write-char char buffer)
                  #'in-phrase)))
             (phrase-backslash (char)
               (write-char char buffer)
               #'in-phrase)
             (in-word (char)
               (case char
                 ((#\Space #\Tab #\Newline #\Return #\No-Break_Space)
                  (save)
                  #'out)
                 (t
                  (write-char char buffer)
                  #'in-word))))
      (loop with state = #'out
            for char across string do
            (setf state (funcall state char)))
      (save)
      result)))

(defun make-search-query (string lexicon)
  (block nil
    (labels ((get-term-id (word)
               (find-term-id (clean-token word) lexicon))
             (get-phrase-ids (string)
               (search-phrase-term-ids string lexicon)))
      (let (term-ids excluded-term-ids phrases excluded-phrases)
        (dolist (result (parse-search-string string))
          (destructuring-bind (type value)
              result
            (ecase type
              (:word
               (pushnew (get-term-id value) term-ids))
              (:excluded-word
               (pushnew (get-term-id value) excluded-term-ids))
              (:phrase
               (let* ((ids (get-phrase-ids value)))
                 ;; Require individual terms from phrases
                 (map nil (lambda (term-id) (pushnew term-id term-ids))
                      ids)
                 ;; Don't add 1-word phrases to the phrase list
                 (when (< 1 (length ids))
                   (push (coerce ids 'tid-vector) phrases))))
              (:excluded-phrase
               (let ((phrase-ids (get-phrase-ids value)))
                 (if (= 1 (length phrase-ids))
                     ;; Convert 1-word phrase into a plain term
                     (push (aref phrase-ids 0) excluded-term-ids)
                     (push (coerce phrase-ids 'tid-vector)
                           excluded-phrases)))))))
        (when (or (member nil term-ids)
                  (member nil phrases)
                  (every #'null (list term-ids phrases
                                      excluded-term-ids
                                      excluded-phrases)))
          ;; No results, or one of the terms is unknown
          (return))
        (make-instance 'search-query
                       :search-string string
                       :term-ids term-ids
                       :excluded-term-ids (delete nil excluded-term-ids)
                       :phrases phrases
                       :corpus lexicon
                       :excluded-phrases (delete nil excluded-phrases))))))
