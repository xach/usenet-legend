;;;; tokens.lisp

(in-package #:usenet-legend)

(defun clean-token (token)
  (let* ((token (nstring-upcase
                 (string-right-trim
                  ",.?!:¹²³`´«»<>_\",[]().';{}/"
                  (string-trim "`´«»<>_\",[]().';{}/" token)))))
    (if (or (some #'alpha-char-p token)
            (every #'digit-char-p token))
        token
        "")))

(defun maybe-split-token (token)
  (if (search "--" token)
      (ppcre:split "--" token)
      (list token)))

(defun stream-tokens (stream)
  (let ((buffer (make-string-output-stream))
        (tokens (make-array 10 :adjustable t :fill-pointer 0)))
    (labels ((save ()
               (let ((token (clean-token (get-output-stream-string buffer))))
                 (dolist (tok (maybe-split-token token))
                   (when (plusp (length tok))
                     (vector-push-extend tok tokens)))))
             (in-token (char)
               (case char
                 ((#\Space #\Tab #\Newline #\Return #\/ #\_ #\No-Break_Space)
                  (save)
                  #'in-whitespace)
                 (t
                  (write-char char buffer)
                  #'in-token)))
             (in-whitespace (char)
               (case char
                 ((#\Space #\Tab #\Newline #\Return #\No-Break_Space)
                  #'in-whitespace)
                 (t
                  (write-char char buffer)
                  #'in-token))))
      (loop with state = #'in-whitespace
            for char = (read-char stream nil)
            while char do (setf state (funcall state char)))
      (save)
      tokens)))

(defgeneric tokens (object)
  (:method ((stream stream))
    (stream-tokens stream))
  (:method ((string string))
    (with-input-from-string (stream string)
      (tokens stream)))
  (:method ((file pathname))
    (with-open-file (stream file)
      (tokens stream)))
  (:method ((list cons))
    "Combine the token vectors of LIST into a single vector with the
    empty token \"\" between each."
    (let* ((tokens (mapcar #'tokens list))
           (output-size (+ (reduce #'+ tokens :key #'length)
                           (1- (length tokens))))
           (output (make-array output-size :initial-element "")))
      (loop with start = 0
            for subtokens in tokens
            do
            (replace output subtokens :start1 start)
            (setf start (1+ (length subtokens))))
      output)))

