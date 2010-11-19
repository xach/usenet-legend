;;;; split.lisp

(in-package #:usenet-legend)

(defun header-matcher (header-name pattern &rest scanner-args
                       &key &allow-other-keys)
  (let ((scanner (apply #'cl-ppcre:create-scanner pattern scanner-args)))
    (lambda (article)
      (let ((value (header header-name article)))
        (and value (cl-ppcre:scan scanner value) t)))))

(defun save-article (article file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-string (complete-message article) stream))
  (probe-file file))

(defun split-pathname (article)
  (make-pathname :name (string-trim "<>" (message-id article))
                 :type "txt"))

(defun split-archive (predicate input-file output-directory)
  "Load each article in the monolithic INPUT-FILE. If PREDICATE called
with the article returns true, save the article to an individual file
in OUTPUT-DIRECTORY. The name of the output file is based on the
article's message-id."
  (let ((buffer (make-string-output-stream)))
    (with-open-file (stream input-file)
      (loop
        (let ((line (read-line stream nil)))
          (when (or (null line) (string= line "From ..."))
            (let ((article (string-article (get-output-stream-string buffer))))
              (when (funcall predicate article)
                (let ((output (merge-pathnames (split-pathname article)
                                               output-directory)))
                  (ensure-directories-exist output)
                  (save-article article output)))))
          (when (null line)
            (return))
          (write-line line buffer))))))
