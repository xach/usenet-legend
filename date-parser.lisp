;;;; date-string-case.lisp

(in-package #:usenet-legend)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *month-names* #("JAN" "FEB" "MAR"
                        "APR" "MAY" "JUN"
                        "JUL" "AUG" "SEP"
                        "OCT" "NOV" "DEC"))

(defun month-number (name)
  (let ((pos (position name *month-names* :test #'string-equal)))
    (if pos
        (1+ pos)
        (error "Unknown month -- ~A" name))))

(defun zone-number (name)
  (assert (or (equalp name "UT")
              (equalp name "GMT"))))

(defun parse-date-pattern (string)
  "Parse date pattern STRING into a data structure. The characters Z
and 9 represent runs of characters and digits respectively."
  (let ((type nil)
        (start nil)
        (pos 0)
        (result '()))
    (labels ((save ()
               (when type
                 (push (list type start pos) result)
                 (setf type nil)))
             (start (kind)
               (setf type kind)
               (setf start pos))
             (outside (char)
               (case char
                 (#\Z
                  (start :string)
                  #'in-string)
                 (#\9
                  (start :digits)
                  #'in-digits)
                 (t
                  #'outside)))
             (in-string (char)
               (case char
                 (#\Z
                  #'in-string)
                 (#\9
                  (save)
                  (start :digits)
                  #'in-digits)
                 (t
                  (save)
                  #'outside)))
             (in-digits (char)
               (case char
                 (#\9
                  #'in-digits)
                 (#\Z
                  (save)
                  (start :string)
                  #'in-string)
                 (t
                  (save)
                  #'outside))))
      (let ((state #'outside))
        (loop for i from 0
              for char across string
              do (setf pos i state (funcall state char)))
        (setf pos (length string))
        (save)))
    (nreverse result)))

(defun date-bindings-alist (control-string bindings)
  (let ((specs (parse-date-pattern control-string)))
    (when (/= (length specs) (length bindings))
      (error "Binding count does not match control string: ~S ~S"
             bindings specs))
    (mapcar 'cons bindings specs)))

(defun date-parser-case-form (var control-string bindings)
  (let ((alist (date-bindings-alist control-string bindings)))
    (labels ((value-form (key)
               (destructuring-bind (type start end)
                   (lookup key)
                 (ecase type
                   (:digits `(parse-integer ,var :start ,start :end ,end))
                   (:string `(subseq ,var ,start ,end)))))
             (lookup (key)
               (cdr (assoc key alist))))
      `(,(length control-string)
         (setf year ,(value-form 'year))
         (setf day ,(value-form 'day))
         ,@(when (lookup 'month)
                 `((setf month ,(value-form 'month))))
         ,@(when (lookup 'month-name)
                 `((setf month (month-number ,(value-form 'month-name)))))
         ,@(loop for key in '(hour minute second)
                 when (lookup key)
                 collect `(setf ,key ,(value-form key)))))))

(defmacro date-parser (string &body patterns)
  "Create a set of patterns against which STRING (evaluated) must
match. Each pattern goes along with a list of bindings; the values of
the bindings are used to return a universal-time from the DATE-PARSER
form."
  (loop with lengths = (make-hash-table)
        for (control-string bindings) in patterns
        for length = (length control-string)
        for existing = (gethash length lengths)
        when existing do
        (error "Identical length  (~D) patterns in date-string-case; first was ~S, second was ~S"
               length existing control-string))
  (let ((var (gensym)))
    `(let ((,var ,string)
           (year 0)
           (month 0)
           (day 0)
           (hour 0)
           (minute 0)
           (second 0)
           (zone 0))
       (ecase (length ,var)
         ,@(loop for (control-string bindings) in patterns
                collect (date-parser-case-form var control-string bindings)))
       (encode-universal-time second minute hour day month year zone))))
)

(defun parse-date (date)
  (date-parser date
    ("9 ZZZ 9999 99:99:99 +9999"
     (day month-name year hour minute second zone))
    ("99 ZZZ 99 99:99:99 GMT"
     (day month-name year hour minute second))
    ("9 ZZZ 99 99:99:99 GMT"
     (day month-name year hour minute second))
    ("9 ZZZ 9999 99:99:99 GMT"
     (day month-name year hour minute second))
    ("99 ZZZ 9999 99:99:99 GMT"
     (day month-name year hour minute second))
    #+nil
    ("99 ZZZ 9999 99:99:99 ZZ"
     (day month-name year hour minute second zone-name))
    ("9999/99/99"
     (year month day))
    ("ZZZ, 9 ZZZ 9999 99:99:99 99999"
     (weekday day month-name year hour minute second zone))
    ("ZZZ, 99 ZZZ 9999 99:99:99 99999"
     (weekday day month-name year hour minute second zone))
    ("99 ZZZ 9999 99:99:99 99999"
     (day month-name year hour minute second zone))
    ("ZZZ, 99 ZZZ 9999 99:99:99 ZZZ"
     (weekday day month-name year hour minute second zone-name))
    ("ZZZ, 99 ZZZ 9999 99:99:99 ZZZ ZZZ"
     (weekday day month-name year hour minute second zone-name dst))
    ("ZZZ, 9 ZZZ 9999 99:99:99 ZZZ"
     (weekday day month-name year hour minute second zone-name))
    ("ZZZ, 9 ZZZ 9999 99:99:99 +9999 (ZZZ)"
     (weekday day month-name year hour minute second zone zone-name))
    ("ZZZ, 99 ZZZ 9999 99:99:99 +9999 (ZZZ)"
     (weekday day month-name year hour minute second zone zone-name))))
