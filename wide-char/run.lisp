;; check-comment-line
;; check if the line is comment only
(defun check-comment-line (line)
  (string= "#" (subseq line 0 1)))

;; (check-comment-line "#comment")
;; (check-comment-line " #comment")

;; remove a comment
(defun remove-comment (line)
  (let ((position (position #\# line :test 'equal)))
    (subseq line 0 position)))

;; (remove-comment "fooo #comment")

;; remove a padding
(defun remove-padding (line)
  (string-right-trim " " line))

;; (remove-padding "  string 1  string 2  ")

;; parse the line
(defun parse-line (line)
  (let* ((parser (line-parser))
         (result (funcall parser line)))
    (if (success-p result)
        result
        (error "[parse error] ~S" line))))

;; select wide character
(defun select-wide (data)
  (let ((kind (nth 0 data)))
    (if (or (string= "F" kind)
            (string= "W" kind))
        data
        nil)))

;; processes a line and returns processed data
(defun process-line (line)
  (if (check-comment-line line)
      nil
      (select-wide
       (parse-line
        (remove-padding
         (remove-comment line))))))

;; processes a file and returns processed data
(defun process-file ()
  (with-open-file (file
                   (make-pathname :name "EastAsianWidth.txt"))
    (labels ((process-lines (acc)
               (let ((line (read-line file nil)))
                 (if (not line)
                     acc
                     (let ((result (process-line line)))
                       (if (not result)
                           (process-lines acc)
                           (process-lines (cons result acc))))))))
      (process-lines nil))))

(print (process-file))

;; merge consecutive ranges

;; output with prepending "#x"