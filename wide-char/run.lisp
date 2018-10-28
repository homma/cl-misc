;; check-comment-line
;; check if the line is comment only
(defun check-comment-line (line)
  (or (string= "" line)
      (string= "#" (subseq line 0 1))))

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
;; removes kind data
(defun select-wide (data)
  (let* ((p (parsed data))
         (kind (nth 0 p)))
    (if (or (string= "F" kind)
            (string= "W" kind))
        (cdr p)
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
                     (reverse acc)
                     (let ((result (process-line line)))
                       (if (not result)
                           (process-lines acc)
                           (process-lines (cons result acc))))))))
      (process-lines nil))))

;; (print (process-file))

;; merge consecutive ranges

(defun %merge-ranges (acc data)
  (if (> 2 (length data))
      (reverse (append data acc))
      (let ((fst (nth 0 data))
            (snd (nth 1 data))
            (remains (nthcdr 2 data)))
        (if (= (1+ (nth 1 fst))
               (nth 0 snd))
            (%merge-ranges acc
                           (cons (list (nth 0 fst) (nth 1 snd))
                                 remains))
            (%merge-ranges (cons fst acc)
                           (rest data))))))

(defun merge-ranges (data)
  (%merge-ranges nil data))

;; (print (merge-ranges (process-file)))

;; output as hexadecimal number with prepending "#x"

(defun %hexadecimalize (acc data)
  (let ((convert (lambda (num)
                   (concatenate 'string
                                "#x"
                                (write-to-string num :base 16)))))
    (if (not data)
        (reverse acc)
        (let ((pivot (first data)))
          (%hexadecimalize
           (cons (list (funcall convert (nth 0 pivot))
                       (funcall convert (nth 1 pivot)))
                 acc)
           (rest data))))))

(defun hexadecimalize (data)
  (%hexadecimalize nil data))

;; (princ (hexadecimalize (merge-ranges (process-file))))

(defun output-to-file ()
  (with-open-file (file
                   (make-pathname :name "output.txt")
                   :direction :output)
    (print (hexadecimalize (merge-ranges (process-file))) file)))

;; (output-to-file)