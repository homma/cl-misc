;;;; hex-parser

;; unused
;; make a sequence as list of string
(defun sequence-in-string (from to)
  (labels ((sequence (acc from to)
             (if (> from to)
                 acc
                 (sequence
                  (cons (write-to-string to :base 16)
                        acc)
                  from
                  (1- to)))))
    (sequence nil from to)))

;; unused
;; test
(defun test-sequence-in-string ()
  (print (sequence-in-string 0 15))
  (print (sequence-in-string 10 20))
  (print (sequence-in-string 0 0)))

;; (test-sequence-in-string)

;; unused
;; parse one hex char
(defun hex-char-parser ()
  (let ((numbers (sequence-in-string 0 15)))
    (labels ((parser-gen (acc numbers)
               (if (not numbers)
                   acc
                   (let ((number (car numbers)))
                     (parser-gen
                      (cons (string-parser number) acc)
                      (cdr numbers))))))
      (or-parser (parser-gen nil numbers)))))

;; unused
;; test

(defun test-hex-char-parser ()
  (let ((parser (hex-char-parser)))
    (print (funcall parser "0"))
    (print (funcall parser "A"))
    (print (funcall parser "f"))
    (print (funcall parser "G"))))

;; (test-hex-char-parser)

;; unused
;; (defun hex-parser ()
;;   #'(lambda (data)
;;       (let* ((p1 (hex-parser-gen))
;;              (modifier (lambda (data)
;;                          (concatenate-parsed (parsed data))))
;;              (parser (modify p1 modifier)))
;;         (funcall parser data))))
