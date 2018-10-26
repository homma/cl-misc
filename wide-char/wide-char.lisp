;;; string-parser : string -> function
;;
;; str : string == stiring to match
;; returns : function == parser
;;
;; parser : string -> parse result
;;
;; data : string == sting to parse
;; returns : parse result
;;
;; parse result : (matched? accepted-string string-for-further-parsing)
;;
(defun string-parser (str)
  #'(lambda (data)
      (let* ((substr (subseq data 0 (length str)))
             (match (string= str substr))
             (accepted
               (if match
                   substr
                   nil))
             (rest
               (if match
                   (subseq data (length str))
                   str)))
        (list match accepted rest))))

;; test
(defun test-string-parser ()
    (let ((parser (string-parser "foo")))
      (write (funcall parser "foo"))
      (write (funcall parser "bar"))
      (write (funcall parser "foobar"))))

;;; parse result

;; make success result
(defun success (parsed rest)
  (list t parsed rest))

;; make failure result
(defun failure (rest)
  (list nil "" rest))

;;; accessor

;; check if succeeded
(defun success-p (parse-result)
  (nth 0 parse-result))

;; take parsed data
(defun parsed (parse-result)
  (nth 1 parse-result))

;; take remaining string for further parsing
(defun rest-string (parse-result)
  (nth 2 parse-result))

;;; seq-parser

(defun %seq-parse (acc str parsers)
  (if (not parsers)
      (success acc str)
      (let* ((parser (car parsers))
             (result (funcall parser str)))
        (if (success-p result)
            (%seq-parse (cons result acc)
                        (rest-string result)
                        (cdr parsers))
            (failure nil)))))

(defun seq-parser (parsers)
  #'(lambda (data)
      (let ((result (%seq-parse nil data parsers)))
        (if (success-p result)
            result
            (failure data)))))

;; test
(defun test-seq-parser ()
  (let ((parser (seq-parser (list (string-parser "foo")
                                  (string-parser "bar")))))
    (print (funcall parser "foobar"))
    (print (funcall parser "foofoo"))
    (print (funcall parser "foobarbaz"))))

(test-seq-parser)
      
;;; or-parser

(defun %or-parse (str parsers)
  (if (not parsers)
      (failure nil)
      (let* ((parser (car parsers))
             (result (funcall str parser)))
        (if (success-p result)
            result
            (%or-parse str (cdr parsers))))))

(defun or-parser (parsers)
  #'(lambda (data)
      (let ((result (%or-parse data parsers)))
        (if (success-p result)
            result
            (failure data)))))

;;; rep1-parser

;;(defun rep1-parser (parser)
;;  #'(lambda (data)
;;      ;; to be implemented

;;; modify

;; (labels ((f (acc n)
;;           (if (< n 0)
;;               acc
;;               (f (concatenate 'string
;;                               (string-upcase (write-to-string n :base 16))
;;                               "\" / \""
;;                               acc) (1- n)))))
;;  (princ (f "F" 14)))
