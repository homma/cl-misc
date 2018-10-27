;;;;;; Parser Data

;;;; parse result

;; make success result
(defun success (parsed rest)
  (list t parsed rest))

;; make failure result
(defun failure (rest)
  (list nil "" rest))

;;;; accessor

;; check if succeeded
(defun success-p (parse-result)
  (nth 0 parse-result))

;; take parsed data
(defun parsed (parse-result)
  (nth 1 parse-result))

;; take remaining string for further parsing
(defun rest-string (parse-result)
  (nth 2 parse-result))

;;;;;; Basic Parsers

;;;; string-parser

;; string-parser : string -> function
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

;; (test-string-parser)

;;;; eol-parser

;; check if it reaches at the end of a string
(defun eol-parser ()
  #'(lambda (data)
      (if (string= "" data)
          (success "" data)
          (failure data))))

;; test
(defun test-eol-parser ()
  (let ((parser (eol-parser)))
    (print (funcall parser ""))
    (print (funcall parser "foo"))))

;; (test-eol-parser)

;;;;;; Parser Combinators

;;;; seq-parser
;; take a list of parsers and run them one by one
;; if all the parsers succeeds, the seq-parser succeeds
;; otherwise fails

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

;; (test-seq-parser)
      
;;;; or-parser
;; take a list of parsers and if one of them succeeds the or-parser succeeds
;; otherwise fails

(defun %or-parse (str parsers)
  (if (not parsers)
      (failure nil)
      (let* ((parser (car parsers))
             (result (funcall parser str)))
        (if (success-p result)
            result
            (%or-parse str (cdr parsers))))))

(defun or-parser (parsers)
  #'(lambda (data)
      (let ((result (%or-parse data parsers)))
        (if (success-p result)
            result
            (failure data)))))

;; test

(defun test-or-parser ()
  (let ((parser (or-parser
                 (list
                  (string-parser "foo")
                  (string-parser "bar")))))
    (print (funcall parser "foo"))
    (print (funcall parser "bar"))
    (print (funcall parser "baz"))))

;; (test-or-parser)

;;;; rep1-parser
;; take one parser and repeat parsing at least once

(defun %rep1-parse (acc str parser)
  (if (= 0 (length str))
      acc
      (let* ((result (funcall parser str)))
        (if (success-p result)
            (%rep1-parse (cons result acc)
                         (rest-string result)
                         parser)
            acc))))

(defun rep1-parser (parser)
  #'(lambda (data)
      (let ((result (%rep1-parse nil data parser)))
        (if (not result)
            (failure data)
            result))))

;; test

(defun test-rep1-parser ()
  (let ((parser (rep1-parser (string-parser "foo"))))
    (print (funcall parser "foo"))
    (print (funcall parser "foofoofoo"))
    (print (funcall parser "foobarfoo"))
    (print (funcall parser "barfoo"))))

;; (test-rep1-parser)

;;;;;; Helper Function

;;;; modify
;; execute a parser then apply a function to the result to mutate it

(defun modify (parser fun)
  #'(lambda (data)
      (let ((result (funcall parser data)))
        (if (success-p result)
            (funcall fun result)
            result))))

;; test

(defun test-modify ()
  (let* ((p1 (rep1-parser (string-parser "foo")))
         (modifier (lambda (data)
                     (apply #'concatenate 'string (mapcar #'parsed data))))
         (parser (modify p1 modifier)))
    (print (funcall parser "foofoofoo"))
    (print (funcall parser "foobarfoo"))
    (print (funcall parser "barfoobar"))))

;; (test-modify)

;;;;;; Syntax Parsers

;;;; dotdot-parser

(defun dotdot-parser ()
  (string-parser ".."))

;;;; semi-parser

(defun semi-parser ()
  (string-parser ";"))

;;;; hex-parser

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

;; test
(defun test-sequence-in-string ()
  (print (sequence-in-string 0 15))
  (print (sequence-in-string 10 20))
  (print (sequence-in-string 0 0)))

;; (test-sequence-in-string)

;; (defun hex-parser ()
;;  (or-parser

