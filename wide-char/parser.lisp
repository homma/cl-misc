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

;; take a list of results and concatenate their parsed string
(defun concatenate-parsed (data)
  (apply #'concatenate 'string (mapcar #'parsed data)))

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
  (let ((len (length str)))
    #'(lambda (data)
        (if (> (length data) len)
            (failure data)
            (let* ((substr (subseq data 0 len))
                   (match (string= str substr)))
              (if match
                  (success substr (subseq data len))
                  (failure str)))))))

;;;; eol-parser

;; check if it reaches at the end of a string
(defun eol-parser ()
  #'(lambda (data)
      (if (string= "" data)
          (success "" data)
          (failure data))))

;;;;;; Parser Combinators

;;;; seq-parser
;; take a list of parsers and run them one by one
;; if all the parsers succeeds, the seq-parser succeeds
;; otherwise fails

(defun %seq-parse (acc str parsers)
  (if (not parsers)
      (success (reverse acc) str)
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

;;;; rep1-parser
;; take one parser and repeat parsing at least once

(defun %rep1-parse (acc str parser)
  (labels ((result (acc str)
             (if acc
                 (success (reverse acc) str)
                 (failure str))))
    (if (= 0 (length str))
      (result acc str)
      (let* ((result (funcall parser str)))
        (if (success-p result)
            (%rep1-parse (cons result acc)
                         (rest-string result)
                         parser)
            (result acc str))))))

(defun rep1-parser (parser)
  #'(lambda (data)
      (%rep1-parse nil data parser)))

;;;;;; Helper Function

;;;; modify
;; execute a parser then apply a function to the result to mutate it

(defun modify (parser fun)
  #'(lambda (data)
      (let ((result (funcall parser data)))
        (if (success-p result)
            (success (funcall fun result) (rest-string result))
            result))))

;;;;;; Syntax Parsers

;;;; dotdot-parser

;(defun dotdot-parser ()
;  (string-parser ".."))

(defun dotdot-parser ()
  #'(lambda (data)
      (if (> 2 (length data))
          (failure data)
          (let ((parser (string-parser "..")))
            (funcall parser data)))))

;;;; semi-parser

(defun semi-parser ()
  (string-parser ";"))

;;;; hex-parser

;; returns a parser which parse hexadecimal string
(defun %hex-parser ()
  (rep1-parser
   (or-parser
    (list
     (string-parser "0")
     (string-parser "1")
     (string-parser "2")
     (string-parser "3")
     (string-parser "4")
     (string-parser "5")
     (string-parser "6")
     (string-parser "7")
     (string-parser "8")
     (string-parser "9")
     (string-parser "A")
     (string-parser "B")
     (string-parser "C")
     (string-parser "D")
     (string-parser "E")
     (string-parser "F")))))

(defun hex-modifier (data)
  (concatenate-parsed (parsed data)))

(defun hex-parser ()
  (modify (%hex-parser) #'hex-modifier))

;;;; kind

(defun kind-parser ()
  (or-parser
   (list
    (string-parser "F")
    (string-parser "W")
    (string-parser "H")
    (string-parser "Na")
    (string-parser "A")
    (string-parser "N"))))

;;;; char-code

(defun char-code-parser ()
  (hex-parser))

;;;; char-range

(defun single-char-code-modifier (data)
  (list data () data))

(defun %char-range-parser ()
  (or-parser
   (list
    (seq-parser
     (list
      (char-code-parser)
      (dotdot-parser)
      (char-code-parser)))
    (modify
     (char-code-parser)
     #'single-char-code-modifier))))

(defun char-range-modifier (data)
  (let ((p (parsed data)))
    (list
     (parsed (nth 0 p))
     (parsed (nth 2 p)))))

(defun char-range-parser ()
  (modify (%char-range-parser) #'char-range-modifier))

;;;; line

(defun %line-parser ()
  (seq-parser
   (list
    (char-range-parser)
    (semi-parser)
    (kind-parser)
    (eol-parser))))

(defun line-modifier (data)
  (let ((p (parsed data)))
    (cons
     (parsed (nth 2 p))
     (parsed (nth 0 p)))))

(defun line-parser ()
  (modify (%line-parser) #'line-modifier))
