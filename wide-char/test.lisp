
;;;; parser tests

;;;; string-parser

(defun test-string-parser ()
    (let ((parser (string-parser "foo")))
      (write (funcall parser "foo"))
      (write (funcall parser "bar"))
      (write (funcall parser "foobar"))))

;; (test-string-parser)

;;;; eol-parser

(defun test-eol-parser ()
  (let ((parser (eol-parser)))
    (print (funcall parser ""))
    (print (funcall parser "foo"))))

;; (test-eol-parser)

;;;;;; Parser Combinators

;;;; seq-parser

(defun test-seq-parser ()
  (let ((parser (seq-parser (list (string-parser "foo")
                                  (string-parser "bar")))))
    (print (funcall parser "foobar"))
    (print (funcall parser "foofoo"))
    (print (funcall parser "foobarbaz"))))

;; (test-seq-parser)
      
;;;; or-parser

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

(defun test-modify ()
  (let* ((p1 (rep1-parser (string-parser "foo")))
         (modifier (lambda (data)
                     (concatenate-parsed (parsed data))))
         (parser (modify p1 modifier)))
    (print (funcall parser "foofoofoo"))
    (print (funcall parser "foobarfoo"))
    (print (funcall parser "barfoobar"))))

;; (test-modify)

;;;;;; Syntax Parsers

;;;; hex-parser

(defun test-%hex-parser ()
  (let ((parser (%hex-parser)))
    (print (funcall parser "012"))
    (print (funcall parser "ABC"))
    (print (funcall parser "f01"))
    (print (funcall parser "GHI"))))

;; (test-%hex-parser)

(defun test-hex-parser ()
  (let ((parser (hex-parser)))
    (print (funcall parser "0123DEF"))
    (print (funcall parser "4567abc"))
    (print (funcall parser "GHI"))))

;; (test-hex-parser)

;;;; kind

(defun test-kind-parser ()
  (let ((parser (kind-parser)))
    (print (funcall parser "Na"))
    (print (funcall parser "NA"))
    (print (funcall parser "N012"))))

;; (test-kind-parser)

;;;; char-code


;;;; char-range

(defun test-%char-range-parser ()
  (let ((parser (%char-range-parser)))
    (print (funcall parser "10..20"))
    (print (funcall parser "10"))
    (print (funcall parser "ABC"))
    (print (funcall parser "abc"))))

;; (test-%char-range-parser)

(defun test-char-range-parser ()
  (let ((parser (char-range-parser)))
    (print (funcall parser "10..20;"))
    (print (funcall parser "10;"))
    (print (funcall parser "ABC;"))
    (print (funcall parser "abc;"))))

;; (test-char-range-parser)

;;;; line

(defun test-%line-parser ()
  (let ((parser (%line-parser)))
    (print (funcall parser "00..11;Na"))
    (print (funcall parser "AA;W"))))

;; (test-%line-parser)

(defun test-line-parser ()
  (let ((parser (line-parser)))
    (print (funcall parser "00..11;Na"))
    (print (funcall parser "AA;W"))))

;; (test-line-parser)