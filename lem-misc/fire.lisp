(in-package cl-user)
(defpackage fire (:use cl lem lem-base))
(in-package fire)

(let ((point (current-point)))
  (insert-string point "🔥"))

(defun fire (n point)
  (labels ((f (i)
    (if (= i 0) nil
        (progn
          (insert-string point "🔥")
          (f (- i 1))))))
    (f n)))

(fire 10 (current-point))


