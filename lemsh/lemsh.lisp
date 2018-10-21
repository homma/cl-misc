(defpackage lemsh
  (:use cl lem lem-base)
  (:export start stop pwrite))

(in-package lemsh)

(defvar *proc*)
(defvar *p-in*)
(defvar *p-out*)

(defvar *timeout* 0.05)
(defvar *interval* 0.01)

;;; start program

(defun create-process (command)
  (setq *proc* (uiop:launch-program command
                                    :input :stream
                                    :output :stream))
  (setq *p-in* (uiop:process-info-input *proc*))
  (setq *p-out* (uiop:process-info-output *proc*)))

(defun start (command)
  (create-process command))

;;; stop program

(defun stop-process ()
  (uiop:close-streams *proc*)
  (uiop:terminate-process *proc*)
  (uiop:wait-process *proc*))

(defun stop ()
  (stop-process))

;;; write to program and read

(defun listen-with-timeout (timeout interval in)
  (if (< timeout 0)
      nil
      (if (listen in)
          t
          (progn
            (sleep interval)
            (listen-with-timeout (- timeout interval) interval in)))))

(defun read-all (in)
  (labels ((%read-all (acc in)
             (let ((timeout *timeout*)
                   (interval *interval*))
               (if (listen-with-timeout timeout interval in)
                   (%read-all (concatenate 'string
                                           (read-line in)
                                           (string #\newline)
                                           acc)
                              in)
                   acc))))
    (%read-all nil in)))

(defun write-and-flush (str out)
  (progn
    (write-line str out)
    (finish-output out)))

(defun write-buffer (str)
  (let ((point (current-point)))
    (insert-character point #\newline)
    (insert-string point str)))

(defun pwrite (str)
  (write-and-flush str *p-in*)
  (write-buffer (read-all *p-out*)))

;;; test

;; bc -lq
(defun test-bc ()
  ;; create sub process
  (start "bc -lq")

  ;; interact with it
  (pwrite "1 * 2 * 3 * 4")

  ;; quit
  (pwrite "quit")
  (stop)
)

;; sh
(defun test-sh ()
  ;; create sub process
  (start "sh")

  ;; interact with it
  (pwrite "ls -l")

  ;; quit
  (pwrite "exit")
  (stop)
)