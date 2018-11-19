
;; https://raw.githubusercontent.com/homma/cl-misc/master/lem-misc/init.lisp

;; (lem-vi-mode:vi-mode)

;; lower case for messages in pprint
(setq *print-case* :downcase)

;; enter lisp mode
(define-key *global-keymap* "C-x l" 'lem-lisp-mode:lisp-mode)

;; color theme
;; default is "emacs-dark"
(lem:load-theme "emacs-light")
