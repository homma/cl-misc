
;; https://raw.githubusercontent.com/homma/cl-misc/master/lem-misc/init.lisp

;; (lem-vi-mode:vi-mode)

;; lower case for messages in pprint
(setq *print-case* :downcase)

;; enter lisp mode
(define-key *global-keymap* "C-x l" 'lem-lisp-mode:lisp-mode)

;; color theme
(lem:define-color-theme "soft-light" ("emacs-light")
  (lem:syntax-comment-attribute :foreground "gray59")
  (lem:syntax-constant-attribute :foreground "dark orange")
  (lem:syntax-keyword-attribute :foreground "green")
  (lem:syntax-builtin-attribute :foreground "green"))

(lem:load-theme "soft-light") 