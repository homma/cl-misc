;; https://ethanschoonover.com/solarized/

(in-package :lem-user)

(defparameter *true-color* nil)

(if *true-color*
    (progn
      "true color"
      (defvar *base03* "#002b36")
      (defvar *base02* "#073642")
      (defvar *base01* "#586e75")
      (defvar *base00* "#657b83")
      (defvar *base0*  "#839496")
      (defvar *base1*  "#93a1a1")
      (defvar *base2*  "#eee8d5")
      (defvar *base3*  "#fdf6e3")
      (defvar *yellow* "#b58900")
      (defvar *orange* "#cb4b16")
      (defvar *red*    "#dc322f")
      (defvar *magenta* "#d33682")
      (defvar *violet* "#6c71c4")
      (defvar *blue*   "#268bd2")
      (defvar *cyan*   "#2aa198")
      (defvar *green*  "#859900"))
    (progn
      "xterm 256 colors"
      "fffff7 is out of the xterm-256color though"
      (defvar *base03* "#1c1c1c")
      (defvar *base02* "#262626")
      (defvar *base01* "#585858")
      (defvar *base00* "#626262")
      (defvar *base0*  "#808080")
      (defvar *base1*  "#8a8a8a")
      (defvar *base2*  "#e4e4e4")
      (defvar *base3*  "#ffffd7")
      (defvar *yellow* "#af8700")
      (defvar *orange* "#d75f00")
      (defvar *red*    "#d70000")
      (defvar *magenta* "#af005f")
      (defvar *violet* "#5f5faf")
      (defvar *blue*   "#0087ff")
      (defvar *cyan*   "#00afaf")
      (defvar *green*  "#5f8700")))

(define-color-theme "solarized-light" ()
  (foreground *base00*)
  (background *base3*)
  (cursor :foreground *blue* :background *base03*)
  (syntax-string-attribute :foreground *green* :background *base3*)
  (syntax-comment-attribute :foreground *base1* :background *base3*)
  (syntax-keyword-attribute :foreground *yellow* :background *base3*)
  (syntax-constant-attribute :foreground *orange* :background *base3*)
  (syntax-function-name-attribute :foreground *blue* :background *base3*)
  (syntax-variable-attribute :foreground *blue* :background *base3*)
  (syntax-type-attribute :foreground *cyan* :background *base3*)
  (syntax-builtin-attribute :foreground *violet* :background *base3*))

(load-theme "solarized-light")