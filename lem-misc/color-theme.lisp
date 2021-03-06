;; (in-package :lem-user)

;; 16 colors
(defvar *black* "#000000")
(defvar *red* "#800000")
(defvar *green* "#008000")
(defvar *yellow* "#808000")
(defvar *blue* "#000080")
(defvar *magenta* "#800080")
(defvar *cyan* "#008080")
(defvar *white* "#0c0c0c")
(defvar *brblack* "#808080")
(defvar *brred* "#ff0000")
(defvar *brgreen* "#00ff00")
(defvar *bryellow* "#ffff00")
(defvar *brblue* "#0000ff")
(defvar *brmagenta* "#ff00ff")
(defvar *brcyan* "#00ffff")
(defvar *brwhite* "#ffffff")

;; xterm color 256
(defvar *color33* "#0087ff")
(defvar *color37* "#00afaf")
(defvar *color61* "#5f5faf")
(defvar *color64* "#5f8700")
(defvar *color125* "#af005f")
(defvar *color136* "#af8700")
(defvar *color166* "#d75f00")
(defvar *color160* "#d70000")
(defvar *color230* "#ffffdf")
(defvar *color234* "#1c1c1c")
(defvar *color235* "#262626")
(defvar *color240* "#585858")
(defvar *color241* "#626262")
(defvar *color244* "#808080")
(defvar *color245* "#8a8a8a")

;; grayscale 24 colors
(defvar *gray23* "#080808")
(defvar *gray22* "#121212")
(defvar *gray21* "#1c1c1c")
(defvar *gray20* "#262626")
(defvar *gray19* "#303030")
(defvar *gray18* "#3a3a3a")
(defvar *gray17* "#444444")
(defvar *gray16* "#4e4e4e")
(defvar *gray15* "#585858")
(defvar *gray14* "#626262")
(defvar *gray13* "#6c6c6c")
(defvar *gray12* "#767676")
(defvar *gray11* "#808080")
(defvar *gray10* "#8a8a8a")
(defvar *gray9* "#949494")
(defvar *gray8* "#9e9e9e")
(defvar *gray7* "#a8a8a8")
(defvar *gray6* "#b2b2b2")
(defvar *gray5* "#bcbcbc")
(defvar *gray4* "#c6c6c6")
(defvar *gray3* "#d0d0d0")
(defvar *gray2* "#dadada")
(defvar *gray1* "#e4e4e4")
(defvar *gray0* "#eeeeee")

;; Solarized
(defvar *sol-base03* "#1c1c1c")
(defvar *sol-base02* "#262626")
(defvar *sol-base01* "#585858")
(defvar *sol-base00* "#626262")
(defvar *sol-base0*  "#808080")
(defvar *sol-base1*  "#8a8a8a")
(defvar *sol-base2*  "#e4e4e4")
(defvar *sol-base3*  "#ffffd7")
(defvar *sol-yellow* "#af8700")
(defvar *sol-orange* "#d75f00")
(defvar *sol-red*    "#d70000")
(defvar *sol-magenta* "#af005f")
(defvar *sol-violet* "#5f5faf")
(defvar *sol-blue*   "#0087ff")
(defvar *sol-cyan*   "#00afaf")
(defvar *sol-green*  "#5f8700")

(lem:define-color-theme "my-theme" ()
  (lem:cursor :foreground *sol-blue*)
  (lem:syntax-string-attribute :foreground *gray16*)
  (lem:syntax-comment-attribute :foreground *gray10*)
  (lem:syntax-keyword-attribute :foreground *sol-green*)
  (lem:syntax-constant-attribute :foreground *sol-cyan*)
  (lem:syntax-function-name-attribute :foreground *sol-orange*)
  (lem:syntax-variable-attribute :foreground *sol-violet*)
  (lem:syntax-builtin-attribute :foreground *sol-green*)
  (lem:syntax-type-attribute :foreground *sol-magenta*)
  (lem:syntax-warning-attribute :foreground *sol-red*))

(lem:load-theme "my-theme")

(lem:define-color-theme "soft-light" ("emacs-light")
  (lem:syntax-comment-attribute :foreground "gray59")
  (lem:syntax-keyword-attribute :foreground "green"))

;; foobarbaz
(lem:load-theme "soft-light")