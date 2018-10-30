# PR message

Lem currently supports Unicode 7.0 East Asian width property
as its character width handling.

It would be nice to update it to the latest Unicode 11.0 version
so that it can properly display newly added wide characters
such as emoji.

This patch includes a machine generated wide char table using
the latest EastAsianWidth.txt below.

- http://ftp.unicode.org/Public/UNIDATA/EastAsianWidth.txt

You can compare it with other implementations.

VIM : https://github.com/vim/vim/blob/master/src/mbyte.c#L1419
Emacs : https://github.com/emacs-mirror/emacs/blob/master/lisp/international/characters.el#L1172

# Fixes

````lisp
(write-to-string #\🔥)
=> "#\\FIRE"
(write-to-string (char-code #\FIRE) :base 16)
=> "1F525"
(lem:wide-char-p #\FIRE)
=> t
;; it is nil in the current Lem implementaion
````

# Note 1

It still has some problem caused by the EastAsianWidth.txt .
But it is out of the scope of this patch and left for the future fix.

The detail of the problem is denoted in the following document.

- https://github.com/hamano/locale-eaw

````lisp
(write-to-string #\🌶)
=> "#\\HOT_PEPPER"
(write-to-string (char-code #\HOT_PEPPER) :base 16)
=> 1F336
(lem:wide-char-p #\HOT_PEPPER)
=> nil
;; HOT PEPPER is considered Newtral in EastAsianWidth.txt

(write-to-string #\🐿)
=> "#\\CHIPMUNK"
(write-to-string (char-code #\CHIPMUNK) :base 16)
=> "1F43F"
(lem:wide-char-p #\CHIPMUNK)
=> nil
;; CHIPMUNK is also considered Newtral

(write-to-string #\👁)
=> "#\\EYE"
(write-to-string (char-code #\EYE) :base 16)
=> "1F441"
(lem:wide-char-p #\EYE)
=> nil
;; considered as Newtral
````

# Note 2

Emacs has additional characters to be considered as full width compered to
the EastAsianWidth.txt .
I don't know why and it is also out of this patch's scope.

## character ragnges which are additionaly considered as full width

- #x2FFC ~ #x2FFF
- #x303F ~ #x3040
- #x3097 ~ #x3098
- #x3100 ~ #x3104
- #x3130
- #x318F
- #x31BB ~ #x31BF
- #x31E4 ~ #x31EF
- #x321F
- #x3248 ~ #x3249
- #x32FF
- #x4DC0 ~ #x4DFF
- #xA48D ~ #xA48F 
- #xA97D ~ #xA97F
- #xFE53
- #xFE67
- #xFE6C ~ #xFE6F
- #x1F203 ~ #x1F209
- #x1F23C ~ #x1F239
- #x1F249
- #x1F252 ~ #x1F259
- #x1F266 ~ #x1F299
- #x1FA60 ~ #x1FA6D
- #x2FFFE ~ #x2FFFF
- #x3FFFE ~ #x3FFFF

# Note 3

East Asian Ambiguous characters are also out of the scope of this patch.
It may need additional handling implemented in the future.