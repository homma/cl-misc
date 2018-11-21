
## Lem のカラーテーマに関するメモ

### 256 色の配列

`lem.term::*colors*` でアクセスできる。

````lisp
(print lem.term::*colors*)
````

### 256 色の色見本

https://en.wikipedia.org/wiki/Xterm#/media/File:Xterm_256color_chart.svg

### Solarized

True Color でないと Solarized 的な色は表現できない。

https://ethanschoonover.com/solarized/
````
SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
````

SOLARIZED : Solarized で使用される色名  
HEX : True Color の 16 進数表現  
TERMCOL : 16 色ターミナルカラーの色名（非公式名）  
XTERM/HEX : 256 色ターミナルカラーの色番号と 16 進数表現  

### Emacs

`M-x list-colors-display` で色見本を確認できる。

https://www.emacswiki.org/emacs/ListColors

## Lem

### Colors

端末の色は以下で登録している。

https://github.com/cxxxr/lem/blob/master/frontends/ncurses/term.lisp

### 色指定の属性

これら以外にもあるかもしれません。

- syntax-string-attribute
- syntax-comment-attribute
- syntax-keyword-attribute
- syntax-constant-attribute
- syntax-function-name-attribute
- syntax-variable-attribute
- syntax-type-attribute
- syntax-warning-attribute
- syntax-builtin-attribute

### Lisp Mode の色指定

https://github.com/cxxxr/lem/blob/master/modes/lisp-mode/grammer.lisp



### Markdown Mode の色指定

https://github.com/cxxxr/lem/blob/master/modes/markdown-mode/markdown-mode.lisp

以下の色指定が適用されます。

````
見出し : syntax-constant-attribute
# foo

引用 : syntax-string-attribute
> foo

コード : syntax-string-attribute
```
foo
```

水平線  : syntax-comment-attribute
- - -
* * *
_ _ _

リスト : syntax-keyword-attribute
- foo
* foo
+ foo
0. foo
````
