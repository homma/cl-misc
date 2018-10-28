Title: EastAsianWidth.txt から全角文字の範囲を示すデータを作成する
---

## はじめに

Unicode の全角・半角判定ルーチンで使用するため、[EastAsianWidth.txt](http://ftp.unicode.org/Public/UNIDATA/EastAsianWidth.txt) をパースして、全角文字の範囲を示すデータを作成します。  
パーサーコンビネーターを使ってみたいので、正規表現は使用禁止とします。

## 今回実装する内容

### PEG 定義

EastAsianWidth.txt のフォーマットを PEG で表すと以下のようになります。

````
# [ファイル] は [行] の 0 回以上の繰り返しの後に [EOF]
FILE <- LINE* EOF

# [行] は [文字コードの範囲][セミコロン][文字種][パディング][コメント][改行] または [コメント][改行]
LINE <- CODE_RANGE SEMI KIND PADDING COMMENT NL / COMMENT NL

# [文字コードの範囲] は [文字コード][ドットドット][文字コード] または [文字コード]
CODE_RANGE <- CHAR_CODE DOTDOT CHAR_CODE / CHAR_CODE

# [文字コード] は [16 進数]
CHAR_CODE <- HEX

# [文字種] は "F", "W", "H", "Na", "A", "N" のいずれか
KIND <- "F" / "W" / "H" / "Na" / "A" / "N"

# [16 進数] は "0", "1", "2", "3" ... "F" の 1 回以上の繰り返し
HEX <- ( "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" /
         "A" / "B" / "C" / "D" / "E" / "F" )+

# [セミコロン] は ";"
SEMI <- ";"

# [パディング] は " " の 1 回以上の繰り返し
PADDING <- " "+

# [ドットドット] は ".."
DOTDOT <- ".."

# [改行] は 0x0A
NL <- 0x0A

# [コメント] は "#" の後に任意の文字の繰り返し
COMMENT <- "#" ANYCHAR

# ANYCHAR は省略
````

### PEG 定義（簡略化版）

以下を前処理で実行することにして、先ほどの PEG を簡略化します。

- 行の切り出し
- コメントだけの行の削除
- パディングの削除
- コメントの削除

````
# [行] は [文字コードの範囲][セミコロン][文字種][行末]
LINE <- CODE_RANGE SEMI KIND END_OF_LINE

# [文字コードの範囲] は [文字コード][ドットドット][文字コード] または [文字コード]
CODE_RANGE <- CHAR_CODE DOTDOT CHAR_CODE / CHAR_CODE

# [文字コード] は [16 進数]
CHAR_CODE <- HEX

# [文字種] は "F", "W", "H", "Na", "A", "N" のいずれか
KIND <- "F" / "W" / "H" / "Na" / "A" / "N"

# [16 進数] は "0", "1", "2", "3" ... "F" の 1 回以上の繰り返し
HEX <- ( "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" /
         "A" / "B" / "C" / "D" / "E" / "F" )+

# [セミコロン] は ";"
SEMI <- ";"

# [ドットドット] は ".."
DOTDOT <- ".."

# [行末] は省略
````

### 中間表現

まずは、ファイルの各行を以下の形式に変換します。

````lisp
'([文字種] [文字コード 1] [文字コード 2])
;; ドットドットがない場合は [文字コード 1] と [文字コード 2] は同じ値
````

### 抽出

変形が完了したら、[文字種] が全角文字に相当するものだけを抜き出します。  
今回抽出するのは、以下の `F` と `W` です。

````
F : Fullwidth
W : Wide
H : Halfwidth
Na : Narrow
A : Ambiguous
N : Neutral
````

### 結合

抽出したデータの各エントリーを確認し、次のエントリーと番号が繋がっているものは一つのエントリーにまとめます。

### 出力

最終的に以下の形式で出力します。

````lisp
'((#x[文字コード] #x[文字コード])
  (#x[文字コード] #x[文字コード])
  ...
  (#x[文字コード] #x[文字コード]))
````

文字コードのアルファベット部分は小文字にします。

## パーサーの実装

今回のパーシングに固有ではない、汎用的に使用可能なパーサーと、今回のパーシング固有のパーサーの 2 種類を作成します。

汎用パーサーはライブラリにまとめておけば今後も再利用可能です。

### 汎用パーサー

作成するパーサーは以下の通りです。

- string-parser : 文字列パーサー
- eol-parser : 行末パーサー

作成するパーサーコンビネータは以下の通りです。

- seq-parser : 順序パーサー
- or-parser : 選択パーサー
- rep1-parser : 1 回以上の繰り返しパーサー

ヘルパー関数として以下を作成します。

- modify : パース結果を変形するために、変形用関数を適用する関数

### 専用パーサー

今回のパーシングに固有のパーサーとして、以下のパーサーを作成します。

- dotdot-parser : ".." をパースするパーサー
- semi-parser : ";" をパースするパーサー
- hex-parser : 16 進数文字列をパースするパーサー
- kind-parser : 文字種をパースするパーサー
- char-code-parser : 文字コードをパースするパーサー
- char-range-parser : 文字コードの範囲をパースするパーサー
- line-parser : 行をパースするパーサー

### parse result

まず最初に、パーサーの返り値を作成するためのヘルパー関数を定義しておきます。  

返り値の形式は、`([パーシングが成功したか] [パースされた文字列] [パース済みの部分を取り除いた残りの文字列])` です。  
パーシングが成功した場合は、リストの先頭が `t` になります。

````lisp
;; make success result
(defun success (parsed rest)
  (list t parsed rest))

;; make failure result
(defun failure (rest)
  (list nil "" rest))
````

### accessor

パーシング結果のデータを処理するためのアクセッサも定義します。

- success-p : パースが成功したかどうかを確認します
- parsed : パースされた文字列を取得します
- rest-string : パースされて残った文字列を取得します
- concatenate-parsed : パース結果がネストした場合に、子のパース結果を連結してまとめます

````lisp
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
````
### string-parser

`string-parser` はパースする文字列を受け取って、その文字列をパースするパーサーを返します。  
例えば、"foo" という文字列を受け取った場合は、"foo" をパースするパーサーを返します。 

`string-parser` から返される関数は無名関数（クロージャ）です。  
"foo" をパースするパーサー関数の場合は、引数にパースする文字列を受け取り、その文字列が "foo" で開始されるかを確認します。  

先ほど作成した `success` と `failure` でパース結果を返しています。

````lisp
;; string-parser : string -> function
;; str : string -- パースする文字列
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
````

### eol-parser

行末を判定するパーサーです。  
パースする文字列のサイズが 0 だった場合に、行末であると判断します。

````lisp
(defun eol-parser ()
  #'(lambda (data)
      (if (string= "" data)
          (success "" data)
          (failure data))))
````

### seq-parser

ここからパーサーコンビネータの実装です。

`seq-parser` は複数のパーサーを受け取って、それを順番に実行するパーサーを返します。

`foobarbaz` という文字列があった場合に、`foo` パーサーと `bar` パーサーと `baz` パーサーを `seq-parser` に渡してあげると、パースが成功します。

`%seq-parse` というサブ関数を使用してパーサーの適用を実装しています。

````lisp
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
````

### or-parser

`or-parser` は複数のパーサーを受け取って、そのいずれか一つがパースに成功するかどうかを確認するパーサーです。

受け取ったパーサーを一つずつ適用し、パースに成功するものがあれば成功を返します。

PEG の `foo / bar / baz` の記法に対応しています。

````lisp
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
````

### rep1-parser

`rep1-parser` はパーサーを一つ受け取り、そのパーサーが 1 回以上連続で成功するかどうかを確認するパーサーです。

1 回もパースに成功しなかった場合は、失敗を返します。  
1 回でもパースに成功した場合は、成功を返します。

PEG の `foo+` に相当します。

````lisp
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
````

### modify

`modify` はパース結果を処理するためのパーサーです。

一つのパーサーと、結果を変更するための関数を受け取り、パーサを実行した結果に関数を適用します。

パース結果をもとに必要なデータを生成するために使用します。

````lisp
(defun modify (parser fun)
  #'(lambda (data)
      (let ((result (funcall parser data)))
        (if (success-p result)
            (success (funcall fun result) (rest-string result))
            result))))
````

### dotdot-parser

`dotdot-parser` は ".." をパースするためだけのパーサーを返します。

パーサーが受け取った文字列が ".." で始まっていた場合は成功となります。

````lisp
(defun dotdot-parser ()
  #'(lambda (data)
      (if (> 2 (length data))
          (failure data)
          (let ((parser (string-parser "..")))
            (funcall parser data)))))
````

### semi-parser

`semi-parser` は ";" をパースするためだけのパーサーです。

````lisp
(defun semi-parser ()
  (string-parser ";"))
````

### hex-parser

`hex-parser` は 16 進数文字列をパースするパーサーです。

サブ関数 `%hex-parser` で 16 進数文字列をパースしています。  
`string-parser` で 16 進数に使用される文字をパースし、`or-parser` と `rep1-parser` を使用して、繰り返しを表現しています。

`hex-modifier` でパース結果を連結し、一つの 16 進数文字列を返しています。

````lisp
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
````

### kind

`kind-parser` は文字種をパースするパーサーです。

`Na` が `N` より先に指定されているため、`Na` が `N` と判定されることはありません。

````lisp
(defun kind-parser ()
  (or-parser
   (list
    (string-parser "F")
    (string-parser "W")
    (string-parser "H")
    (string-parser "Na")
    (string-parser "A")
    (string-parser "N"))))
````

### char-code

`char-code-parser` は文字コードをパースするパーサーを返します。  
実装の実態は `hex-parser` です。

````lisp
(defun char-code-parser ()
  (hex-parser))
````

### char-range

`char-range-parser` は文字コードの範囲をパースするパーサーを返します。

`char-code-parser` と `dotdot-parser` を組み合わせてパーサーを作成しています。

文字コードの範囲は、"00..11" の形式と、"22" の形式があるので、両方の場合に対応する処理を実装しています。

````lisp
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
````

### line

`line-parser` は行をパースするパーサーを返します。

これまで作成してきたパーサーを組み合わせて、行のデータをパースしています。  
返り値は `([文字種] [文字コード1] [文字コード2])` の形式に整形してあります。

````lisp
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
````