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
HEX <- "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "A" / "B" / "C" / "D" / "E" / "F"

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
# [行] は [文字コードの範囲][セミコロン][文字種]
LINE <- CODE_RANGE SEMI KIND

# [文字コードの範囲] は [文字コード][ドットドット][文字コード] または [文字コード]
CODE_RANGE <- CHAR_CODE DOTDOT CHAR_CODE / CHAR_CODE

# [文字コード] は [16 進数]
CHAR_CODE <- HEX

# [文字種] は "F", "W", "H", "Na", "A", "N" のいずれか
KIND <- "F" / "W" / "H" / "Na" / "A" / "N"

# [16 進数] は "0", "1", "2", "3" ... "F" の 1 回以上の繰り返し
HEX <- "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "A" / "B" / "C" / "D" / "E" / "F"

# [セミコロン] は ";"
SEMI <- ";"

# [ドットドット] は ".."
DOTDOT <- ".."
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

### 作成するパーサー

作成するパーサーは以下の通りです。

- string-parser : 文字列パーサー

作成するパーサーコンビネータは以下の通りです。

- seq-parser : 順序パーサー
- or-parser : 選択パーサー
- rep1-parser : 1 回以上の繰り返しパーサー

ヘルパー関数として以下を作成します。

- modify : パース結果を変形するために、変形用関数を適用する関数

### string-parser

````lisp
;;; string-parser : string -> function
;;
;; str : string == stiring to match
;; returns : function == parser
;;
;; parser : string -> parse result
;;
;; data : string == sting to parse
;; returns : parse result
;;
;; parse result : (matched? accepted-string string-for-further-parsing)
;;
(defun string-parser (str)
  #'(lambda (data)
      (let* ((substr (subseq data 0 (length str)))
             (match (string= str substr))
             (accepted
               (if match
                   substr
                   nil))
             (rest
               (if match
                   (subseq data (length str))
                   str)))
        (list match accepted rest))))

;; test
(defun test-string-parser
    (let ((parser (string-parser "foo")))
      (write (funcall parser "foo"))
      (write (funcall parser "bar"))
      (write (funcall parser "foobar"))))
````

### seq-parser

### or-parser

### rep1-parser

### modify

# メモ

````lisp
(labels ((f (acc n)
           (if (< n 0)
               acc
               (f (concatenate 'string
                               (string-upcase (write-to-string n :base 16))
                               "\" / \""
                               acc) (1- n)))))
  (princ (f "F" 14)))
````