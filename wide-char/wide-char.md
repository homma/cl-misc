Title: EastAsianWidth.txt から全角文字の範囲を示すデータを作成する
---

## はじめに

Unicode の全角・半角判定ルーチンで使用するため、[EastAsianWidth.txt](http://ftp.unicode.org/Public/UNIDATA/EastAsianWidth.txt) をパースして、全角文字の範囲を示すデータを作成します。  
パーサーコンビネーターを使ってみたいので、正規表現は使用しません。

## 今回実装する内容

以下を実装していきます。

- ファイルフォーマットを PEG で定義
- 定義した PEG をもとにパーサーを作成
- 作成したパーサで `EastAsianWidth.txt` をパース
- パースした結果を加工してデータを作成

### PEG 定義

EastAsianWidth.txt のフォーマットを PEG で表すと以下のようになります。

````
# [ファイル] は [行] の 0 回以上の繰り返しの後に [EOF]
FILE <- LINE* EOF

# [行] は [文字コードの範囲][セミコロン][文字種][パディング][コメント][改行] または [コメント][改行] または空行
LINE <- CODE_RANGE SEMI KIND PADDING COMMENT NL / COMMENT NL / NL

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

以下については前処理で実行することにして、先ほどの PEG を簡略化します。

- 行の切り出し
- 空行およびコメントだけの行の削除
- パディングの削除
- コメントの削除

以下が簡略化版の PEG です。

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

この PEG 定義を使用してパーサーを作成します。

### 中間表現

パース結果として、ファイルの各行を以下の形式で出力します。

````lisp
'([文字種] [文字コード 1] [文字コード 2])
;; ドットドットがない場合は [文字コード 1] と [文字コード 2] は同じ値
````

### 抽出

出力を受け取ったら、[文字種] が全角文字に相当するものだけを抜き出します。  
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
'(#x[文字コード] #x[文字コード])
'(#x[文字コード] #x[文字コード])
  ...
'(#x[文字コード] #x[文字コード])
````

文字コードのアルファベット部分は小文字にします。

## パーサーの実装

今回のパースに固有ではない、汎用的に使用可能なパーサーと、今回のパース固有のパーサーの 2 種類を作成します。

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

### パース結果を表すデータ

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

### パース結果内のデータへのアクセス

パース結果のデータを処理するためのアクセッサを定義します。

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

### 文字列パーサー

`string-parser` はパースする文字列を受け取り、その文字列をパースするパーサーを返します。  
例えば、引き数に `foo` という文字列を受け取った場合は、`foo` をパースするパーサーを返します。 

`string-parser` から返される関数は無名関数（クロージャ）です。  
`foo` をパースするパーサー関数の場合は、引数にパースする文字列を受け取り、その文字列が `foo` で開始されるかを確認します。  

先ほど作成した `success` と `failure` でパース結果を返しています。

````lisp
;; string-parser : string -> function
;; str : string -- パースする文字列
(defun string-parser (str)
  (let ((len (length str)))
    #'(lambda (data)
        (if (< (length data) len)
            (failure data)
            (let* ((substr (subseq data 0 len))
                   (match (string= str substr)))
              (if match
                  (success substr (subseq data len))
                  (failure str)))))))
````

### 行末パーサー

行末を判定するパーサーです。  
パースする文字列のサイズが 0 だった場合に、行末であると判断します。

````lisp
(defun eol-parser ()
  #'(lambda (data)
      (if (string= "" data)
          (success "" data)
          (failure data))))
````

### 順序パーサー

ここからはパーサーコンビネータの実装です。

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

### 選択パーサー

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

### 1 回以上の繰り返しパーサー

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

### modify 関数

`modify` はパース結果を処理するための関数です。

引き数に一つのパーサーと、結果を変更するための関数を受け取り、パーサーを実行した結果に関数を適用します。  
パース結果をもとに必要なデータを生成するために使用します。

````lisp
(defun modify (parser fun)
  #'(lambda (data)
      (let ((result (funcall parser data)))
        (if (success-p result)
            (success (funcall fun result) (rest-string result))
            result))))
````

### ドットドットパーサー

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

### セミコロンパーサー

`semi-parser` は ";" をパースするためだけのパーサーです。

````lisp
(defun semi-parser ()
  (string-parser ";"))
````

### 16 進数文字列パーサー

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

### 文字種パーサー

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

### 文字コードパーサー

`char-code-parser` は文字コードをパースするパーサーを返します。  
実装の実態は `hex-parser` です。

````lisp
(defun char-code-parser ()
  (hex-parser))
````

### 文字コードの範囲パーサー

`char-range-parser` は文字コードの範囲をパースするパーサーを返します。

`char-code-parser` と `dotdot-parser` を組み合わせてパーサーを作成しています。

文字コードの範囲は、"00..11" の形式と、"22" の形式があるので、両方の場合に対応する処理を実装しています。

`char-range-modifier` で文字コードの範囲のデータのみを抜き出し、16 進数文字列を数値に直しています。

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
     (parse-integer (parsed (nth 0 p)) :radix 16)
     (parse-integer (parsed (nth 2 p)) :radix 16))))

(defun char-range-parser ()
  (modify (%char-range-parser) #'char-range-modifier))
````

### 行パーサー

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

パーサーの実装は以上で終了です。

## パーサーの実行

ここからはプログラムの駆動部分を実装します。  
具体的には以下の処理を実装します。

- ファイルからデータを読み取る
- 前処理を施す
- パーサーを実行する
- 出力を加工する
- ファイルに書き出す

### コメント行の確認

空行もしくは `#` で始まっている行かどうかを判定します。

````lisp
(defun check-comment-line (line)
  (or (string= "" line)
      (string= "#" (subseq line 0 1))))
````

### コメントの除去

前処理の一環として、行データからコメントを削除します。

````lisp
(defun remove-comment (line)
  (let ((position (position #\# line :test 'equal)))
    (subseq line 0 position)))
````

### パディングの除去

前処理の一環として、行データからパディングを削除します。

````lisp
(defun remove-padding (line)
  (string-right-trim " " line))
````

### 行のパース

行データにパーサーを適用します。  

基本的にはパースは失敗しない前提です。  
もしパースが失敗した場合は、エラーを発生させ、デバッガが起動します。

````lisp
(defun parse-line (line)
  (let* ((parser (line-parser))
         (result (funcall parser line)))
    (if (success-p result)
        result
        (error "[parse error] ~S" line))))
````

### 全角文字列データの抜き出し

文字種が全角のものだけを抽出します。

````lisp
(defun select-wide (data)
  (let* ((p (parsed data))
         (kind (nth 0 p)))
    (if (or (string= "F" kind)
            (string= "W" kind))
        (cdr p)
        nil)))
````

### 行データに対する処理のまとめ

ここまでの処理を `process-line` 関数にまとめます。

````lisp
(defun process-line (line)
  (if (check-comment-line line)
      nil
      (select-wide
       (parse-line
        (remove-padding
         (remove-comment line))))))
````

### ファイルのオープンとファイルに対する処理

ローカルに配置した `EastAsianWidth.txt` を開き、行単位で読み込んで、先ほどの `process-line` 関数を適用します。

````lisp
(defun process-file ()
  (with-open-file (file
                   (make-pathname :name "EastAsianWidth.txt"))
    (labels ((process-lines (acc)
               (let ((line (read-line file nil)))
                 (if (not line)
                     (reverse acc)
                     (let ((result (process-line line)))
                       (if (not result)
                           (process-lines acc)
                           (process-lines (cons result acc))))))))
      (process-lines nil))))
````

### 連続する文字コードの連結

文字コードの範囲が連続している場合は、前後を連結します。

````lisp
(defun %merge-ranges (acc data)
  (if (> 2 (length data))
      (reverse (append data acc))
      (let ((fst (nth 0 data))
            (snd (nth 1 data))
            (remains (nthcdr 2 data)))
        (if (= (1+ (nth 1 fst))
               (nth 0 snd))
            (%merge-ranges acc
                           (cons (list (nth 0 fst) (nth 1 snd))
                                 remains))
            (%merge-ranges (cons fst acc)
                           (rest data))))))

(defun merge-ranges (data)
  (%merge-ranges nil data))
````

### 16 進数文字列への変換

数値データを 16 進数文字列データに変換します。

````lisp
(defun %hexadecimalize (acc data)
  (let ((convert (lambda (num)
                   (string-downcase
                    (concatenate 'string
                                 "#x"
                                 (write-to-string num :base 16))))))
    (if (not data)
        (reverse acc)
        (let ((pivot (first data)))
          (%hexadecimalize
           (cons (list (funcall convert (nth 0 pivot))
                       (funcall convert (nth 1 pivot)))
                 acc)
           (rest data))))))

(defun hexadecimalize (data)
  (%hexadecimalize nil data))
````

### 出力したい文字列への変換

16 進数文字列を括弧でくくり、最終的な出力形式に加工します。

````lisp
(defun %list-to-string (list)
  (concatenate 'string
               "'("
               (nth 0 list)
               " "
               (nth 1 list)
               ")"))


(defun make-output-string ()
  (let ((data (hexadecimalize (merge-ranges (process-file))))
        (width 4))
    (labels ((list-to-string (acc newline data)
               (if (not data)
                   acc
                   (let ((pivot (first data))
                         (nl (= 0 (mod newline width))))
                     (list-to-string
                      (concatenate 'string
                                   acc
                                   (%list-to-string pivot)
                                   (if nl
                                       (string #\newline)
                                       " "))
                      (1+ newline)
                      (rest data))))))
      (list-to-string "" 1 data))))
````

### 結果の出力

ここまでの処理の結果をファイルに出力します。

````lisp
(defun output-to-file ()
  (with-open-file (file
                   (make-pathname :name "output.txt")
                   :direction :io
                   :if-exists :supersede)
    (princ (make-output-string)
           file)))
````

### プログラムの実行

`output-to-file` 関数を実行すると、カレントディレクトリの `output.txt` ファイルに処理結果が格納されます。

````lisp
(output-to-file)
````

## 出力の正しさの検証

Vim の [実装](https://github.com/vim/vim/blob/master/src/mbyte.c#L1419) と比較して、同一のデータであることを確認しました。

## 終わりに

### このプログラムの用途

最近使い始めたソフトウェアが対応している Unicode のバージョンが 7.0 でした。  
一方、Unicode の現時点での最新バージョンは 11.0 です。

Unicode 7.0 で困ることは、絵文字の全角判定です。  
絵文字を全角とすることは Unicode 9.0 で推奨となったようです。  
そのため、Unicode 9.0 より前のバージョンに対応している実装では、絵文字が半角文字とみなされてしまうことがあります。

これを避けるため、最新の Unicode バージョンに対応する必要がありました。

### 実装してみて

パーサーコンビネータは簡単に作成できて楽しい！

`おしまい`