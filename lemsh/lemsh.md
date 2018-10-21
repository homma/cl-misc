Title: Lem エディタから OS コマンドを実行する

## はじめに

Lem エディタのバッファ上から OS のコマンドを実行し、結果をバッファに書き戻す処理を実装します。

### 実行例

今回実装するプログラムの実行例は以下の通りです。  
Lem エディタ内から `Bourne Shell` を実行し、シェルに対して `ls -l` を実行させ、結果をバッファに書き戻しています。

````lisp
;; load program
(load "lemsh")

;; create sub process
(lemsh:start "sh")

;; interact with it
(lemsh:pwrite "ls -l")
-rw-r--r--  1 me  staff  2197 10 21 15:35 lemsh.md
-rw-r--r--  1 me  staff  2111 10 21 15:32 lemsh.lisp
total 16

;; quit
(lemsh:stop)
````

`Bourne Shell` 以外にも、任意のコマンドを実行することが可能です。

### OS コマンドの実行方法

Lem エディタから OS コマンドを呼び出す方法はいくつかありますが、今回は [uiop](https://github.com/fare/asdf/tree/master/uiop) の [launch-program](https://github.com/fare/asdf/blob/master/uiop/launch-program.lisp) を使用します。

`uiop:launch-program` は OS コマンドを実行する関数のポータブルな実装です。

Lem から OS コマンドを実行する方法は他にも存在しますが、Lem 自身の実装に `uiop:run-program` が使用されていたため、類似の `uiop:launch-program` を使用しています。

### Disclaimer

- Common Lisp には詳しくないので、間違いがある可能性があります
- Common Lisp として適切なプログラミングスタイルではない可能性があります
- エラー処理は実装していません
- Lem の実装が変わった場合に、実行できなくなる可能性があります
- プログラムの実行は自己責任でお願いします

## 実装

### パッケージ

パッケージ名は `lemsh` としました。  
Lem の関数を使用するため、`lem` と `lem-base` を `use` しています。  
`start`、`stop`、`pwrite` の 3 つの関数を `export` しています。

````lisp
(defpackage lemsh
  (:use cl lem lem-base)
  (:export start stop pwrite))

(in-package lemsh)
````

### 変数

パッケージで使用する変数を定義しています。  

`*proc*` はサブプロセスの情報を保持する変数です。  
`*p-in*` はサブプロセスへの入力用ストリーム、`*p-out*` はサブプロセスからの出力用ストリームです。  
`*timeout*` と `*interval*` はサブプロセスへのポリングの間隔を設定します。

````lisp
(defvar *proc*)
(defvar *p-in*)
(defvar *p-out*)

(defvar *timeout* 0.05)
(defvar *interval* 0.01)
````

### OS コマンドの実行

`uiop:launch-program` に OS コマンド名を文字列で渡してサブプロセスを作成します。  

続いて、`*proc*`、`*p-in*`、`*p-out*` を設定しています。
サブプロセスの入力ストリームと出力ストリームは `uiop:process-info-input` と `uiop:process-info-output` で取得できます。

最後に、`export` 用に関数をラップしています。

````lisp
;;; start program

(defun create-process (command)
  (setq *proc* (uiop:launch-program command
                                    :input :stream
                                    :output :stream))
  (setq *p-in* (uiop:process-info-input *proc*))
  (setq *p-out* (uiop:process-info-output *proc*)))

(defun start (command)
  (create-process command))
````

### サブプロセスの停止

`uiop:terminate-process` でサブプロセスを停止します。

その前に、`uiop:close-streams` で入出力ストリームを閉じています。  
`uiop` の実装内のコメントによると、入出力ストリームを作成した場合は実行するようにとのことなので、ここで実行しています。

また、`uiop:wait-process` でプロセスの停止を待機しています。
こちらも `uiop` のコメントで実行するように指定されています。

最後に `export` 用に関数をラップしています。

````lisp
;;; stop program

(defun stop-process ()
  (uiop:close-streams *proc*)
  (uiop:terminate-process *proc*)
  (uiop:wait-process *proc*))

(defun stop ()
  (stop-process))
````

### サブプロセスへの入力と出力の受け取り

サブプロセスへの入力は `*p-in*` に文字列を書き込みます。  
サブプロセスの出力の受け取りは `*p-out*` を読み出します。

#### サブプロセスへの入力

以下がサブプロセスへ文字列を書き込む関数です。  
呼び出す際は、`out` に `*p-in*` を指定します。  

`write-line` で書き込んで `finish-output` でフラッシュしています。

````lisp
(defun write-and-flush (str out)
  (progn
    (write-line str out)
    (finish-output out)))
````

#### サブプロセスの出力の確認

サブプロセスが複数行の出力を返す場合は、出力データがまだ残っていないか確認をする必要があります。  

`C` の場合は `select` を使うことが多いと思いますが、`Common Lisp` では `listen` や `read-char-no-hang` を使用します。  
ここでは `listen` を使用して、サブプロセスの出力が残っていないか確認をしています。  

サブプロセスの出力が遅延することも考慮して、`timeout` の時間分は出力を待機するような実装にしています。

````lisp
(defun listen-with-timeout (timeout interval in)
  (if (< timeout 0)
      nil
      (if (listen in)
          t
          (progn
            (sleep interval)
            (listen-with-timeout (- timeout interval) interval in)))))
````

#### サブプロセスの出力を読み出す

`read-line` を使用して、サブプロセスの出力を 1 行ずつ読み出しています。  
サブプロセスの出力が残っているかを先ほどの `listen` を使用して判定しています。

````lisp
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
````

#### サブプロセスの出力をエディタのバッファに書き出す

`read-line` で読み出したサブプロセスの出力を Lem エディタのバッファに書き出します。

`lem-base:current-point` で現在のカーソル位置を取り出しています。  
`lem-base:insert-character` で、指定した場所に改行を書き込んでいます。  
更に、`lem-base:insert-string` で文字列を出力します。  

````lisp
(defun write-buffer (str)
  (let ((point (current-point)))
    (insert-character point #\newline)
    (insert-string point str)))
````

#### 上記の関数をまとめる

サブプロセスへの入力、サブプロセスからの出力の読み出し、エディタバッファへの書き込みを関数にまとめます。

````lisp
(defun pwrite (str)
  (write-and-flush str *p-in*)
  (write-buffer (read-all *p-out*)))
````

## 実行テスト

以下は `bc -lq` を実行して計算結果を受け取っている例です。

````lisp
;; create sub process
(lemsh:start "bc -lq")

;; interact with it
(lemsh:pwrite "1 * 2 * 3 * 4")
;; => 24

;; quit
(lemsh:pwrite "quit")
(lemsh:stop)
````

## まとめ

以上、Lem エディタ内から OS コマンドを実行し、その結果をエディタのバッファに出力する方法をまとめました。  

## 付記

### プログラムの改善ポイント

上記の実装には以下のような改善ポイントがありますが、このドキュメントの範囲外とさせていただきます。

- 複数のコマンドを同時に実行することを考慮していません
- エラー処理を行なっていません
- 入力データや出力データが大きい場合については検討していません
