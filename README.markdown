 rc-boot.el - emacs.el を分割しちゃったり拡張を自動インストールしたりするもの
===============================================================================

 なんやのんコレ?
-----------------

.emacs.el がウンコみたいに長くなってきてメンテナンスしきれへんわー
ってときに、ファイルを分割したくなったりしますが、

  (load "~/.emacs.d/rc/hoge.el")
  (load "~/.emacs.d/rc/fuga.el")
  (load "~/.emacs.d/rc/piyo.el")
  (load "~/.emacs.d/rc/punyo.el")
  (load "~/.emacs.d/rc/moe.el")
  (load "~/.emacs.d/rc/gunyu.el")
  (load "~/.emacs.d/rc/pooo.el")
  (load "~/.emacs.d/rc/peee.el")
    .
    .
    .
  
みたいになったりしてそれはそれでウザいし、で、どっかでエラーがでたり
すると途中で初期化がとまってウザかったりとウキーとなったりするんで
そのあたりを解決するものです。





 使うとどーなる?
-----------------

オイラの今のl .emacs.el に自分でかいたものは、

  (load "~/.emacs.d/rc/lisp/rc-boot.el")

の一行だけになってます。
他の設定の実体は、

  ~/.emacs.d/rc/*/*.el

の中に分散している状況です。





 rc-boot.el が読み込むファイル
-------------------------------

rc-boot.el は、各種設定ファイルが

  - ~/.emacs.d/rc/first
  - ~/.emacs.d/rc/base
  - ~/.emacs.d/rc/ext
  - ~/.emacs.d/rc/private
  - ~/.emacs.d/rc/project
  - ~/.emacs.d/rc/funcs
  - ~/.emacs.d/rc/last

の各種ディレクトリに入っていると考え、ディレクトリがあれば順番に
directory-files して、それが読み込むべきファイルだと判断されれば読み込みます。

読みこむべきファイルというのは、

  hoge.el

のように、単に .el のサフィクスを持つものか

  hoge.fsf-unicom-22.fsf-nt-@.el
  
のように、emacsen の識別パターンをサフィクスに含み、それが用いる emacs に
適合している場合です。


これで、うんこ長いけど、emacsen ごとにおもいっきり変わったりするフォント
設定とかをファイル別にわけることができます。

ああ、このemacsen の識別パターンと言うのは、簡単な正規表現の置き換えで、
. は \| に、@ は .*? になります。ですから、先のファイル名の持つパターンは
正規表現だと

  \`\(fsf-unicom-22\|fsf-nt-.*?\)\'

になりますね。で、何とマッチさせるかと言えば、rc-boot.el が rc-emacsen と
言う変数に設定する識別名でして、

   (emacs の種類) - (システムタイプ) - (emacs-major-version)

となります。

emacs の種類は、現状、 fsf と meadow の二種類、システムタイプは、

  - unicom  .... UNIX compatible なやつ
  - cygwin  .... cygwin
  - nt      .... windows 系

ってなります。ああ、でも Vista 以降の時代に nt ってなんだか
ヘンかもしれないですね。いいんです。今でも DOS とかって
識別名があるのが emacs たんなんですから。

要するに、unix 系と windows の fsf 系列しか今のところ対応してません。
xemacs とかよくわからないの。mac は unix系って認識で おk ? (よくわかってない)





 rc-boot.el が提供する便利かもしれないシンボル
-----------------------------------------------

  rc-emacsen ... 先述の emacsen の識別名が入ってます

  rc-emacsen-match .... rc-emacsen が与えられたパターンにマッチすると 
                        nil 以外を返します。
                        (rc-emasen-match "fsf-@-@") とかって使います。
                       ああ、でも (rc-emacsen-match "fsf-@-@.meadow-nt-23")
                       とか、ドット区切りのパターンには対応してないです。

  rc-emacsen-case .... rc-emacs-match を用いたブロックを生成するマクロです
  
                        (rc-emasen-case
                          (fsf-@-@
                            (foo-fsf)
                            (bar-fsf)
                          (meadow-@-@
                            (foo-meadow)
                            (bar-meadow))
                          (t
                            (foo-default)
                            (bar-default))))
  
                       は、
                      
                        (cond
                         ((rc-emacsen-match "fsf-@-@")
                            (foo-fsf)
                            (bar-fsf)
                          )
                          ((rc-emacsen-match "meadow-@-@")
                            (foo-meadow)
                            (bar-meadow))
                          (t
                            (foo-default)
                            (bar-default)))
                      
                       に展開されます。
                    
  
