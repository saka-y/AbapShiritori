# ABAPしりとり

ABAPのサンプルとしてしりとりのプログラムを作ってみました。

## 概要

ABAPの言語仕様はこの10年で大きく変わっていますが、残念ながら古くからのABAP開発者にはその変化に取り残された人が多くいるようです。そこで、初学者向けのABAPソース読解用のサンプルとして作ってみました。ポイントは次の2点です。

### 1. サブルーチンを使用しない
ABAPの文法の基礎としてサブルーチン（FORM・PERFORM）を見かけることがあると思いますが、
[PERFORM - ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapperform.htm) に「Subroutines are obsolete.」という記述があるとおり、10年前には非推奨になっている記述方法です。ただし、古くからある既存のSAP標準プログラムではサブルーチンが使われているので、言語仕様として完全に廃止されることは当面無いと思います。

なので、今どきのプログラムらしくクラスを使用しています。

### 2. インライン宣言を使用する
変数のスコープは最小にする、というのが言語を問わずプログラミングの原則ですが、以前のABAPでは変数を使うその場で宣言することはできず、事前にDATA文で宣言してその後で使う、という書き方が必要でした。インライン宣言（ [Inline Declaration - ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abendata_inline.htm) ）の登場で他言語のように必要なその場で変数宣言することが可能になりました。

これも最近の新しい記述方法と思っている人が多いようですが、実は2013年登場というそれなりに古くからあるものです。当該バージョンのNetWeaver ASサーバが実際に使われるようになったのはS/4HANAになってからの2015年以降だとおもいますが、それでも「新しい」というほどのものではないです。

[ABAP News for Release 7.40 – Inline Declarations](https://blogs.sap.com/2013/05/23/abap-news-for-release-740-inline-declarations/)

インライン宣言を使う方がプログラムが簡潔に記述できるので、できるだけインライン宣言を使用しています。

## 実行方法

ソースコードを見てもらうとわかるように、しりとりに使用する単語（日本語のポケモン名）を品目マスタに持つようにしています。

https://github.com/saka-y/ExcelSapTools にある ExcelBatchInputTool.xlsm の「sample」シートに品目マスタ登録用データがありますので、このシートを使って登録してください。
日本版Best Practices環境向けに、品目タイプ：UNBW、品目コード：Pxxxx、プラント：1510としています。異なる環境で使用する場合は登録シートの内容を変更して、合わせてソースコード75行目からのSELECT内容を変更してください。

