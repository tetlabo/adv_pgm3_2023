### 応用プログラミング3
### 第12回　Rによるデータスクレイピング
### 週次課題　解答


#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#


# Q1: Qiita APIからのデータ収集と整形
#     Qiitaは、技術ブログサービスとして、多数のユーザーによって幅広い話題が
#     投稿されています。その記事を検索したりできるAPIが提供されています。
#     ユーザー登録しなくても、IPアドレスごとに1時間に60回までのアクセスが可能です。
#
#     * API仕様: https://qiita.com/api/v2/docs#get-apiv2items
#     * 検索クエリオプション: https://help.qiita.com/ja/articles/qiita-search-options
#     * 参考: Qiita APIを使って記事一覧を取得する https://qiita.com/koki_develop/items/57f86a1abc332ed2185d

# Q1-1: この問題では、Qiita APIの仕様を確認し、以下の条件を満たす記事の情報を
#       取得し、データフレームとして格納してください。
#
#     条件1: タグに「R」が指定されている
#     条件2: 2022年に作成された
#     条件3: 直近100件の記事

library(tidyverse)
library(httr)
library(jsonlite)


url <- "https://qiita.com/api/v2/items?page=1&per_page=100&query=tag:R+created:>2022-01-01"


res <- GET(url)


df <- fromJSON(content(res, "text"))

# Q1-2: 取得したデータフレームから、以下の要素 (列) のみを、この順番で抽出して表示してください。
#       * 記事タイトル
#       * URL
#       * タグ
#       * いいね！ (Qiitaでは "LGTM！") の数

df %>% select(title, url, tags, likes_count)

#--------------------------------------------------------------------#

# Q2: 日本プロ野球Webサイトからのスクレイピングとデータ処理
#
# Q2-1: NPB ( https://npb.jp/ ) の2021年シーズンチーム打撃成績ページから、
#       セ・リーグとパ・リーグそれぞれについて、データが記述されている表 (<table>) を
#       スクレイピングし、別々のデータフレームに格納してください。
#
#       * セ・リーグチーム打撃成績: https://npb.jp/bis/2021/stats/tmb_c.html
#       * パ・リーグチーム打撃成績: https://npb.jp/bis/2021/stats/tmb_p.html
#
#       ヒント: Webページには<table>タグが2つ含まれます。そのうち、打撃成績が含まれる
#              表は最初の1つになります。複数の "xml_nodeset" から任意の位置の要素を
#              取得するには、dplyr パッケージの nth() (n番目) 関数を使います。
#              引数に、取り出したい要素の番号を指定します。

library(rvest)

central_url <- "https://npb.jp/bis/2021/stats/tmb_c.html"
pacific_url <- "https://npb.jp/bis/2021/stats/tmb_p.html"

central_df <- read_html(central_url) %>% html_element("table") %>% html_table()
pacific_df <- read_html(pacific_url) %>% html_element("table") %>% html_table()


# Q2-2: 球団別のOPSを計算しましょう。OPSは、野球をデータ分析の観点から楽しむ
#       セイバーメトリクスにおいて提案された指標で、打撃能力を的確に表現しているとされます。
#       OPSは、"出塁率 + 長打率" で求められます。各リーグのデータフレームの末尾列に、
#       "OPS" という名前で、球団別のOPSを計算して格納してください。
#       列を追加する際、データフレームを上書きで使用してください。

central_df <- central_df %>% mutate(OPS = 出塁率 + 長打率)
pacific_df <- pacific_df %>% mutate(OPS = 出塁率 + 長打率)


View(central_df)


View(pacific_df)

#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
