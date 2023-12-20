### 応用プログラミング3
### 第12回　Rによるデータスクレイピング
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 3.2 GET() 関数による受信 (1)

library(tidyverse)
library(httr)

url <- "https://httpbin.org/get"


res <- GET(url)


#--------------------------------------------------------------------#

# リファラーを付与する例
res <- GET(url, config = add_headers(referer = "https://www.google.co.jp/search?q=httr+r"))
res


#--------------------------------------------------------------------#

# UserAgentを設定 (偽装) する例
res <- GET(url, user_agent("httr package"))
res


#--------------------------------------------------------------------#

# 3.3 GET() 関数による受信 (2)

# HTMLを取得し、xml2::read_html() で処理
url <- "https://httpbin.org/html"
res <- GET(url)
headers(res)
status_code(res)
content(res)


#--------------------------------------------------------------------#

# HTMLを取得し、テキストとして出力
content(res, "text")


#--------------------------------------------------------------------#

# HTMLを取得し、jsonlite::fromJSON() で処理
url <- "https://httpbin.org/json"
res <- GET(url)
headers(res)
status_code(res)
content(res)


#--------------------------------------------------------------------#

url <- "https://httpbin.org/get"
query_res <- GET(url, query = list(key1 = "test", key2 = "テスト"))
content(query_res)


#--------------------------------------------------------------------#

# 3.4 POST() 関数による送信

url <- "https://httpbin.org/post"
res <- POST(url, body = "これはテストです")
res
content(res)$data


#--------------------------------------------------------------------#

body <- list(a = 1, b = 2, c = 3)
res <- POST(url, body = body)
res
content(res)$form


content(res)$data # dataは空


#--------------------------------------------------------------------#

# ファイルをアップロードする例
res <- POST(url, body = upload_file("test.txt"))


content(res)$data


#--------------------------------------------------------------------#

# 4.3 read_html() 関数によるWebアクセス

library(rvest)

html <- read_html("https://rvest.tidyverse.org/")


html


#--------------------------------------------------------------------#

res <- GET("http://rvest.tidyverse.org/")
class(content(res))


#--------------------------------------------------------------------#

# 4.4 html_elements() 関数による要素へのアクセス

# pタグをすべて出力
html <- read_html("./simple_example.html")
html %>% html_elements("p")


#--------------------------------------------------------------------#

# CSSセレクターによる指定
html %>% html_elements("p > a")


#--------------------------------------------------------------------#

# pタグの最初の1つを出力
html %>% html_element("p")


#--------------------------------------------------------------------#

# imgタグを出力 (Base64エンコードされた画像)
html %>% html_element("img")


#--------------------------------------------------------------------#

# 4.5 html_attrs() / html_attr() による属性の取得

html %>% html_elements("p > a") %>% html_attrs()


html %>% html_elements("p > a") %>% html_attr("href")


#--------------------------------------------------------------------#

# 4.6 html_text() 関数によるテキストの取得

html %>% html_elements("p") %>% html_text()


html %>% html_elements("p") %>% html_text2()


#--------------------------------------------------------------------#

# 4.7 html_table() 関数による表構造データの取得

url <- "https://db.netkeiba.com/?pid=jockey_leading"


html <- read_html(url, encoding = "EUC-JP") # イマドキEUCだった!


# rowspan属性のせいで見出しの読み込みが意図しないものになる
res <- html %>% html_elements("table") %>% html_table()


# 列名を結合する
colnames(res[[1]])[9:18] <- paste(colnames(res[[1]])[9:18], res[[1]][1, 9:18], sep = "_")


# 1行目を削除して、型を自動判定する (readr::type_convert() 関数)
df <- res[[1]][-1, ] %>% type_convert()


View(df)


#--------------------------------------------------------------------#

# Yahoo! ニュースのトピックを取得する例

topics <- read_html("https://www.yahoo.co.jp/") %>%
    html_element("#tabpanelTopics1 > div > div") %>%
    html_nodes("li") %>%
    html_text2() %>%
    str_replace_all("NEW$", "")


topics
