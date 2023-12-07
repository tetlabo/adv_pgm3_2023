### 応用プログラミング3
### 第8回　tidyverseによるデータハンドリング (2)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 2.2 標準の日時データ操作関数 (1) Date型
# Posit CloudのタイムゾーンはUTCなので、夜間に本プログラムを実行すると
# 日本時間での日付とずれますが、ここでは特に問題としません。

today <- Sys.Date()


today


today + 1

#--------------------------------------------------------------------#

# 2.3 標準の日時データ操作関数 (2) POSIXct型

now <- Sys.time()
now

# Posit CloudのタイムゾーンはUTCなので、JST ("Asia/Tokyo") に変換する
# 変換後はCharacter型になっているので注意
format(now, tz = "Asia/Tokyo", usetz = TRUE)


now + 600 # 10分後


# Posit CloudのタイムゾーンはUTCなので、JST ("Asia/Tokyo") に変換する
# 変換後はCharacter型になっているので注意
format(now + 600, tz = "Asia/Tokyo", usetz = TRUE)

#--------------------------------------------------------------------#

# 2.4 標準の日時データ操作関数 (3) POSIXlt型

now <- Sys.time()


# POSIXlt型に変換する
# Posit CloudのタイムゾーンはUTCなので、JST ("Asia/Tokyo") に変換する
now <- as.POSIXlt(now, tz = "Asia/Tokyo")


now


attributes(now)


now$hour

#--------------------------------------------------------------------#

# 2.6 lubridateパッケージの日時データ操作関数 (1) オブジェクトの作成

# lubridateパッケージの読み込み
library(lubridate)

# 参考: 利用可能な日時指定関数
# ymd(), ydm(), mdy(), myd(), dmy(), dym(), yq(): 年月日に対応
# 
# ymd_hms(), ymd_hm(), ymd_h(), dmy_hms(), dmy_hm(), dmy_h(), 
# mdy_hms(), mdy_hm(), mdy_h(), ydm_hms(), ydm_hm(), ydm_h(): 年月日時刻に対応

today <- "2021-08-25"


class(today)


today <- ymd(today, tz = "Asia/Tokyo")


class(today)


now <- "2022-11-07 07:39:36 UTC"


class(now)


now <- ymd_hms(now, tz = "Asia/Tokyo")


class(now)


now2 <- "7日,11月 7時39分, 22年"
parse_date_time(now2, "dmHMy", tz = "Asia/Tokyo")

#--------------------------------------------------------------------#

# 2.7 lubridateパッケージの日時データ操作関数 (2) 日時情報の抽出と変更

# lubridateパッケージのタイムゾーンは標準でUTCなので、どの関数にもタイムゾーン指定が必要
now <- ymd_hms(with_tz(Sys.time(), tz = "Asia/Tokyo"), tz = "Asia/Tokyo")
now


year(now)


month(now, label = TRUE)


month(now, label = TRUE, locale ="C")


month(now, label = TRUE, locale ="ja_JP.UTF8")


wday(now, label = TRUE, locale = "ja_JP.UTF8")


wday(now, label = TRUE, locale = "zh_CN.UTF8")


year(now) <- 2022


hour(now) <- hour(now) + 1


now

#--------------------------------------------------------------------#

# 2.8 lubridateパッケージの日時データ操作関数 (3) 日時の演算

now <- ymd_hms(with_tz(Sys.time(), tz = "Asia/Tokyo"), tz = "Asia/Tokyo")
now


round_date(now, unit = "hour")


floor_date(now, unit = "hour")


ceiling_date(now, unit = "hour")


ceiling_date(now, unit = "month")


ceiling_date(now, unit = "month") - days(1)

#--------------------------------------------------------------------#

# 2.9 zipanguパッケージによる休日の処理

# remotes::install_github("uribo/zipangu")
library(zipangu)

is_jholiday("2021-07-23") # 2021年のみ「スポーツの日」


jholiday_spec(2021, "スポーツの日", lang = "jp")


jholiday_spec(2019, "体育の日", lang = "jp") # 2019年以前は「体育の日」

#--------------------------------------------------------------------#

# 3.2 標準の文字列データ操作関数

vec <- c("日本", "Japan", "ニホン", "にほん", "Japan")
grep("Japan", vec)


grepl("日本", vec)


gsub("Japan", "Japón", vec)

#--------------------------------------------------------------------#

# 3.4 stringrパッケージの文字列データ操作関数 (1) 検索

library(tidyverse) # library(stringr)

# rioパッケージは、csvファイルやExcelファイル、JSON、他ソフトのデータ、
# Googleスプレッドシートなどさまざまなフォーマットのデータを共通のインターフェースで
# 扱える "A Swiss-Army Knife for Data I/O" と称するパッケージ
library(rio)

# Web (政府CIOポータル) 上のExcelファイルを読み込む
# 地方自治体に対する「オープンデータ」に関するアンケート結果
df <- import(file = "https://cio.go.jp/sites/default/files/uploads/documents/r2_survey_comments.xlsx", sheet = 5, skip = 5)


df %>% mutate(結果1 = str_count(内容, "コロナ"), 結果2 = str_detect(内容, "データ")) %>% select(starts_with("結果"))


df %>% summarise(結果3 = str_which(内容, "アプリ"))

#--------------------------------------------------------------------#

# 3.5 stringrパッケージの文字列データ操作関数 (2) 抽出

df %>% mutate(結果 = str_extract(内容, "^...")) %>% select(結果) # 最初の3文字を抽出


df %>% summarise(結果 = str_subset(内容, "アプリ"))


df %>% mutate(結果 = str_match_all(内容, "アプリ")) %>% select(結果)


str_match_all(df[["内容"]], "アプリ")

#--------------------------------------------------------------------#

# 3.6 stringrパッケージの文字列データ操作関数 (3) 置換

# 「新型コロナウイルス感染症」を「COVID-19」に置換する
df %>% mutate(内容 = str_replace_all(内容, "(新型)*コロナ(ウイルス)*(感染症)*", "COVID-19")) %>% select(内容)


# コードを、8桁、左側をゼロ埋めする
df %>% mutate(地方公共団体コード = str_pad(地方公共団体コード, width = 8, side = "left", pad = 0)) %>% select(地方公共団体コード)


str_trim(" あいうえお   ")

#--------------------------------------------------------------------#

# 4.1　map() 系関数による繰り返し処理

iris %>% select(-Species) %>% map(mean)


iris %>% map_if(is.numeric, mean)

#--------------------------------------------------------------------#

# 4.2　`modify()` 系関数による繰り返し処理

iris %>% select(-Species) %>% modify(scale)


iris %>% modify_if(is.numeric, scale)

#--------------------------------------------------------------------#

# 5.1 DBIパッケージによるデータベースアクセス

library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "stationery.db")


dbListFields(con, "店舗マスター")


res <- dbSendQuery(con, "SELECT * FROM 店舗マスター")


dbFetch(res)

#--------------------------------------------------------------------#

# 5.2 jsonliteパッケージによるJSONデータの操作

library(jsonlite)


# お菓子の虜 Web API: 1996年から続いているコンビニお菓子情報「お菓子の虜」に掲載されている2000種以上のお菓子を、さまざまな検索軸で検索できる
url <- "https://sysbird.jp/toriko/api/?apikey=guest&format=json&keyword=ポテトチップス&max=10"


dat <- fromJSON(url)


dat[["item"]]


#--------------------------------------------------------------------#

# 5.3 xmlconvertパッケージによるXMLデータの操作

library(xmlconvert)


# XMLでレスポンスを得ることもできる
url <- "https://sysbird.jp/toriko/api/?apikey=guest&format=xml&keyword=ポテトチップス&max=10"


dat <- xml_to_df(url, records.tags = "item")


dat

#--------------------------------------------------------------------#

# 5.4 forcatsパッケージによるfactor型データの操作

library(tidyverse) #library(forcats)

# dplyrパッケージに付属するstarwarsデータは、スターウォーズの登場人物のプロフィールをまとめたもの
# species列が欠損値である行を除去し、speciesの上位3種を残して他を「その他」にまとめる
starwars %>% filter(!is.na(species)) %>%
    mutate(new_species = fct_lump(species, n = 3, other_level = "その他")) %>%
    count(new_species)

#--------------------------------------------------------------------#

# 5.5 tidyquantパッケージによる株価データの取得

# install.packages("tidyquant")
library(tidyquant)


# 富士通株式会社の2021年1月1日から本日までの株価を取得する
fj_stock <- tq_get("6702.T", get = "stock.prices", from = "2021-01-01")


library(ggplot2)
theme_set(theme_gray(base_family = "IPAexGothic", base_size = 14))

ggplot(fj_stock, aes(x = date, y = adjusted)) +
    geom_line() +
    labs(x = "", y = "")

#--------------------------------------------------------------------#

# 参考: 某マーケティングリサーチ企業の株価推移

mm_stock <- tq_get("3978.T", get = "stock.prices", from = "2017-03-22") # 日付は上場日


ggplot(mm_stock, aes(x = date, y = adjusted)) +
    geom_line() +
    annotate(geom = "rect", fill = "pink", alpha = 0.4, xmin = ymd("2016-04-01"), xmax = ymd("2018-03-31"), ymin = -Inf, ymax = Inf) +
    annotate(geom = "text", x = ymd("2017-01-01"), y = 3000, label = "筆者在職期間", size = 6) +
    annotate(geom = "text", x = ymd("2017-03-22"), y = 600, label = "上場", size = 6) +
    xlim(ymd("2016-01-01"), ymd("2023-08-31")) +
    labs(x = "", y = "") + ggtitle("某マーケティングリサーチ企業の株価推移")
