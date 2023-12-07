### 応用プログラミング3
### 第7回　tidyverseによるデータハンドリング (1)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 2.2 tidy dataとは

# untidy / messy (整然としていない) データの例
# tidyrパッケージに付属する、2000年のビルボードヒットチャートのデータ
tidyr::billboard

# tidyrパッケージの pivot_longer() 関数でtidy dataに変換する (詳細は後述)
library(tidyverse)
# from: https://jmledford3115.github.io/datascibiol/lab8_1.html
billboard2 <- 
  billboard %>% 
  pivot_longer(wk1:wk76, # a range of columns
               names_to = "week",
               values_to = "rank",
               values_drop_na = TRUE #this will drop the NA's
               )
billboard2

#--------------------------------------------------------------------#

# 2.4 パイプによる処理の効率化

# パイプを使わないパターン
# with() 関数、by() 関数については以下を参考に
# Quick-R: Using With and By https://www.statmethods.net/stats/withby.html
with(iris, by(Petal.Length, Species, mean))

# tidyverseによる処理
library(tidyverse)

# irisデータからPetal.Length, Species列を選択し、Speciesでグループ化、
# ラベル (品種) ごとに平均を算出してデータフレームに格納する
df <- iris %>% select(Petal.Length, Species) %>%
               group_by(Species) %>% 
               summarise(avg = mean(Petal.Length))
df

#--------------------------------------------------------------------#

# 3.1 readrパッケージによるデータの読み込み

library(tidyverse) # library(readr)

df <- read_csv("day.csv",
    col_types = cols(instant = "-", season = col_factor(), yr = col_factor(),
                     mnth = col_factor(), holiday = col_factor(),
                     weekday = col_factor(levels = as.character(0:6)),
                     workingday = col_factor(),
                     weathersit = col_factor(levels = as.character(1:4))))

View(df)

#--------------------------------------------------------------------#

# 3.2 readxlパッケージによるExcelファイルの読み込み

library(tidyverse)
library(readxl)

# 列の型を指定して読み込む
df <- read_excel("customer_data.xlsx",
                 col_types = c("text", "text", "text", "numeric", "text", "text", "text"))

View(df)

#--------------------------------------------------------------------#

# 3.3 writexlパッケージによるExcelファイルへの書き出し

library(tidyverse)
library(writexl)

df <- iris %>% select(Petal.Length, Species) %>%
               group_by(Species) %>% 
               summarise(avg = mean(Petal.Length))

write_xlsx(df, "iris_modified.xlsx")

#--------------------------------------------------------------------#

# 4.3 select() 関数による列の選択

library(tidyverse) # library(dplyr)

df <- read_csv("tokyo_weather_2022.csv")
df[["年月日"]] <- lubridate::ymd(df[["年月日"]])

View(df)

df %>% select(年月日, 平均気温)

df %>% select(-contains("気温"))

# 参考: その他のヘルパー関数

# * starts_with(): 指定した文字列から始まる列を選択
# * ends_with(): 指定した文字列で終わる列を選択
# * contains(): 指定した文字列を含む列を選択
# * matches(): 正規表現で文字列を指定して列を選択
# * num_range(): 列名に連番を含む場合、範囲を指定して列を選択

# num_range() の例

tidyr::billboard %>% select(artist, track, num_range("wk", 30:39))

#--------------------------------------------------------------------#

# 4.4 filter() 関数による行の抽出

library(tidyverse)

df <- read_csv("tokyo_weather_2022.csv")
df[["年月日"]] <- lubridate::ymd(df[["年月日"]])


df %>% filter(最高気温 >= 35)

df %>% filter(年月日 >= "2020-12-01" & 平均気温 >= 11)

df %>% filter(between(平均気温, 20, 24))

#--------------------------------------------------------------------#

# 4.5 mutate() 関数による列の追加

library(tidyverse)

df <- read_csv("tokyo_weather_2022.csv")
df[["年月日"]] <- lubridate::ymd(df[["年月日"]])


df <- df %>% mutate(気温差 = 最高気温 - 最低気温)
df <- df %>% mutate(日照時間 = 日照時間 * 60) # 分に変換

View(df)

# 桜は2月1日からの累積気温が600度に達すると開花するらしい
df %>%
    filter(年月日 >= "2020-02-01") %>%
    mutate(累積気温 = cumsum(最高気温)) %>%
    select(年月日, 累積気温)

#--------------------------------------------------------------------#

# 4.6 summarise() 関数による要約

library(tidyverse)

df <- read_csv("tokyo_weather_2022.csv")
df[["年月日"]] <- lubridate::ymd(df[["年月日"]])


df %>% summarise(年平均気温 = mean(平均気温))


# 最頻値を算出する関数
Mode = function(x){
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
         mod = NA
    else
         if(is.numeric(x))
    mod = as.numeric(names(ta)[ta == tam])
    else
         mod = names(ta)[ta == tam]
    return(mod)
}

df %>% summarise(年平均気温 = mean(平均気温), 天気概況_昼_最頻値 = Mode(天気概況_昼))

#--------------------------------------------------------------------#

# 4.7 across(), if_any(), if_all() 関数

# 「気温」を含む列の値を華氏に変換する
df %>%
    mutate(across(contains("気温"), ~ . * 1.8 + 32, .names = "{col}_華氏")) %>%
    select(contains("気温")) # 結果確認のため


#--------------------------------------------------------------------#

# 4.8 group_by() 関数によるグループ化

library(tidyverse)

df <- read_csv("day.csv",
    col_types = cols(season = col_factor(),
                     yr = col_factor(), mnth = col_factor(),
                     holiday = col_factor(), 
                     weekday = col_factor(levels = as.character(0:6)),
                     workingday = col_factor(), 
                     weathersit = col_factor(levels = as.character(1:4))))

df %>%
    group_by(yr, mnth) %>%
    summarise(平均気温 = mean(temp), 平均利用者数 = mean(cnt)) %>%
    mutate(前月比 = 平均利用者数 / lag(平均利用者数))


# グループ化した状態での mean(cnt) はグループ内の平均を返す
df %>%
    group_by(yr, mnth) %>%
    mutate(rel_cnt = cnt / mean(cnt)) %>%
    select(rel_cnt)

# グループ化を解除すると、全体の平均を返す
df %>%
    group_by(yr, mnth) %>%
    ungroup() %>%
    mutate(rel_cnt = cnt / mean(cnt)) %>%
    select(rel_cnt)

#--------------------------------------------------------------------#

# 5.2 pivot_longer() 関数による縦持ちデータへの変換

# 横持ちデータ
head(iris)

# 縦持ちデータへの変換
long_iris <- iris %>%
    rowid_to_column("id") %>%
    pivot_longer(Sepal.Length:Petal.Width, names_to = "type", values_to = "value")

View(long_iris)

#--------------------------------------------------------------------#

# 5.3 pivot_wider() 関数による横持ちデータへの変換

wide_iris <- long_iris %>%
    pivot_wider(id_cols = "id", names_from = "type", values_from = "value")

View(wide_iris)
