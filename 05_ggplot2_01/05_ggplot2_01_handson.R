### 応用プログラミング3
### 第5回　ggplot2パッケージによるグラフィックス作成 (1)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 3.1 ggplot2の典型的なプログラム

library(ggplot2)

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
    geom_point(size = 4) +
    theme_classic(base_size = 16)

#--------------------------------------------------------------------#

# 3.3 ggplot() 関数

library(ggplot2) # library(tidyverse)
p <- ggplot()
p

#--------------------------------------------------------------------#

# 3.4 aes() 関数

p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species, shape = Species))
p <- p + geom_point(size = 4)
p

#--------------------------------------------------------------------#

# 3.6 theme_*() 関数

p <- ggplot(iris, aes(x = Sepal.Length,
        y = Petal.Length, colour = Species)) +
        geom_point(size = 4)


p + theme_classic(base_size = 16)


p + theme_dark(base_size = 16)


p + theme_minimal(base_size = 16)


library(ggthemes)

# 古いExcel風テーマ
p + theme_excel(base_size = 16) + scale_colour_excel()


# 新しいExcel風テーマ
p + theme_excel_new(base_size = 16) + scale_colour_excel_new()

#--------------------------------------------------------------------#

# 4.2 ggsave() 関数
# 画面に表示する場合とファイルに出力する場合で
# サイズの単位が違うようで、数値の指定に注意が必要

p <- ggplot(iris, aes(x = Sepal.Length,
        y = Petal.Length, colour = Species)) +
        geom_point(size = 2) +
        theme_classic(base_size = 4)

ggsave("handson_output01.png", p)

ggsave("handson_output02.png", p, device = ragg::agg_png)


# 日本語を含む場合、標準のグラフィックス
# デバイスでは文字化けすることがある
p <- ggplot(iris, aes(x = Sepal.Length,
        y = Petal.Length, colour = Species)) +
        geom_point(size = 2) +
        theme_classic(base_size = 4) +
        ggtitle("irisデータの散布図")

ggsave("handson_output03.png", p)

ggsave("handson_output04.png", p, device = ragg::agg_png)

#--------------------------------------------------------------------#

# 5.1 ヒストグラム

library(tidyverse)

df <- read_csv("day.csv")
df[["yr"]] <- as.factor(df[["yr"]])


# 標準化された気温の分布を描く
ggplot(df, aes(x = temp)) +
    geom_histogram()


# yr列ごとにヒストグラムを描き分ける
ggplot(df, aes(x = temp, fill = yr)) +
    geom_histogram(position = "identity",
    alpha = 0.4)


# ビンの数を10個 (10等分) に指定する
ggplot(df, aes(x = temp, fill = yr)) +
    geom_histogram(position = "identity",
    alpha = 0.4, bins = 10)


# ビンの幅を0.02に指定する
ggplot(df, aes(x = temp, fill = yr)) +
    geom_histogram(position = "identity",
    alpha = 0.4, binwidth = 0.02)


# 縦横を入れ替える (xとyの指定を入れ替える)
ggplot(df, aes(y = temp, fill = yr)) +
    geom_histogram(position = "identity",
    alpha = 0.4, binwidth = 0.02)


# 密度を描く geom_freqpoly() を使う
ggplot(df, aes(x = temp, y = after_stat(density), colour = yr)) +
    geom_freqpoly(position = "identity",
    alpha = 0.4, binwidth = 0.02, size = 2)

#--------------------------------------------------------------------#

# 5.2 散布図

# day.csvを使用
df <- read_csv("day.csv")
df[["yr"]] <- as.factor(df[["yr"]])


# 標準化された気温 (temp) と利用者数 (cnt) の関係を示す散布図を描く
ggplot(df, aes(x = temp, y = cnt)) +
    geom_point(size = 1.5)


# データ点を年 (yr) ごとに描き分ける
ggplot(df, aes(x = temp, y = cnt, shape = yr, colour =yr)) +
    geom_point(size = 3, alpha = 0.4)


# 散布図自体を年 (yr) ごとに分割する
# facet については次回取り上げる
ggplot(df, aes(x = temp, y = cnt)) +
    geom_point(size = 1.5) +
    facet_wrap(~yr)

# 散布図自体を年 (yr) と平日 / 休日フラグ (holiday)
# ごとに分割する。facet については次回取り上げる
ggplot(df, aes(x = temp, y = cnt)) +
    geom_point(size = 1.5) +
    facet_wrap(holiday~yr)


# データ点の大きさで3次元目をあらわすバブルチャート
# 気温をx、利用者数をy、一時利用者数をz (点の大きさ) とする
ggplot(df, aes(x = temp, y = cnt, shape = yr, colour =yr, size = casual)) +
    geom_point(alpha = 0.4)

#--------------------------------------------------------------------#

# 5.3 棒グラフ

# day.csvを使用
df <- read_csv("day.csv")
df[["yr"]] <- as.factor(df[["yr"]])

# 天気 (weathersit) ごとの件数を棒グラフで描く
# デフォルトではデータの件数を自動的にカウントする
ggplot(df, aes(x = weathersit)) +
    geom_bar()


# 年 (yr) ごとの利用者数の合計を棒グラフで描く
# stat = "summary", fun = "sum" でデータの値を合計する
ggplot(df, aes(x = yr, y = cnt)) +
    geom_bar(stat = "summary", fun = "sum")


# 年 (yr) ごとの利用者数を一時利用者と定期利用者の積み上げ棒グラフで描く

# tidy dataに変換する (詳細は次の機会に)
df_tmp <- select(df, yr, casual, registered) %>% pivot_longer(!yr, names_to = "type", values_to = "cnt")

ggplot(df_tmp, aes(x = yr, y = cnt, fill = type)) +
    geom_bar(stat = "summary", fun = "sum")


# 年 (yr) ごとの利用者数を一時利用者と定期利用者の帯グラフで描く

# tidy dataに変換する (詳細は次の機会に)
df_tmp <- select(df, yr, casual, registered) %>% pivot_longer(!yr, names_to = "type", values_to = "cnt")

ggplot(df_tmp, aes(x = yr, y = cnt, fill = type)) +
    geom_bar(stat = "summary", fun = "sum", position = "fill") +
    scale_y_continuous(labels = scales::percent)
