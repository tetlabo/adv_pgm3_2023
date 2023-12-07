### 応用プログラミング3
### 第2回　Rプログラミングの基本、最近の動向について
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太



# 2.1 データフレーム

# 定数letters (アルファベット小文字) の先頭10要素を代入
a <- letters[1:10]
b <- sample(x = 1:100, size = 10) # 1から100の範囲から10個の乱数を抽出 (一様分布)
df <- data.frame(a, b) # ベクトルaとbをデータフレームの列として結合
df

#--------------------------------------------------------------------#

# 2.2 ファイルからの読み込み

df <- read.csv("pseudo_sake_data.csv", fileEncoding = "UTF-8") # 疑似日本酒データを読み込む


tail(df, n = 1) # データの末尾行を出力


df <- read.csv("small_na_sample.csv", na.strings = "") # 文字列型の列で値が入っていないセルを欠損値として扱う
df

#--------------------------------------------------------------------#

# 2.3 データフレームの操作 (1)

df <- read.csv("pseudo_sake_data.csv", fileEncoding = "UTF-8")
df[334, ] # データフレームの334行目を表示


df[, 4] # データフレームの4列目を表示

#--------------------------------------------------------------------#

# 2.4 データフレームの操作 (2)

df[334, 5:6] # 334行目の5列目と6列目を表示


df$エキス分 # エキス分列を表示


df$エ # 実は完全一致でなくても参照できてしまう


df[["エキス分"]] # エキス分列を表示

#--------------------------------------------------------------------#

# 2.5 データフレームの操作 (3)

df <- read.csv("pseudo_sake_data.csv", fileEncoding = "UTF-8")


df[df[["タイプ"]] == "吟醸酒", ][1:3, ] # 吟醸酒のデータのみ抽出する


df[df[["アルコール分"]] >= 17, ] # アルコール分が17%以上のデータを抽出する


df[df[["タイプ"]] == "吟醸酒" & df[["アルコール分"]] >= 17, ] # 複数の条件を評価するには & または | (OR) を使う

#--------------------------------------------------------------------#

# Q. データフレーム df から、総酸が1未満、精米歩合が20 (%) 以下の条件に
# 合致する行を抽出してください

df[] # 適切なプログラムを記述してください

#--------------------------------------------------------------------#

# 3.1 データの行数、列数の確認

nrow(df)


ncol(df)


dim(df)

#--------------------------------------------------------------------#

# 3.2 質的 (カテゴリー) 変数の集計

df <- read.csv("pseudo_sake_data.csv", header = TRUE, fileEncoding = "UTF-8") # 疑似日本酒データを読み込む


sort(table(df[["タイプ"]]), decreasing = TRUE) # sort(decreasing = TRUE) 関数で頻度の降順に並べ替え


prop.table(sort(table(df[["タイプ"]]), decreasing = TRUE)) * 100

#--------------------------------------------------------------------#

# 3.3 量的変数の集計

data(airquality) # 組み込みのairqualityデータセットを使う


wind_cut1 <- cut(airquality[["Wind"]], breaks = 10) # 1日の平均風速 (mph) を10等分する
                                                    # デフォルトでは "始点" より大きく、"終点" 以下
table(wind_cut1)


wind_cut2 <- cut(airquality[["Wind"]], breaks = seq(0, 22, 2)) # 任意の間隔 (2mphごと) に等分する
                                                               # seq(始点, 終点, 刻み) で連続したベクトルを生成する
table(wind_cut2)

#--------------------------------------------------------------------#

# 4.2 hist() 関数

data(iris) # 組み込みのirisデータを使用する


par(family = "IPAexGothic") # 日本語フォントの設定
hist(iris[["Sepal.Length"]]) # Sepal.Length列のデータの分布をヒストグラムで描画する


pretty_breaks <- pretty(range(iris[["Sepal.Length"]]), n = 20) # breaksオプションで区間を指定する
                                                               # pretty() 関数は数値の範囲を切りの良い間隔で分けてくれる
pretty_breaks

par(family = "IPAexGothic")
hist(iris[["Sepal.Length"]], breaks = pretty_breaks)

#--------------------------------------------------------------------#

# 4.3 plot() 関数

par(family = "IPAexGothic")
plot(iris[, c("Sepal.Length", "Petal.Length")]) # irisデータのSepal.Length, Petal.Lengthの散布図を描画する


par(family = "IPAexGothic")
plot(iris[, c("Sepal.Length", "Petal.Length")], cex = 3, col = iris[["Species"]]) # データ点のサイズを大きくし、Speciesの値で色分けする

#--------------------------------------------------------------------#

# 4.4 pairs() 関数

# 5列目 (Species) は数値ではないため除外
# panel.smoothでデータ点の近似曲線を描画する
par(family = "IPAexGothic")
pairs(iris[, -5], panel = panel.smooth, col = iris[["Species"]])

#--------------------------------------------------------------------#

# 4.5 barplot() 関数

df <- read.csv("tokyo_weather_2022.csv") # 気象庁Webサイトよりダウンロードした2020年の気象データを使用する


tbl <- sort(table(df[["天気概況_昼"]])) # 日中の天気概況を集計し、頻度で並べ替える


par(mar = c(4, 14, 2, 2), family = "IPAexGothic") # ラベルがはみ出さないためにマージンを調整する
barplot(tbl, horiz = TRUE, las = 2) # horiz = TRUEで横置きに、las = 2でラベルも横向きに

#--------------------------------------------------------------------#

# Q. 疑似日本酒データを読み込み、「タイプ」別の「容器」の度数を
# 集計し、棒グラフで表現してください

df <- read.csv("pseudo_sake_data.csv", fileEncoding = "UTF-8") # 疑似日本酒データを読み込む

tbl <-  # 「容器」と「タイプ」のクロス集計表を作成してください

(, col = 1:nrow(tbl), legend.text = rownames(tbl), args.legend = list(cex = 0.45, x = "topright")) # 棒グラフを作成してください

#--------------------------------------------------------------------#

# 5.2 パッケージの読み込み

vec <- sample(0:9, 20, replace = TRUE)


vec


nnzero(vec) # エラーになる


library(Matrix) # パッケージを読み込む
nnzero(vec) # ゼロでない値の数を出力する

#--------------------------------------------------------------------#

# 5.3 パッケージのインストール

install.packages("TeachingDemos") # 統計学の学習のためのTeachingDemosパッケージをインストール


library(TeachingDemos)

dice(ndice = 3, plot.it = TRUE) # サイコロを振るシミュレーションのための dice() 関数を実行

#--------------------------------------------------------------------#

# 5.4 GitHub上のパッケージ

install.packages("remotes") # remotesパッケージはCRANからインストールする


remotes::install_github("tetlabo/nothing") # 筆者のリポジトリから、「ほぼ何もしない」パッケージをインストールする


library(nothing)


hello() # ただ、"Hello, World!" と返すだけの関数

#--------------------------------------------------------------------#

# 6.3 tidyverseを活用したプログラムの例

library(tidyverse)


df <- iris %>% select(Petal.Length, Species) %>%   # irisデータからPetal.Length, Species列を選択し、Speciesでグループ化、
               group_by(Species) %>%               # ラベル (品種) ごとに平均を算出してデータフレームに格納する
               summarise(avg = mean(Petal.Length))
df


ggplot(df, aes(x = Species, y = avg, fill = Species)) +
        geom_bar(stat = "identity") +
        theme(legend.position = "none") # ggplot2パッケージでグラフィックスを描画
