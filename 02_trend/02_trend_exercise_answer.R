### 応用プログラミング3
### 第2回　Rプログラミングの基本、最近の動向について
### 週次課題　解答


#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#

# Q1: データフレームの作成
# 以下の3つのベクトルを作成し、データフレーム df として結合してください。
# ベクトル ID: 1から10の連番
# ベクトル group: AからJの連続したアルファベット (Rの定数を使用してください)
# ベクトル value: 標準正規分布 (平均0、標準偏差1) に従う乱数10個

ID <- 1:10
group <-LETTERS[1:10]
value <- rnorm(10)

df <- data.frame(ID, group, value)

# 結果の確認
df

#--------------------------------------------------------------------#

# Q2: データフレームの操作 (1)
# 作成したデータフレーム df について、以下の操作を実行できるプログラムを記述してください。

# Q2-1: ID列の値を2倍、value列の値を3倍した結果を df に代入し、df 全体を表示してください。
# 出力イメージ
# 元の df
# > df
# ID  group  value
#  1      A  -0.9465565
#  2      B  0.3408036
#  3      C  0.2885507
# ...
#
# 処理後の df
# > df
#  2      A  -2.83967
#  4      B  1.022411
#  6      C  0.8656521
# ...

df[["ID"]] <- df[["ID"]] * 2
df[["value"]] <- df[["value"]] * 3

df

# Q2-2: value列の値がマイナスである行だけを抽出してください。
# ※乱数なのでマイナスの値が存在しない場合もあります。
# ※その場合でも、マイナスの値があった場合に抽出できるコードを記述してください。

# どのコードも同じ結果が得られます
df[df[["value"]] < 0, ]


df[df$value < 0, ]


df[df["value"] < 0, ]


subset(df, value < 0) # subset() 関数は未紹介

#--------------------------------------------------------------------#

# Q3: データフレームの操作 (2)
# ファイル 02_trend_exercise_data.csv を使い、以下の操作を実行できる
# プログラムを記述してください。
# データ出典: 株式会社KSP-SP "KSP-POSオープンデータ"
# "2021年 KSP-POS 食品スーパー新商品売れ筋ランキング 2021年37週 アイスクリーム・デザート類"
# https://www.ksp-sp.com/open_data/ranking/2021/202137.html

# Q3-1: ファイル 02_trend_exercise_data.csv を df として読み込んでください。
#       なお、データには欠損値があり、"x" として記録されています。(欠損値は筆者によるもの)
#       欠損値を x と指定して読み込んでください。

df <- read.csv("./02_trend_exercise_data.csv", na.strings = "x")

# Q3-2: df について、すべての行の「商品名称」と「平均売価」列を選択して出力してください。
#       列番号で指定しても、列名で指定しても構いません。

# どちらのコードも同じ結果が得られます
df[, c("商品名称", "平均売価")]


df[, c(3, 8)]


#--------------------------------------------------------------------#

# Q4: 質的 (カテゴリー) 変数の集計
# df について、メーカー名の度数を集計してください。
# 出力は、度数の降順に並べ替えてください。

tbl <- sort(table(df[["メーカー名"]]), decreasing = TRUE)
tbl

#--------------------------------------------------------------------#

# Q5: 散布図の作成
# df について、「平均売価」をx軸に、「金額PI」をy軸に配置した
# 散布図を作成してください。その際、x軸の範囲を (0, 350) に、
# y軸の範囲を (0, 1000) に指定してください。
# 描画範囲の指定について参考: http://cse.naro.affrc.go.jp/takezawa/r-tips/r/48.html


# どちらのコードも同じ結果が得られます (軸ラベルが異なります)
plot(df[, c("平均売価", "金額PI")],
    xlim = c(0, 350),
    ylim = c(0, 1000))


plot(df$平均売価, df$金額PI,
    xlim = c(0, 350),
    ylim = c(0, 1000))

#--------------------------------------------------------------------#

# Q6: 棒グラフの作成
# Q4で行った集計結果をもとに、ランキングに登場するメーカー名の度数を
# 棒グラフで描画してください。その際、左から右に度数が降順に並ぶ
# ように指定してください。また、メーカー名のラベルが縦 (左に90度
# 倒れた状態) になるようにしてください。
# ラベルの回転について参考: https://data-science.gr.jp/implementation/ida_r_barplot.html

par(mar = c(12, 4, 4, 2)) # 下側のマージンを調整する (採点対象外)
barplot(tbl, las = 2)

#--------------------------------------------------------------------#

# Q7: パッケージの活用

# Q7-1: GitHubから、"tubeplayR" パッケージをインストールしてください。
#       https://github.com/kazutan/tubeplayR
#       サイトの "install" 節を参考にしてください。
#       必要があれば、「パッケージをインストールするためのパッケージ」も
#       インストールしてください。

# どちらのコードも同じ結果が得られます
install.packages("remotes")
remotes::install_github("kazutan/tubeplayR")


# install.packages("devtools")
# devtools::install_github("kazutan/tubeplayR")


# Q7-2: tubeplayRパッケージを読み込み、何か適当なYouTube動画のURLを引数に
#       tubeplay() 関数を実行してください。

library(tubeplayR)
tubeplay("https://www.youtube.com/watch?v=qqgQIqt4s1U")

#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
