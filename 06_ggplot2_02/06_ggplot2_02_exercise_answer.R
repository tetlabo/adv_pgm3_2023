### 応用プログラミング3
### 第6回　ggplot2パッケージによるグラフィックス作成 (2)
### 週次課題　解答


#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#

# Q1: データとパッケージの読み込み

# Q1-1: tidyverseパッケージを読み込んでください。
# (自動的にreadr, ggplot2パッケージも読み込まれます)

library(tidyverse)

# Q1-2: weather_data.csvをreadrパッケージの
# read_csv() 関数で読み込んでください。

df <- read_csv("./weather_data.csv")

#--------------------------------------------------------------------#

# 注: 前回と今回は ggplot2 パッケージについての内容なので、以下の「グラフを
# 描いてください」という課題については、すべて ggplot2 の文脈でプログラミングし、
# 回答してください。

#--------------------------------------------------------------------#

# Q2: 折れ線グラフ

# Q2-1: データフレーム df の「date_time」列と「tempc」列 (屋外の気温) について、
# x軸をdate_time、y軸をtempcとして折れ線グラフを作成してください。
# グラフをファイルとして保存する必要はありません。

ggplot(df, aes(x = date_time, y = tempc)) +
    geom_line()

#--------------------------------------------------------------------#

# Q2-2: データフレーム df の「date_time」列と「tempc」列 (屋外の気温)
# および「tempinc」(屋内の気温) について、x軸をdate_timeとし、tempcとtempincの値を
# それぞれ折れ線グラフとして、1枚のグラフィックスの中に重ねて描画してください。
# 単位が同じなので、軸は片側だけでよいです。また、線の区別をつけるために、
# tempcの色 (colour) を "red"、tempincの色を "blue" と指定してください。
# グラフをファイルとして保存する必要はありません。

ggplot(df, aes(x = date_time, y = tempc)) +
    geom_line(colour = "red") +
    geom_line(aes(y = tempinc), colour = "blue")

#--------------------------------------------------------------------#

# Q3: 箱ひげ図とバイオリンプロット

# Q3-1: データフレーム df の「humidity」(屋外の湿度) 列について、箱ひげ図を作成してください。
# 「ひげ」の両端には横棒も示してください。

ggplot(df, aes(y = humidity)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot()

#--------------------------------------------------------------------#

# Q3-2: データフレーム df の「humidity」列について、週別のバイオリンプロットを作成してください。

# tidyverseにおいて日付データを扱うlubridateパッケージを読み込む
library(lubridate)

# データから日時と湿度のみを選択し、週単位でグループ化する (週の開始日をfactor型に変換する)
df_week <- df %>% select(date_time, humidity) %>% group_by(week = as.factor(floor_date(date_time, unit = "week")))

# 以下を埋めてください
ggplot(df_week, aes(x = week, y = humidity, fill = week)) +
    geom_violin()


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
