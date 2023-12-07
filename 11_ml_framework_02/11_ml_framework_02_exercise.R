### 応用プログラミング3
### 第12回　機械学習フレームワークによる機械学習 (2)
### 週次課題用プログラム
### 提出期限:
### 
### 提出者の情報を記述してください
### 学籍番号: 
### 氏名: 


#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#

# Q: 何度か使用している、Bike sharing datasetの hour.csv について、
#    利用者数 cnt を目的変数として、ランダムフォレストを用いた回帰モデルを
#    作成してください。その際、前回と今回紹介した機械学習フレームワークを
#    活用し、できるだけ見通しの良い (コードがすっきりした) プログラムを
#    作成するよう意識してください。また、今回紹介したさまざまなデータ加工 (前処理)
#    テクニックを活用し、できるだけ予測精度が高くなるよう努めてください。
#    なお、ひとつの目標として、いろいろがんばると、誤差 (RMSE) が60程度まで
#    到達できます。もちろん、到達しないから減点、などはありません。
#    ※ これまで、データ量の観点で日ごとのデータ day.csv を使うことが多かった
#    ですが、今回の課題は時間ごとのデータ hour.csv を使いますので、注意してください。

# パッケージの読み込み
# その他、必要なものがあれば適宜読み込んでください。
library(tidyverse)
library(lubridate)


# データの読み込み
df <- read_csv("hour.csv", col_types = cols(season = col_factor(), yr = col_factor(),
    mnth = col_factor(), hr = col_factor(), holiday = col_factor(),
    weekday = col_factor(levels = as.character(0:6)), workingday = col_factor(),
    weathersit = col_factor(levels = as.character(1:4))))

head(df)


# データの分割
# データを学習用と検証用に分割します。2012年6月30日までのデータでモデルを作成し、それ以降のデータを予測し、精度を検証してください。
# なお、単なる連番である instant と、cnt に直結する casual, registered は除きます。
df_train <- df %>% filter(dteday <= "2012-06-30") %>% select(-instant, -casual, -registered)
df_test <- df %>% filter(dteday >= "2012-07-01") %>% select(-instant, -casual, -registered)


# データの加工
# できるだけ精度が高くなるように、さまざまに加工してみてください。
# ただ、無理にすべてをtidymodelsで行う必要はありません。




# モデル作成
# ランダムフォレストをtidymodelsの文脈で使用するには、以下のようにします。
# https://parsnip.tidymodels.org/reference/rand_forest.html
# 前回のハンズオンのコードを参考にしてください。
# グリッドサーチは不要です。
# 以下のコードはあくまで例です。これだけでは動作しません。

rf_model <- rand_forest(mode = "regression", trees = 200) %>%
    set_engine("randomForest")


# データ加工レシピが df_rec であるとして
df_wflow <- workflow() %>%
    add_model(rf_model) %>%
    add_recipe(df_rec)


df_fit <- df_wflow %>% fit(data = df_train)


# 予測結果を格納
df_pred <- predict(df_fit, df_test)


# モデルの精度評価
# 回帰の場合の評価指標RMSEは、Metricsパッケージの
# rmse() 関数で算出できます。

library(Metrics)

# この結果が60近くまで小さくなれば大したもの (?) です。
rmse(df_test[["cnt"]], pred_A)


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
