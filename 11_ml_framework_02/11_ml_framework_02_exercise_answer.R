### 応用プログラミング3
### 第11回　機械学習フレームワークによる機械学習 (2)
### 週次課題　解答


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

# readrとtidymodelsで col_factor() 関数がなぜか衝突するので、読み込むタイミングを意識する
library(tidymodels)

#--------------------------------------------------------------------#

# 比較のため、データ無加工で作成したモデル

# データの分割
# データを学習用と検証用に分割します。2012年6月30日までのデータでモデルを作成し、それ以降のデータを予測し、精度を検証してください。
# なお、datetime型である dteday と単なる連番である instant、cnt に直結する casual, registered は除きます。
df_train <- df %>% filter(dteday <= "2012-06-30") %>% select(-dteday, -instant, -casual, -registered)
df_test <- df %>% filter(dteday >= "2012-07-01") %>% select(-dteday, -instant, -casual, -registered)


rf_model <- rand_forest(mode = "regression", trees = 1000) %>%
    set_engine("ranger", num.threads = parallel::detectCores()-2, seed = 334)


# ワークフローの作成
df_wflow <- workflow() %>%
    add_formula(cnt ~ .) %>%
    add_model(rf_model)


df_fit <- df_wflow %>% fit(data = df_train)


df_pred <- predict(df_fit, df_test)


# モデルの精度評価
# 回帰の場合の評価指標RMSEは、Metricsパッケージの
# rmse() 関数で算出できます。

library(Metrics)

# この結果が60近くまで小さくなれば大したもの (?) です。
rmse(df_test[["cnt"]], pull(df_pred))


#-----このへんで "Relaunch Project" する-----#

#--------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
# データの読み込み
df <- read_csv("hour.csv", col_types = cols(season = col_factor(), yr = col_factor(),
    mnth = col_factor(), hr = col_factor(), holiday = col_factor(),
    weekday = col_factor(levels = as.character(0:6)), workingday = col_factor(),
    weathersit = col_factor(levels = as.character(1:4))))
library(tidymodels)
library(summarytools)

# 精度を高める工夫をしたモデル

# 数時間前のデータを追加する
# 基本的に、自転車を借りる際に「今この瞬間」の気象条件で判断することは
# 考えにくく、前夜や当日朝の天気で判断するだろう。そのため、1時間前から12時間前の
# 過去のデータを追加することで、目的変数 cnt と関係する特徴量を増やすことができる。
df_add <- df %>%
    mutate(
        prev_temp1 = lag(temp, n = 1),
        prev_temp2 = lag(temp, n = 2),
        prev_temp3 = lag(temp, n = 3),
        prev_temp4 = lag(temp, n = 4),
        prev_temp5 = lag(temp, n = 5),
        prev_temp6 = lag(temp, n = 6),
        prev_temp7 = lag(temp, n = 7),
        prev_temp8 = lag(temp, n = 8),
        prev_temp9 = lag(temp, n = 9),
        prev_temp10 = lag(temp, n = 10),
        prev_temp11 = lag(temp, n = 11),
        prev_temp12 = lag(temp, n = 12),
        prev_atemp1 = lag(atemp, n = 1),
        prev_atemp2 = lag(atemp, n = 2),
        prev_atemp3 = lag(atemp, n = 3),
        prev_atemp4 = lag(atemp, n = 4),
        prev_atemp5 = lag(atemp, n = 5),
        prev_atemp6 = lag(atemp, n = 6),
        prev_atemp7 = lag(atemp, n = 7),
        prev_atemp8 = lag(atemp, n = 8),
        prev_atemp9 = lag(atemp, n = 9),
        prev_atemp10 = lag(atemp, n = 10),
        prev_atemp11 = lag(atemp, n = 11),
        prev_atemp12 = lag(atemp, n = 12),
        prev_hum1 = lag(hum, n = 1),
        prev_hum2 = lag(hum, n = 2),
        prev_hum3 = lag(hum, n = 3),
        prev_hum4 = lag(hum, n = 4),
        prev_hum5 = lag(hum, n = 5),
        prev_hum6 = lag(hum, n = 6),
        prev_hum7 = lag(hum, n = 7),
        prev_hum8 = lag(hum, n = 8),
        prev_hum9 = lag(hum, n = 9),
        prev_hum10 = lag(hum, n = 10),
        prev_hum11 = lag(hum, n = 11),
        prev_hum12 = lag(hum, n = 12),
        prev_weathersit1 = lag(weathersit, n = 1),
        prev_weathersit2 = lag(weathersit, n = 2),
        prev_weathersit3 = lag(weathersit, n = 3),
        prev_weathersit4 = lag(weathersit, n = 4),
        prev_weathersit5 = lag(weathersit, n = 5),
        prev_weathersit6 = lag(weathersit, n = 6),
        prev_weathersit7 = lag(weathersit, n = 7),
        prev_weathersit8 = lag(weathersit, n = 8),
        prev_weathersit9 = lag(weathersit, n = 9),
        prev_weathersit10 = lag(weathersit, n = 10),
        prev_weathersit11 = lag(weathersit, n = 11),
        prev_weathersit12 = lag(weathersit, n = 12),
        prev_cnt1 = lag(cnt, n = 1),
        prev_cnt2 = lag(cnt, n = 2),
        prev_cnt3 = lag(cnt, n = 3),
        prev_cnt4 = lag(cnt, n = 4),
        prev_cnt5 = lag(cnt, n = 5),
        prev_cnt6 = lag(cnt, n = 6),
        prev_cnt7 = lag(cnt, n = 7),
        prev_cnt8 = lag(cnt, n = 8),
        prev_cnt9 = lag(cnt, n = 9),
        prev_cnt10 = lag(cnt, n = 10),
        prev_cnt11 = lag(cnt, n = 11),
        prev_cnt12 = lag(cnt, n = 12)
        ) %>%
    select(-cnt, everything()) %>%
    slice(-c(1:12))
head(df_add)


# データの分割
# データを学習用と検証用に分割します。2012年6月30日までのデータでモデルを作成し、それ以降のデータを予測し、精度を検証してください。
# なお、datetime型である dteday と単なる連番である instant、cnt に直結する casual, registered は除きます。
df_add_train <- df_add %>% filter(dteday <= "2012-06-30") %>% select(-dteday, -instant, -casual, -registered)
df_add_test <- df_add %>% filter(dteday >= "2012-07-01") %>% select(-dteday, -instant, -casual, -registered)


# データの加工
# できるだけ精度が高くなるように、さまざまに加工してみてください。
# ただ、無理にすべてをtidymodelsで行う必要はありません。

# "cnt" 系の特徴量をYeo-Johnson変換する
df_add_bin_rec <- recipe(cnt ~ ., data = df_add_train) %>%
#    step_discretize(all_numeric_predictors(), -contains("cnt"), num_breaks = 5, min_unique = 3) %>%
    step_YeoJohnson(contains("cnt"), -all_outcomes()) %>%
    prep()


df_add_bin_train <- bake(df_add_bin_rec, new_data = NULL)


# ある程度データを整えられたのでは
stview(dfSummary(df_add_bin_train))


# モデル作成
rf_model <- rand_forest(mode = "regression", mtry = tune(), min_n = tune(), trees = 1000) %>%
#    set_engine("randomForest")
    set_engine("ranger", num.threads = parallel::detectCores()-2, seed = 334)


# データ加工レシピが df_add_bin_rec であるとして
df_add_bin_wflow <- workflow() %>%
    add_model(rf_model) %>%
    add_recipe(df_add_bin_rec)


## k-Fold法の設定
folds <- vfold_cv(df_add_bin_train, v = 10)


## ワークフローを実行
## 探索範囲を30分割して最適化
df_tune_res <- tune_grid(df_add_bin_wflow, resamples = folds, grid = 30)


## 実行結果を表示
show_best(df_tune_res)


## ベストなモデルを抽出
final_flow <- df_add_bin_wflow %>% finalize_workflow(parameters = select_best(df_tune_res))
final_model <- final_flow %>% fit(df_add_bin_train)

# 予測結果を格納
df_add_bin_test <- bake(df_add_bin_rec, new_data = df_add_test)

df_add_bin_pred <- predict(final_model, df_add_bin_test)


# モデルの精度評価
# 回帰の場合の評価指標RMSEは、Metricsパッケージの
# rmse() 関数で算出できます。

library(Metrics)

# この結果が60近くまで小さくなれば大したもの (?) です。
rmse(df_add_bin_test[["cnt"]], pull(df_add_bin_pred))


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
