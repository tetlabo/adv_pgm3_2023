### 応用プログラミング3
### 第9回　Rによる統計モデリング、機械学習の基礎
### 週次課題　解答

#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#

# 共通: パッケージの読み込み
# 以下を実行してください。他に必要なパッケージがあれば適宜読み込んでください。
library(tidyverse)
library(rpart)
library(partykit)
library(randomForest)

# Q: 回帰モデルの作成
#    データ day.csv (レンタル自転車の日別データ) を読み込み、
#    それぞれ指示にしたがって適切なモデルを作成し、評価してください。
#    なお、目的変数は利用者数 cnt です。

# Q1: データの読み込み
#     ファイルからデータを読み込んでください。
#     読み込みには readr パッケージの read_csv() 関数を使ってください。

df <- read_csv("day.csv")


# Q2: データの加工
#     データには、日付 (dteday)、一時利用者数 (casual)、
#     定期利用者数 (registered) といった、不要または目的変数と直結し
#     モデリングには不適な列がありますので、除外してください。
#
#     また、yr 列には0と1の値が入っており、0が2011年、1が2012年を
#     あらわします。ここでは、2011年のデータを使ってモデルを作成し、
#     2012年のデータを予測するため、上記の処理と合わせてデータを
#     絞り込んでください。

df_yr0 <- df %>% filter(yr == 0) %>% select(-dteday, -casual, -registered, -yr)

# Q3: 線形回帰モデルの作成
#     df_yr0 データを使い、目的変数を cnt、説明変数 (特徴量) をその他の列とした
#     線形回帰モデルを作成してください。そして、モデルの概要を summary() 関数で
#     出力し、また、AICを算出してください。

res_lm <- lm(cnt ~ ., data = df_yr0)
summary(res_lm)
AIC(res_lm)

# Q4: 一般化線形モデル (GLM) の作成
#     df_yr0 データを使い、目的変数を cnt、説明変数 (特徴量) をその他の列とし、
#     誤差分布にガンマ分布、リンク関数には "identity" を指定した一般化
#     線形モデルを作成してください。そして、モデルの概要を summary() 関数で
#     出力し、また、AICを算出してください。

res_glm <- glm(cnt ~., data = df_yr0, family = Gamma(link = "identity"))
summary(res_glm)
AIC(res_glm)

# Q5: ランダムフォレストモデルの作成
#     df_yr0 データを使い、目的変数を cnt、説明変数 (特徴量) をその他の列とし、
#     ランダムフォレストモデルを作成してください。合わせて、モデルにおける特徴量の
#     重要度をグラフィックスとしてプロットしてください。
#     (ランダムフォレストでは summary() による要約、AICの算出はできません)

library(randomForest)

best_mtry <- tuneRF(x = df_yr0[, 1:ncol(df_yr0) - 1], y = pull(df_yr0[, ncol(df_yr0)]), doBest = TRUE)
res_rf <- randomForest(cnt ~., data = df_yr0, mtry = best_mtry$mtry, ntree = 500, importance = TRUE)

varImpPlot(res_rf)

# Q6: 各モデルによる2012年のデータの予測と評価
#     線形回帰、一般化線形モデル、ランダムフォレストで作成したモデルを使い、
#     2012年のデータ (yr = 1) を予測し、当てはまりを確認してください。

# Q6-1: 予測用データの作成
#       Q2 と同様の手順で、df_yr1 を作成してください。

df_yr1 <- df %>% filter(yr == 1) %>% select(-dteday, -casual, -registered, -yr)


# Q6-2: 線形回帰モデルによる予測と評価
#       線形回帰モデルを df_yr1 に適用し、予測値を得てください。
#       予測値と実測値のずれを、グラフィックスとしてプロットしてください。
#       第9回の資料3.3節を参考にしてください。

pred_lm <- predict(res_lm, df_yr1)

df <- data.frame(cnt = df_yr1[["cnt"]], pred_lm)

ggplot(df, aes(x = cnt, y = pred_lm)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linewidth = 3, colour = "red") +
    lims(x = c(-3000, 9000), y = c(-3000, 9000))


# Q6-3: 一般化線形モデルによる予測と評価
#       一般化線形モデルを df_yr1 に適用し、予測値を得てください。
#       予測値と実測値のずれを、グラフィックスとしてプロットしてください。

pred_glm <- predict(res_glm, df_yr1)

df <- data.frame(cnt = df_yr1[["cnt"]], pred_glm)

ggplot(df, aes(x = cnt, y = pred_glm)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linewidth = 3, colour = "red") +
    lims(x = c(-700, 9000), y = c(-700, 9000))


# Q6-4: ランダムフォレストモデルによる予測と評価
#       ランダムフォレストモデルを df_yr1 に適用し、予測値を得てください。
#       予測値と実測値のずれを、グラフィックスとしてプロットしてください。

pred_rf <- predict(res_rf, df_yr1)

df <- data.frame(cnt = df_yr1[["cnt"]], pred_rf)

ggplot(df, aes(x = cnt, y = pred_rf)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linewidth = 3, colour = "red") +
    lims(x = c(0, 9000), y = c(0, 9000))


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。