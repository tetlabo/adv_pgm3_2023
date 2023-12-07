### 応用プログラミング3
### 第12回　機械学習フレームワークによる機械学習 (2)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 必要なパッケージの読み込み
library(tidyverse)

#--------------------------------------------------------------------#

# 2.4 正規化・標準化

## 正規化

library(tidymodels)

iris_split <- initial_split(iris, prop = 0.8)


iris_train <- training(iris_split)
iris_test <- testing(iris_split)


iris_rec <- recipe(Species ~ ., data = iris_train) %>%
    step_range(all_numeric_predictors()) %>%
    prep()


iris_train2 <- bake(iris_rec, new_data = NULL)


head(iris_train2)


summary(iris_train2)

#--------------------------------------------------------------------#

## 標準化

iris_rec <- recipe(Species ~ ., data = iris_train) %>%
    step_normalize(all_numeric_predictors()) %>%
    prep()


iris_train2 <- bake(iris_rec, new_data = NULL)


head(iris_train2)


summary(iris_train2)


#--------------------------------------------------------------------#

# 2.5 対数変換

df <- data.frame(id = 1:500, value = exp(rnorm(500)))


ggplot(df, aes(x = value)) + geom_histogram()


df_rec <- recipe(~ ., data = df) %>%
    step_log(value) %>%
    prep()


df2 <- bake(df_rec, new_data = NULL)


ggplot(df2, aes(x = value)) + geom_histogram()

#--------------------------------------------------------------------#

# 2.6 べき乗変換

df <- data.frame(id = 1:500, value = exp(rnorm(500)))


ggplot(df, aes(x = value)) + geom_histogram()


df_rec <- recipe(~ ., data = df) %>%
    step_YeoJohnson(value, limits = c(-10, 10)) %>%
    prep()

df2 <- bake(df_rec, new_data = NULL)


ggplot(df2, aes(x = value)) + geom_histogram()

#--------------------------------------------------------------------#

# 2.7 無相関化

iris_rec <- recipe(Species ~ ., data = iris_train) %>%
    step_pca(all_numeric_predictors(), threshold = .8, options = list(center = TRUE, scale. = TRUE)) %>%
    prep()


iris_train2 <- bake(iris_rec, new_data = NULL)


head(iris_train2)


cor(iris_train2[, 2:3])

#--------------------------------------------------------------------#

# 2.8 欠損値の処理

df <- read_csv("pseudo_sake_data_withNA.csv",
    col_types = cols(容器 = readr::col_factor(levels = c("紙パック", "リターナブル瓶", "茶色瓶", "青色瓶", "緑色瓶", "その他色瓶", "その他")),
    タイプ = readr::col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))


summary(df)


df_rec1 <- recipe(タイプ ~ ., data = df) %>%
    step_naomit(all_predictors(), all_outcomes()) %>%
    prep()


df1 <- bake(df_rec1, new_data = NULL)


summary(df1)

#--------------------------------------------------------------------#

## 定量データは平均で、定性データは最頻値で補間する

df_rec2 <- recipe(タイプ ~ ., data = df) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors(), all_outcomes()) %>%
    prep()


df2 <- bake(df_rec2, new_data = NULL)


summary(df2)

#--------------------------------------------------------------------#

# 2.9 外れ値の処理

library(Rlof)


# データを生成
x <- sort(rnorm(100))
df <- data.frame(x = x, y = x + runif(100, min = -1, max = 1))


# 意図的に外れ値を作成
df[51:60, "y"] <- df[51:60, "y"] - 3


ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 3) + labs(title = "元データ")


# 近傍100点を使ったLOF
df$lof_res <- lof(df, k = 20)


df$lof_res


summary(df$lof_res)


# LOF scoreが1.5を超える値に外れ値フラグをつける
df$outlier <- ifelse(df$lof_res >= 1.5, TRUE, FALSE)


# 外れ値フラグがついたデータを除いた散布図を描画
ggplot(df, aes(x = x, y = y, colour = outlier)) +
  geom_point(size = 3) + labs(title = "外れ値検出の結果")

#--------------------------------------------------------------------#

# 2.10 ダミー変数化

df <- read_csv("pseudo_sake_data.csv",
    col_types = cols(容器 = readr::col_factor(levels = c("紙パック", "リターナブル瓶", "茶色瓶", "青色瓶", "緑色瓶", "その他色瓶", "その他")),
    タイプ = readr::col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))


# "0+" は、ダミー変数化した際に他がすべてゼロである場合に一意に決まる列を残す、という指定
model_mat <- model.matrix(as.formula(タイプ ~ 0+.), data = df)


head(model_mat)


# デフォルトではラベル数-1の列数に展開される
# すべてのラベルを残して展開したい場合は、 one_hot = TRUE オプションを指定
df_rec <- recipe(タイプ ~ ., data = df) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    prep()


df1 <- bake(df_rec, new_data = NULL)

head(df1)

#--------------------------------------------------------------------#

# 2.11 ビニング

df <- read_csv("pseudo_sake_data.csv",
    col_types = cols(容器 = readr::col_factor(levels = c("紙パック", "リターナブル瓶", "茶色瓶", "青色瓶", "緑色瓶", "その他色瓶", "その他")),
    タイプ = readr::col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))


# 定量データを4つの区間に等分割する
df_rec1 <- recipe(タイプ ~ ., data = df) %>%
    step_cut(all_numeric_predictors(), breaks = 4) %>%
    prep()


df_train1 <- bake(df_rec1, new_data = NULL)


head(df_train1)


# 各区間の個数ができるだけ等しくなるように分割する
df_rec2 <- recipe(タイプ ~ ., data = df) %>%
    step_discretize(all_numeric_predictors(), num_breaks = 4, min_unique = 5) %>%
    prep()


df_train2 <- bake(df_rec2, new_data = NULL)


head(df_train2)

#--------------------------------------------------------------------#

# 2.12 アンダーサンプリング・オーバーサンプリング

library(themis)


df <- read_csv("pseudo_sake_data.csv",
    col_types = cols(容器 = readr::col_factor(levels = c("紙パック", "リターナブル瓶", "茶色瓶", "青色瓶", "緑色瓶", "その他色瓶", "その他")),
    タイプ = readr::col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))


# 処理前の状態の確認
table(df[["タイプ"]])

#--------------------------------------------------------------------#

## ダウンサンプリング

df_rec1 <- recipe(タイプ ~ ., data = df) %>%
    step_downsample(タイプ) %>%
    prep()


df1 <- bake(df_rec1, new_data = NULL)


table(df1[["タイプ"]])

#--------------------------------------------------------------------#

## オーバーサンプリング

df_rec2 <- recipe(タイプ ~ ., data = df) %>%
    step_upsample(タイプ) %>%
    prep()


df2 <- bake(df_rec2, new_data = NULL)


table(df2[["タイプ"]])

#--------------------------------------------------------------------#

# 2.13 交互作用特徴量・多項式特徴量の合成

## 交互作用特徴量の生成
df_rec1 <- recipe(タイプ ~ ., data = df) %>%
    step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors()) %>%
    prep()


df1 <- bake(df_rec1, new_data = NULL)


head(df1)

#--------------------------------------------------------------------#

## 多項式特徴量の生成
make_exp <- function(x, n) { x ^ n}


df2 <- df %>% mutate(across(where(is.numeric),
    .fns = list(exp = ~make_exp(x = ., n = 2)),
    .names = "{col}^2"))


head(df2)

#--------------------------------------------------------------------#

#-----ここで "Relaunch Project" する-----#

# 3.4 tidymodelsにおけるグリッドサーチ
library(tidyverse)
library(tidymodels)

df <- read_csv("pseudo_sake_data.csv",
    col_types = cols(容器 = readr::col_factor(levels = c("紙パック", "リターナブル瓶", "茶色瓶", "青色瓶", "緑色瓶", "その他色瓶", "その他")),
    タイプ = readr::col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))

df_split <- initial_split(df, prop = 0.8)

df_train <- training(df_split)
df_test <- testing(df_split)


## k-Fold法の設定
## 一般的にはk-Foldだが、tidymodelsではv-Fold
## k-Fold法は、データセットをk分割し、k-1組で学習、残り1組で検証を
## k回繰り返す精度評価の手法
folds <- vfold_cv(df_train, v = 5)


## 今回は正解率のみを評価指標にする
metrics <- metric_set(accuracy)


## 標準化の適用 (実際のところ、決定木では標準化は不要)
df_rec <- recipe(タイプ ~ ., data = df_train) %>%
    step_normalize(all_numeric_predictors()) %>%
    prep()


## 決定木モデルの定義
## チューニングしたいパラメーターを tune() 関数で指定する
## 探索範囲はデフォルトのまま
## https://dials.tidymodels.org/reference/trees.html
dt_model <- decision_tree(mode = "classification", cost_complexity = tune(), min_n = tune()) %>%
    set_engine("rpart")


## 一連の処理をワークフローとして定義
df_flow <- workflow() %>%
    add_model(dt_model) %>%
    add_recipe(df_rec)


## ワークフローを実行
## 探索範囲を5分割して最適化
## 本当はもっと細かい方がよいが、Posit Cloudのスペックのため
df_tune_flow <- tune_grid(df_flow, resamples = folds, metrics = metrics, grid = 5)


## 実行結果を表示
show_best(df_tune_flow)
