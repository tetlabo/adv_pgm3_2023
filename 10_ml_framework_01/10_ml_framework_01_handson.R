### 応用プログラミング3
### 第10回　機械学習フレームワークによる機械学習 (1)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 3 tidymodelsによる機械学習ワークフロー

# 3.1 rsampleパッケージによるデータの分割

library(tidymodels)

iris_split <- initial_split(iris, prop = 0.8)


iris_train <- training(iris_split)


iris_test <- testing(iris_split)


dim(iris_train)


dim(iris_test)

#--------------------------------------------------------------------#

# 3.2 recipesパッケージによる前処理

iris_rec <- recipe(Species ~ ., data = iris_train) %>%
    step_normalize(all_numeric_predictors())


#--------------------------------------------------------------------#

# 参考: レシピの適用
# 今回、教材ではレシピを定義して、そのまま次の処理に移行していますが、
# step_*() 関数で定義した処理を実際に適用したデータを作成することも
# できます。その際は、prep() 関数と bake() 関数を使用します。
# prep() 関数は、例えば実際に標準化するために平均と標準偏差を計算するなど、
# レシピを適用するために必要な値を求める関数です。
# bake() 関数は、レシピをデータに対して適用し、変換後のデータを得る関数です。
# 学習用データに適用するには、bake(レシピ, new_data = NULL) とします。
# 検証用データに適用するには、bake(レシピ, new_data = オブジェクト名) とします。

iris_train_df <- iris_rec %>% prep() %>% bake(new_data = NULL)


iris_test_df <- iris_rec %>% prep() %>% bake(new_data = iris_test)


View(iris_train_df)


View(iris_test_df)

#--------------------------------------------------------------------#

# 3.3 parsnipパッケージによるモデルの設定

dt_model <- decision_tree(mode = "classification") %>%
    set_engine("rpart")

#--------------------------------------------------------------------#

# 3.4 workflowsパッケージによるワークフローの登録

iris_wflow <- workflow() %>%
    add_model(dt_model) %>%
    add_recipe(iris_rec)

#--------------------------------------------------------------------#

# 3.5 モデルの作成と予測

iris_fit <- iris_wflow %>% fit(data = iris_train)


iris_fit %>% extract_fit_parsnip()


predict(iris_fit, iris_test)

#--------------------------------------------------------------------#

# 3.6 モデルの精度評価

iris_aug <- augment(iris_fit, iris_test)


metrics <- metric_set(accuracy, sens, spec, precision, recall, kap)


iris_aug %>% metrics(truth = Species, estimate = .pred_class)

#--------------------------------------------------------------------#

# 4. mlr3による機械学習ワークフロー

# 4.1 タスクの定義

library(mlr3verse)
library(GGally)
theme_set(theme_gray(base_family = "IPAexGothic", base_size = 12))


task <- as_task_classif(iris, target = "Species")

autoplot(task, type = "pairs", upper = list(continuous = wrap("cor", size = 2)))

#--------------------------------------------------------------------#

# 4.2 モデルの設定

learner <- lrn("classif.rpart")


learner$param_set


learner$param_set$values <- list(cp = 0.1, keep_model = TRUE)

#--------------------------------------------------------------------#

# 4.3 データの分割

resampling <- rsmp("holdout", ratio = 0.8)


resampling$instantiate(task)


resampling$train_set(1)


resampling$test_set(1)

#--------------------------------------------------------------------#

# 4.4 学習

learner$predict_type <- "prob"


learner$train(task, row_ids = resampling$train_set(1))


learner$model


autoplot(learner)

#--------------------------------------------------------------------#

# 4.5 予測

pred <- learner$predict(task, row_ids = resampling$test_set(1))


pred$confusion


pred$prob


pred$response


autoplot(pred)


learner$predict_newdata(iris[1:5, ])

#--------------------------------------------------------------------#

# 4.6 精度評価

pred$score()


pred$score(msr("classif.acc"))
