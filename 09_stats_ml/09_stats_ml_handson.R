### 応用プログラミング3
### 第9回　Rによる統計モデリング、機械学習の基礎
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


library(tidyverse)

# 3.1 線形モデル

b <- 10
a <- 3
x <- rnorm(100, mean = 10, sd = 5)
e <- rnorm(100, sd = 10)

y <- (a * x) + b + e

lm_res <- lm(y ~ x)

summary(lm_res)

#--------------------------------------------------------------------#

## モデルの描画

library(ggplot2)
library(ggpmisc)

lm_df <- data.frame(x, y)
ggplot(lm_df, aes(x = x, y = y)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", formula = y ~ x) +
    stat_poly_eq(formula = y ~ x,
                    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~")),
                    parse = TRUE, size = 8)

#--------------------------------------------------------------------#

# 3.2　モデルに基づく予測

fitted(lm_res)


x2 <- rnorm(100, mean = 11, sd = 6)
lm_df2 <- data.frame(x = x2)


predict(lm_res, lm_df2)

#--------------------------------------------------------------------#

# 3.3 非線形モデル

## 多項式モデル

set.seed(334)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 10, sd = mean(x^3) / 4)
y <- (y + abs(min(y))) / 1000


formula <- y ~ poly(x, 3, raw = TRUE)
poly_res <- lm(formula)


summary(poly_res)

#--------------------------------------------------------------------#

### モデルの描画

library(ggplot2)
library(ggpmisc)

poly_df <- data.frame(x, y)
ggplot(poly_df, aes(x = x, y = y)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", formula = formula) +
    stat_poly_eq(formula = formula,
                    aes(label = paste(after_stat(eq.label))),
                    parse = TRUE, size = 5) +
                    ylim(0, 1400)

#--------------------------------------------------------------------#

## 対数モデル

set.seed(3)
b <- 10
a <- 3
x <- rexp(100, rate = 0.5)
e <- rnorm(100, sd = 2)
y <- (a * log(x)) + b + e
nls_df <- data.frame(x, y)


formula <- y ~ a * log(x) + b
nls_res <- nls(formula, start = c(a = 1, b = 1), data = nls_df)


summary(nls_res)

#--------------------------------------------------------------------#

### モデルの描画

args <- list(formula = formula, start = list(a = 1, b = 1))
formula_label <- paste0("y = ", round(nls_res$m$getAllPars()[1], 2), " * log(x) + ", round(nls_res$m$getAllPars()[2], 2))


ggplot(nls_df, aes(x = x, y = y)) +
    geom_point(size = 3) +
    stat_smooth(method = "nls", se = FALSE, method.args = args, linewidth = 3) +
    annotate(geom = "text", x = 6, y = 1, label = formula_label, size = 5)

#--------------------------------------------------------------------#

# 3.4 一般化線形モデル

data(airquality)
aq_df <- airquality %>% select(-Month, -Day) %>% drop_na()


glm_res <- glm(Ozone ~ Solar.R + Wind + Temp, data = aq_df, family = Gamma(link = "log"))


summary(glm_res)

#--------------------------------------------------------------------#

## モデルの描画

Ozone_pred <- fitted(glm_res)
glm_df <- data.frame(Ozone = aq_df[["Ozone"]], Ozone_pred)


ggplot(glm_df, aes(x = Ozone, y = Ozone_pred)) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linewidth = 3, colour = "red") +
    lims(x = c(0, 170), y = c(0, 170))

#--------------------------------------------------------------------#

# 3.5 ロジスティック回帰モデル

set.seed(340)
b <- 10
a <- 3
x <- rnorm(100, mean = 10, sd = 5)
e <- rnorm(100, sd = 5)
y <- (a * x) + b + e
y <- dplyr::if_else(y >= mean(y), 1, 0)


logi_res <- glm(y ~ x, family = binomial(link = "logit"))


summary(logi_res)

#--------------------------------------------------------------------#

## モデルの描画

logi_df <- data.frame(x, y)
formula_label <- paste0("a = ", round(logi_res$coefficients[2], 2), ", b = ", round(logi_res$coefficients[1], 2))


ggplot(logi_df, aes(x = x, y = y)) + geom_point(size = 3) +
    geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE, linewidth = 3) +
    annotate(geom = "text", x = 1, y = 1, label = formula_label, size = 5)

#--------------------------------------------------------------------#

# 3.6 モデルの評価

## 線形回帰モデルの評価

summary(lm_res)

### RMSEの算出

y_pred <- predict(lm_res, lm_df) # 本来は別途用意したテスト用データを使う
y_diff <- lm_df[["y"]] - y_pred
y_squared <- y_diff ** 2
y_mse <- mean(y_squared)
y_rmse <- sqrt(y_mse)


y_rmse


#--------------------------------------------------------------------#

## ロジスティック回帰モデルの評価

library(precrec)
logi_mmdata <- mmdata(fitted(logi_res), y)
logi_curve <- evalmod(logi_mmdata)


autoplot(logi_curve, "ROC") + theme_classic(base_family = "IPAexGothic", base_size = 18) + theme(legend.position = "none")

### 正解率の算出

y_resp <- predict(logi_res, logi_df, type = "response")
y_pred <- dplyr::if_else(y_resp >= 0.5, 1, 0)
y_diff <- logi_df[["y"]] == y_pred
y_rate <- sum(y_diff) / nrow(logi_df)


y_rate

#--------------------------------------------------------------------#

# 4.2 決定木

library(rpart)
library(partykit)
library(ggparty)

df <- read_csv("./pseudo_sake_data.csv", col_types = cols(容器 = col_factor(), タイプ = col_factor(levels = c("一般酒", "吟醸酒", "純米酒", "本醸造酒"))))

## モデルの作成

res_dt <- rpart(タイプ ~., data = df)

#--------------------------------------------------------------------#

## 決定木の描画

ggparty(as.party(res_dt)) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = タイプ),
                                        position = position_fill()),
                               xlab("タイプ")),
                 # draw only one label for each axis
                 shared_axis_labels = TRUE,
                 # draw line between tree and legend
                 legend_separator = TRUE
                 )

#--------------------------------------------------------------------#

## 決定木の剪定

res_dt2 <- prune(res_dt, cp = 0.05)


summary(res_dt2)

#--------------------------------------------------------------------#

## 剪定した決定木の描画

ggparty(as.party(res_dt2)) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = タイプ),
                                        position = position_fill()),
                               xlab("タイプ")),
                 # draw only one label for each axis
                 shared_axis_labels = TRUE,
                 # draw line between tree and legend
                 legend_separator = TRUE
                 )


#--------------------------------------------------------------------#

# 4.3 ランダムフォレスト

library(randomForest)

## 最適なmtry値の探索

best_mtry <- tuneRF(x = df[, 1:ncol(df) - 1], y = pull(df[, ncol(df)]), doBest = TRUE)

#--------------------------------------------------------------------#

## モデルの作成

res_rf <- randomForest(タイプ ~., data = df, mtry = best_mtry$mtry, ntree = 100, importance = TRUE)


#--------------------------------------------------------------------#

## 変数重要度の描画

varImpPlot(res_rf)

#--------------------------------------------------------------------#

## 決定木の本数と精度の関係を描画

plot(res_rf)

#--------------------------------------------------------------------#

# 4.4 クラスタリング

## 疑似データの生成

# 8次元 (列) のデータを生成
x <- matrix(rnorm(100 * 8),100, 8)
# クラスターの数は3にする
cluster <- sample(1:4, 100, replace = TRUE)
# 各クラスターの中心をランダムに定義する
cluster_mean <- matrix(rnorm(4 * 8, sd = 6), 4, 8)
# データ点を各クラスターの中心に近づける
df <- data.frame(x + cluster_mean[cluster, ])

#--------------------------------------------------------------------#

## クラスタリング前に散布図行列を眺める

pairs(df)

#--------------------------------------------------------------------#

## k-means法によるクラスタリング

res <- kmeans(df[, 1:8], centers = 4)


df[["クラスタ番号"]] <- res[["cluster"]]
pairs(df, col = res$cluster)

#--------------------------------------------------------------------#

library(factoextra)

## 主成分分析で2次元に圧縮した時のクラスタの分布
fviz_cluster(res, df[, 1:8])

#--------------------------------------------------------------------#

## 最適なクラスタ数の判断

fviz_nbclust(df[, 1:8], kmeans, method = "wss")

#--------------------------------------------------------------------#

## 参考: 主成分分析の結果を解釈する
# fviz_cluster() 関数は内部で標準の prcomp() 関数を
# 使っている。同様の処理を行い、第1、第2主成分の負荷量から
# 何を示す概念か解釈を試みる。

# 主成分分析の実行
res_pca <- prcomp(df[, 2:(ncol(df) - 1)], scale = FALSE, center = FALSE)

# 第1、第2主成分で全体の39%程度を説明できている
summary(res_pca)$importance[,1:2]

# 各特徴量の第1、第2主成分に対する負荷量を確認する
## 第1主成分
head(sort(res_pca$rotation[, 1]), 10)
tail(sort(res_pca$rotation[, 1]), 10)

## 第2主成分
head(sort(res_pca$rotation[, 2]), 10)
tail(sort(res_pca$rotation[, 2]), 10)
