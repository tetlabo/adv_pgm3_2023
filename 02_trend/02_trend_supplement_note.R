## ----setup, include = FALSE-------------------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(scales)
library(tidyverse)
options(dplyr.width = Inf, scipen = 1, digits = 4)
theme_set(theme_gray(base_family = "IPAexGothic", base_size = 16))


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
vec <- 1:100
length(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
data(iris) # 組み込みのirisデータセットを使う
nrow(iris) # 行数
ncol(iris) # 列数
dim(iris) # 行数、列数


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
length(iris[["Species"]]) # Species列の件数 = 行数
length(iris[1, ]) # 1行目の件数 = 列数


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec <- sample(1:100, 10) # 1から100の間で10個整数の乱数を抽出
min(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec <- sample(1:100, 10) # 1から100の間で10個整数の乱数を抽出
max(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec <- sample(1:100, 10) # 1から100の間で10個整数の乱数を抽出
range(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec <- rnorm(100, mean = 1, sd = 3) # 平均1、標準偏差3の正規乱数を100個抽出
mean(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec1 <- rnorm(900, mean = 1, sd = 3)
vec2 <- rnorm(100, mean = 100, sd = 30)
vec <- c(vec1, vec2)
mean(vec) # 外れ値に引っ張られる
mean(vec, trim = 0.1)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
vec <- c(1:10, NA) # 欠損値を含むベクトルを作成
min(vec) # 結果が得られない
max(vec) # 結果が得られない
mean(vec) # 結果が得られない


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
min(vec, na.rm = TRUE) # 欠損値を除去して最小値を算出
max(vec, na.rm = TRUE) # 欠損値を除去して最大値を算出
mean(vec, na.rm = TRUE) # 欠損値を除去して平均を算出


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334) # 結果が固定されるよう乱数のシードを設定
vec1 <- rnorm(900, mean = 1, sd = 3)
vec2 <- rnorm(100, mean = 100, sd = 30)
vec <- c(vec1, vec2)
median(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
df <- read.csv("pseudo_sake_data.csv")
# 説明のため、あえて個別に関数を適用していく
tbl <- table(df[["タイプ"]])
# sort() 関数による並べ替え
sort_tbl <- sort(tbl, decreasing = TRUE)
# 最頻値の出力
sort_tbl[1]

# which.max() 関数による方法
tbl[which.max(tbl)]


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec <- rnorm(100, mean = 10, sd = 3)
# 不偏分散を算出
var(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
sample_var <- function(vec){
    vec <- na.omit(vec) # 欠損値を除去
    avg <- mean(vec) # 平均を算出
    dev <- vec - avg # 偏差を算出
    dev_square <- sum(dev^2) # 偏差平方和を算出
    sample_var <- dev_square / length(vec) # 標本分散を算出
    return(sample_var)
}
sample_var(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec <- rnorm(100, mean = 10, sd = 3)
# 不偏標準偏差を算出
sd(vec)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# 標本標準偏差を算出
sqrt(sample_var(vec))


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
quantile(vec)
summary(vec)


## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="「相関がある」データ"------------------------------------------------------------------
set.seed(3349800)
vec_x <- 1:30
vec_y <- vec_x + rnorm(30, sd = 6)
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y")
res <- lm(vec_y ~ vec_x)
abline(res, col = 2, lwd = 3)
dev.off()


## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="関係はあるが「相関はない」データ"------------------------------------------------------
set.seed(3349800)
vec_x <- 1:30
make_vec_y <- function(vec_x){
    vec_y <- (vec_x - mean(vec_x))^2 + mean(vec_x)
    return(vec_y)
}
vec_y <- make_vec_y(vec_x) * rnorm(30, mean = 1, sd = 0.2)
plot(make_vec_y(vec_x), type = "l", col = 2, lwd = 3, xlab = "", ylab = "", xaxt = "none", yaxt = "none")
par(new = TRUE)
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y")
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(3349800)
vec_x <- 1:30
vec_y <- vec_x + rnorm(30, sd = 6)
cor(vec_x, vec_y)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y", main = paste0("相関係数: ", round(cor(vec_x, vec_y), 3)))
res <- lm(vec_y ~ vec_x)
abline(res, col = 2, lwd = 3)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(3349800)
vec_x <- 1:30
vec_y <- -vec_x + rnorm(30, sd = 6)
cor(vec_x, vec_y)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
par(family = "IPAexGothic")
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y", main = paste0("相関係数: ", round(cor(vec_x, vec_y), 3)))
res <- lm(vec_y ~ vec_x)
abline(res, col = 2, lwd = 3)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(3349800)
vec_x <- 1:30
vec_y <- vec_x + rnorm(30, sd = 70)
cor(vec_x, vec_y)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y", main = paste0("相関係数: ", round(cor(vec_x, vec_y), 3)))
res <- lm(vec_y ~ vec_x)
abline(res, col = 2, lwd = 3)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE, fig.cap="離散データの分布例"--------------------------------------------------------------------------------
vec <- sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1))
barplot(table(vec))
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
vec_x <- 1:10
vec_y <- c(1, 2, 3, 8, 7, 5, 6, 10, 4, 9)
cor(vec_x, vec_y, method = "spearman")


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# ggplot2を使えばもっとシンプルに書けますが、
# まだ紹介していないので、あえてbase Rで
set.seed(3349800)
vec_x <- 1:100
vec_y <- vec_x + rnorm(100, sd = 30)
cor(vec_x, vec_y)
par(family = "IPAexGothic")
plot(vec_x, vec_y, col = 4, pch = 16, cex = 2, xlab = "x", ylab = "y", xlim = c(0, 305), ylim = c(-350, 250), main = "外れ値で相関関係がまるっきり変わってしまう例")
res <- lm(vec_y ~ vec_x)
abline(res, col = 4, lwd = 3)
text(50, -100, paste0("相関係数: ", round(cor(vec_x, vec_y), 3)), col = 4)
vec_x <- c(1:100, 301:303)
vec_y <- c(vec_y, -301:-303 + rnorm(3, sd = 20))
cor(vec_x, vec_y)
points(301:303, -301:-303 + rnorm(3, sd = 20), col = 2, pch = 16, cex = 2)
res <- lm(vec_y ~ vec_x)
abline(res, col = 2, lwd = 3)
text(250, -150, paste0("相関係数: ", round(cor(vec_x, vec_y), 3)), col = 2)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
tbl <- as.table(matrix(c(40, 15, 25, 20, 15, 35, 35, 15), nrow = 2, byrow = TRUE))
rownames(tbl) <- c("男性", "女性")
colnames(tbl) <- c("ビール", "ワイン", "日本酒", "ウイスキー")
tbl


## ----eval=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------
## install.packages("vcd")


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
library(vcd)
assocstats(tbl)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
assoc(tbl)
dev.off()


## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="統計的検定のよくある図"----------------------------------------------------------------
library(webr)
vec <- rnorm(100)
res <- t.test(vec, mu = 0)
plot(res)
dev.off()


## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="両側検定と片側検定", fig.width=16, fig.height=10---------------------------------------
vec1 <- rnorm(100, mean = 10)
vec2 <- rnorm(100, mean = 10.5)
res1 <- t.test(vec1, vec2)
res2 <- t.test(vec1, vec2, alternative = "greater")
plot(res1)
dev.off()

plot(res2)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# 乱数の固定
set.seed(334)

# 平均が0.24、標準偏差1の正規分布に基づく乱数を抽出
vec <- rnorm(100, mean = 0.24)

# データの平均が母平均0 (帰無仮説) と等しいかを検定
t.test(vec, mu = 0)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec1 <- rnorm(100) # 平均0, 標準偏差1
vec2 <- rnorm(100, mean = 0.25, sd = 1.3)
t.test(vec1, vec2)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
res <- t.test(vec1, vec2)
plot(res)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
sleep


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
t.test(extra ~ group, data = sleep, paired = TRUE)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(334)
vec1 <- rbeta(100, 2.6, 3)
vec2 <- rbeta(100, 3, 2.6)
df <- data.frame(id = c(rep("a", 100), rep("b", 100)), value = c(vec1, vec2))

ggplot(df, aes(x = value, colour = id, fill = id)) + geom_density(alpha = 0.4) + ggtitle("正規分布には見えないデータ")
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
wilcox.test(vec1, vec2)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec1 <- rnorm(50, mean = 10, sd = 10)
vec2 <- rnorm(50, mean = 11.5, sd = 18)
vec3 <- rnorm(50, mean = 13.5, sd = 6)
id <- c(rep("A", 50), rep("B", 50), rep("C", 50))
df <- data.frame(id, value = c(vec1, vec2, vec3))

ggplot(df, aes(x = id, y = value, fill = id)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
dev.off()

oneway.test(value ~ id, data = df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec1 <- rbeta(50, 2.6, 3)
vec2 <- rbeta(50, 3, 2.6)
vec3 <- rbeta(50, 2.2, 3.4)
df <- data.frame(group = c(rep("a", 50), rep("b", 50), rep("c", 50)), value = c(vec1, vec2, vec3))

ggplot(df, aes(x = value, colour = group, fill = group)) + geom_density(alpha = 0.4) + ggtitle("正規分布には見えないデータ")
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
kruskal.test(value ~ group, data = df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# https://stats.stackexchange.com/a/115767
# Set coefficients
set.seed(334)
alpha <- 10
beta1 <- 0.1
beta2 <- -1.2
beta3 <- 0.9

# Generate 200 trials
method <- c(rep(0, 50), rep(1, 50))
# 0: 女性, 1: 男性
gender <- rep(c(rep(0, 25), rep(1, 25)), 2)
e <- rnorm(100, 0, sd = 1)

# Generate your data using the regression equation
score <- alpha + beta1 * method + beta2 * gender + beta3 * method * gender + e
score <- score * 8

# Join the variables in a data frame
df <- data.frame(method, gender, score = floor(score))
head(df)
tail(df)
interaction.plot(x.factor = df[["method"]],
    trace.factor = df[["gender"]],
    response = df[["score"]],
    fun = "mean", type = "b",
    ylim = c(70, 85), ylab = "mean of score",
    xlab = "method", trace.label = "gender",
    pch = c(19, 21), cex.lab = 1.2)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
res <- aov(score ~ method * gender, data = df)
summary(res)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
set.seed(334)
vec1 <- rnorm(50, mean = 10, sd = 10)
vec2 <- rnorm(50, mean = 11, sd = 10)
vec3 <- rnorm(50, mean = 14.1, sd = 10)
id <- c(rep("A", 50), rep("B", 50), rep("C", 50))
df <- data.frame(id, value = c(vec1, vec2, vec3))

summary(aov(value ~ id, data = df))

pairwise.t.test(df[["value"]], df[["id"]], data = df, p.adjust.method = "bonferroni")


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
library(rstatix)

set.seed(334)
vec1 <- rnorm(50, mean = 10, sd = 10)
vec2 <- rnorm(50, mean = 11.5, sd = 18)
vec3 <- rnorm(50, mean = 14.5, sd = 6)
id <- c(rep("A", 50), rep("B", 50), rep("C", 50))
df <- data.frame(id, value = c(vec1, vec2, vec3))

oneway.test(value ~ id, data = df)

games_howell_test(value ~ id, data = df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# ある職業に従事する親から生まれた子100人のうち男子が38人だった場合
prop.test(x = 38, n = 100, p = 0.4878)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
binom.test(x = 38, n = 100, p = 0.4878)


## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="カイ2乗値とカイ2乗分布"----------------------------------------------------------------
library(webr)
men <- c(34, 66)
women <- c(50, 50)
df <- rbind.data.frame(men, women)
rownames(df) <- c("男性", "女性")
colnames(df) <- c("好き", "嫌い")
res <- chisq.test(df)
plot(res)
dev.off()


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
men <- c(34, 66)
women <- c(50, 50)
df <- rbind.data.frame(men, women)
rownames(df) <- c("男性", "女性")
colnames(df) <- c("好き", "嫌い")
df
chisq.test(df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
men <- c(3, 7)
women <- c(6, 4)
df <- rbind.data.frame(men, women)
rownames(df) <- c("男性", "女性")
colnames(df) <- c("好き", "嫌い")
df
chisq.test(df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
men <- c(3, 7)
women <- c(6, 4)
df <- rbind(men, women)
rownames(df) <- c("男性", "女性")
colnames(df) <- c("好き", "嫌い")
fisher.test(df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
Performance <- matrix(c(794, 86, 150, 570), nrow = 2,
       dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
win <- c(45, 55)
mac <- c(67, 33)
lin <- c(34, 66)
df <- rbind(win, mac, lin)
rownames(df) <- c("Windows", "macOS", "Linux")
colnames(df) <- c("薦める", "薦めない")
df
chisq.test(df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
win <- c(7, 8)
mac <- c(9, 2)
lin <- c(1, 7)
df <- rbind(win, mac, lin)
rownames(df) <- c("Windows", "macOS", "Linux")
colnames(df) <- c("薦める", "薦めない")
df
fisher.test(df)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
library(rstatix)

# データの生成
mydata <- data.frame(
  結果 = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
  勉強法 = gl(3,1,30,labels=LETTERS[1:3]),
  被験者 = gl(10,3,labels=letters[1:10])
)
mydata$結果 <- factor(
  mydata$結果, levels = c(1, 0),
  labels = c("合格", "不合格")
  )
mydata

# クロス集計
xtabs(~結果 + 勉強法, mydata)

# Compare the proportion of success between treatments
cochran_qtest(mydata, 結果 ~ 勉強法|被験者)


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# chisq.test() 関数の多重比較
win <- c(45, 55)
mac <- c(67, 33)
lin <- c(34, 66)
df <- rbind(win, mac, lin)
rownames(df) <- c("Windows", "macOS", "Linux")
colnames(df) <- c("薦める", "薦めない")
df
chisq.test(df)

pairwise.prop.test(df, p.adjust.method = "hochberg")


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
# install.packages("fmsb")
library(fmsb)

# Fisherの正確確率検定の多重比較
win <- c(7, 8)
mac <- c(9, 2)
lin <- c(1, 7)
df <- rbind(win, mac, lin)
rownames(df) <- c("Windows", "macOS", "Linux")
colnames(df) <- c("薦める", "薦めない")
df
fisher.test(df)

pairwise.fisher.test(df, p.adjust.method = "hochberg")


## ----eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
library(rstatix)

# データの生成
mydata <- data.frame(
  結果 = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
  勉強法 = gl(3,1,30,labels=LETTERS[1:3]),
  被験者 = gl(10,3,labels=letters[1:10])
)
mydata$結果 <- factor(
  mydata$結果, levels = c(1, 0),
  labels = c("合格", "不合格")
  )
mydata

# クロス集計
xtabs(~結果 + 勉強法, mydata)

# Compare the proportion of success between treatments
cochran_qtest(mydata, 結果 ~ 勉強法|被験者)

pairwise_mcnemar_test(mydata, 結果 ~ 勉強法|被験者)

