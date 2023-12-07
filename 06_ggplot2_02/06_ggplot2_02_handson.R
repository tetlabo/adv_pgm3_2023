### 応用プログラミング3
### 第6回　ggplot2パッケージによるグラフィックス作成 (2)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 2.1 折れ線グラフ

library(tidyverse)

df <- read_csv("day.csv", col_types = cols(season = col_factor(), yr = col_factor(), mnth = col_factor(), holiday = col_factor(), weekday = col_factor(levels = as.character(0:6)), workingday = col_factor(), weathersit = col_factor(levels = as.character(1:4))))

df_monthly <- df %>% group_by(ym = lubridate::floor_date(dteday, "month")) %>% summarise(avg_temp = mean(temp), avg_cnt = mean(cnt))

ggplot(df_monthly, aes(x = ym, y = avg_cnt)) + geom_line(size = 0.3) + scale_x_date(date_breaks = "3 month", date_labels = "%y/%m")

#--------------------------------------------------------------------#

# 2.2 箱ひげ図

ggplot(df, aes(x = yr, y = registered, fill = yr)) + stat_boxplot(geom = "errorbar") + geom_boxplot()

#--------------------------------------------------------------------#

# 2.3 バイオリンプロット

ggplot(df, aes(x = yr, y = registered, fill = yr)) + geom_violin()

#--------------------------------------------------------------------#

# 2.4 グラフを重ねる

# from https://stackoverflow.com/a/51844068
scaleFactor <- max(df_monthly[["avg_temp"]]) / max(df_monthly[["avg_cnt"]])

colors <- c("avg_temp" = "red4", "avg_cnt" = "blue4")

ggplot(df_monthly, aes(x = ym)) +
    geom_line(aes(y = avg_temp, colour = "avg_temp"), size = 1.5) +
    geom_line(aes(y = avg_cnt * scaleFactor, colour = "avg_cnt"), size = 1.5, alpha = 0.6) +
    scale_y_continuous(sec.axis = sec_axis(~ . / scaleFactor, name = "cnt")) +
    scale_colour_manual(values = colors) +
    theme(legend.position = "bottom")

#--------------------------------------------------------------------#

ggplot(df, aes(x = yr, y = registered, fill = yr)) +
    geom_violin() +
    geom_boxplot(fill = "white", width = 0.2)

#--------------------------------------------------------------------#

# 2.5 エラーバーなどを追加する

ggplot(airquality, aes(x = Month, y = Temp)) +
    geom_line(stat = "summary", fun = "mean") +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) + ggtitle("標準誤差を追加")


#--------------------------------------------------------------------#

ggplot(airquality, aes(x = Month, y = Temp)) +
    geom_line(stat = "summary", fun = "mean") +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun.min = function(x){mean(x) - sd(x)}, fun.max = function(x){mean(x) + sd(x)}, geom = "errorbar", width = 0.2) + ggtitle("標準偏差を追加")

#--------------------------------------------------------------------#

# 2.6 テキストの描画

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
    geom_point(size = 4) +
    annotate("text", label = "setosa", x = 6.7, y = 1.5, size = 10) +
    annotate("text", label = "versicolor", x = 7.3, y = 3.8, size = 10) +
    annotate("text", label = "virginica", x = 5, y = 6, size = 10) +
    annotate("segment", x = 6.25, xend = 5.9, y = 1.45, yend = 1.45, size = 2, arrow = arrow()) +
    annotate("segment", x = 6.65, xend = 6.15, y = 3.75, yend = 3.75, size = 2, arrow = arrow()) +
    annotate("segment", x = 5.6, xend = 6, y = 5.95, yend = 5.95, size = 2, arrow = arrow()) +
    theme(legend.position = "none")

#--------------------------------------------------------------------#

# 3.1 フォントの指定

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
    geom_point() +
    labs(title = "irisデータのプロット", x = "がく片長", y = "花弁長") +
    theme_gray(base_family = "IPAexMincho", base_size = 18)

#--------------------------------------------------------------------#

# 4.1 軸のカスタマイズ

# from: https://ggplot2-book.org/scale-position.html

base <- ggplot(mpg, aes(displ, hwy)) + geom_point()

base
base + scale_x_reverse()
base + scale_y_reverse()

# manual transformation
ggplot(mpg, aes(log10(displ), hwy)) + 
  geom_point()

# transform using scales
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_x_log10()


date_base <- ggplot(economics, aes(date, psavert)) + 
  geom_line(na.rm = TRUE) +
  labs(x = NULL, y = NULL)

date_base 
date_base + scale_x_date(date_breaks = "25 years")

#--------------------------------------------------------------------#

# 4.2 凡例のカスタマイズ

base <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point(aes(colour = Species), size = 3) + 
  xlab(NULL) + 
  ylab(NULL)

base + theme(legend.position = "left")
base + theme(legend.position = "right") # デフォルト
base + theme(legend.position = "bottom")
base + theme(legend.position = "none")

#--------------------------------------------------------------------#

# 5.2 esquisse

library(esquisse)

esquisser(df, viewer = "browser")

#--------------------------------------------------------------------#

# 5.3 ggraptR

library(ggraptR)

ggraptR()
