### 応用プログラミング3
### 第15回　RからPythonを使う / Shinyによるアプリ開発
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太


# 1. 前回の続き

## 4.3　テキストの分散表現

library(tidyverse)
library(text2vec)

# 作成済みの分散表現を読み込む
# Posit Cloudでギリギリ実行できるサイズまで絞り込んだもの
word_vectors <- readRDS("./word_vectors_small.rds")


# パリからフランスを引いて日本を足すと...
res <- word_vectors["パリ", , drop = FALSE] -
        word_vectors["フランス", , drop = FALSE] +
        word_vectors["日本", , drop = FALSE]
cos_sim <- sim2(x = word_vectors, y = res, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


# メモリからオブジェクトを削除する
rm(list = ls())
gc()
gc()


#--------------------------------------------------------------------#

# 2. RとPython

## 2.3　Rプログラム中でPythonを使う

library(tidyverse)
library(reticulate)

np <- import("numpy", convert = FALSE)
pd <- import("pandas", convert = FALSE)


dic <- dict(id = as.character(1:10), value = sample(1:100, 10))


df <- pd$DataFrame(dic)


res <- df$describe()

library(flextable)
py_to_r(res) %>% flextable()


#--------------------------------------------------------------------#

py_run_file("./pyscript_example.py")


py$model$coef_ %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(., py$wine$feature_names) %>%
    flextable()


#--------------------------------------------------------------------#

# 5. i2dashパッケージによるダッシュボード開発の実践

## 5.2　i2dashオブジェクト

library(i2dash)

dashboard <- i2dashboard(title = "テスト", author = "名前", theme = "cosmo", interactive = TRUE, datadir = getwd(), pages = list())


assemble(dashboard, "example.Rmd")


rmarkdown::run("example.Rmd")


#--------------------------------------------------------------------#

## 5.3　ページの追加

dashboard %<>% add_page(page = "contents01", title = "ページ1", layout = "storyboard") %>%
    add_page(page = "contents02", title = "リスト1", layout = "focal_left", menu = "リスト") %>%
    add_page(page = "contents03", title = "リスト2", layout = "focal_left", menu = "リスト")


assemble(dashboard, "example.Rmd")


rmarkdown::run("example.Rmd")


#--------------------------------------------------------------------#

## 5.4　コンテンツの追加

library(DT)


dashboard %<>% add_component(datatable(iris), page = "contents01")


p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
  geom_point()


dashboard %<>% add_component(p, page = "contents01")


assemble(dashboard, "example.Rmd")


rmarkdown::run("example.Rmd")


#--------------------------------------------------------------------#

library(leaflet)


leaflet_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 139.5552138, lat = 35.6088936,
             popup = "専修大学生田キャンパス2号館")


dashboard %<>% add_component(leaflet_map, page = "contents02")


assemble(dashboard, "example.Rmd")


rmarkdown::run("example.Rmd")
