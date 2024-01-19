### 応用プログラミング3
### 第14回　日本語テキストマイニング (2)
### ハンズオン用プログラム
### 
### 専修大学ネットワーク情報学部
### 田中健太

#--------------------------------------------------------------------#

# はじめに以下の2つのスクリプトを実行してください。
# * install_MeCab_on_Posit_Cloud.sh
# * 実行が終了したら、"Relaunch Project" してください
# * install_RMeCab_on_Posit_Cloud.R

# 実行後、RStudioの [Tools] - [Global Options] から、
# [Python] メニューを選び、[Select] から最新のPythonを選択してください。
# その後、"Relaunch Project" でプロジェクトを再起動し、以下のスクリプトを実行してください。
# * install_sudachipy_on_Posit_Cloud.sh


#--------------------------------------------------------------------#

# 2.2　歌詞データの収集

## 歌ネット (https://www.uta-net.com/) をスクレイピングして
## 歌詞を収集するコードのサンプル
## 参考: 【GoogleColaboratory】歌ネット（Uta-Net）から歌詞をスクレイピングする
## https://zenn.dev/robes/articles/00e86185677fb5

## 以下のコードは実行しないでください
##### NOT RUN #####
# library(tidyverse)
# library(httr)
# library(rvest)

# base_url <- "https://www.uta-net.com"

# #urls <- "https://www.uta-net.com/artist/6636/"
# # とりあえずAKB48と乃木坂46と櫻坂46を選択 (オッサンには区別がつかないですが...)
# urls <- c("https://www.uta-net.com/artist/6636/0/1/", "https://www.uta-net.com/artist/6636/0/2/", "https://www.uta-net.com/artist/12550/", "https://www.uta-net.com/artist/29512/")

# # User Agentを偽装する
# pseudo_user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"

# # データフレームの初期化
# df <- data.frame(id = numeric(), artist = character(), title = character(), lyric = character(), url = character())

# for (i in 1:length(urls)) {
#     res <- GET(urls[i], user_agent(pseudo_user_agent))
#     html <- content(res)
#     links <- html %>% html_elements("td.sp-w-100")
#     for (link in links) {
#         href <- link %>% html_element("a") %>% html_attr("href")
#         song_url <- paste0(base_url, href, collapse = "")
#         song_res <- GET(song_url, user_agent(pseudo_user_agent))
#         song_html <- content(song_res)
#         song_title <- song_html %>% html_element("h2") %>% html_text2()
#         artist_name <- song_html %>% html_element("h3") %>% html_text2()
#         lyric <- song_html %>% html_element("div#kashi_area") %>% html_text2() %>% str_replace_all("\\n+", " ")
#         tmp_df <- data.frame(id = 0, artist = artist_name, title = song_title, lyric = lyric, url = song_url)
#         df <- df %>% bind_rows(tmp_df)
#         print(paste0("now processing '", artist_name, "', '", song_title, "'"))
#         Sys.sleep(1)
#     }
# }

# df[["id"]] <- as.numeric(factor(df[["artist"]], levels = c("AKB48", "乃木坂46", "櫻坂46")))

# write_csv(df, "pops_lyrics.csv")
##### NOT RUN #####


#--------------------------------------------------------------------#

# 3.1　ワードクラウド
# アーティストごとの歌詞の違いを可視化する (...歌詞だけに)

library(tidyverse)
# home <- Sys.getenv("HOME")
# dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(gibasa)
df <- read_csv("./pops_lyrics.csv")

res <- df %>% select(-url) %>% gibasa::tokenize(., text_field = "lyric", docid_field = "title") %>% prettify()

## 歌詞をアーティストごとに形態素解析し、頻度分析する

## ストップワードを定義する
## stopwordsパッケージには、日本語のストップワードも含まれている
## https://cran.r-project.org/package=stopwords
mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194],
  "WOW", "Wow", "wow", "WO", "NO", "Hey", "Yeah", "AH", "OH", "Oh", "oh", "Na", "LA", "La", "L\u30fbA"))

res2 <- res %>% filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  mutate(token = if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  filter(!token %in% mystopwords) %>%
  filter(str_detect(token, "[^\\p{Hiragana}{1}|\\p{Katakana}{1}|\\p{Latin}{1}|\\p{N}{1}]")) %>% # ひらがな1文字、アルファベット1文字の形態素を除去
  mutate(token = if_else(token == "BIUTIFULビューティフル", "ビューティフル", token)) %>% # 辞書の原形が変なので
  group_by(id, artist, token) %>%
  summarise(count = n()) %>% # 頻度を算出
  mutate(prob = count / n()) %>% # アーティストごとに曲数が違うので頻度から出現率も算出
  select(artist, token, count, prob) %>%
  arrange(artist, desc(prob)) %>%
  top_n(100) %>% # 上位100語を抽出
  ungroup() # グループ化を解除して普通のデータフレームにする


## wordcloudパッケージ版
library(wordcloud)

### グループごとにワードクラウドを作成し、画像として保存するため、
### 一連の処理を関数化する
make_wordcloud <- function(df) {
  # ファイル名を設定
  filename <- paste0("wordcloud_", str_replace_all(unique(df$artist), " ", "_"), ".png")
  tmp_df <- data.frame(token = df$token, prob = df$prob) # 形態素と出現率だけからなるデータフレームを作成
  ragg::agg_png(filename=filename, width = 1024, height = 1024, pointsize = 18, res = 96)
  par(family = "IPAexMincho")
  wordcloud(df$token, df$prob, scale = c(4, 1.5), colors = mypalette) # ワードクラウドを作成
  dev.off()
}

mypalette <- brewer.pal(8, "Dark2") # RColorBrewerパッケージのパレットから文字色を8色選択

### dplyrパッケージの group_map() 関数は、group_by() したグループ全体に
### 対して引数に指定した関数を適用する。パイプ経由のデータは .x として渡す。
### https://dplyr.tidyverse.org/reference/group_map.html
res2 %>% group_by(id) %>% group_map(~ make_wordcloud(.x))

#--------------------------------------------------------------------#

## wordcloud2パッケージ版

### wordcloud2はHTML + JavaScriptで出力するので、画像として保存するには
### htmlwidgetsパッケージやwebshot2パッケージを使う
### 参考: https://twitter.com/DavidUbilava/status/1577524847730921472

library(wordcloud2)
library(webshot2)
library(htmlwidgets)

make_wordcloud2 <- function(df) {
  filename <- paste0("wordcloud2_", str_replace_all(unique(df$artist), " ", "_"), ".png")
  tmp_df <- data.frame(token = df$token,  count = as.numeric(cut(df$count, breaks = 10, labels = 1:10)))
  wc <- wordcloud2(tmp_df, fontFamily = "IPAexMincho", size = 2.5, minSize = 0.5)
  saveWidget(wc, "wc.html", selfcontained = FALSE) # HTMLとして保存
  webshot("wc.html", filename, delay = 5, vwidth = 1280, vheight = 1280) # ヘッドレスでスクリーンショットを取得
}

res2 %>% group_by(id) %>% group_map(~ make_wordcloud2(.x))


#--------------------------------------------------------------------#

## ggwordcloud版

library(ggwordcloud)

### 文字色はランダムに選択する
res2[["colour"]] <- sample(mypalette, nrow(res2), replace = TRUE)

make_ggwordcloud <- function(df) {
  filename <- paste0("ggwordcloud_", str_replace_all(unique(df$artist), " ", "_"), ".png")
  tmp_df <- data.frame(token = df$token, prob = df$prob)
  wc <- ggplot(df, aes(label = token, size = prob, colour = colour)) +
    geom_text_wordcloud(fontface = "bold", family = "IPAexMincho") +
    scale_size(range = c(2, 14)) +
    theme_minimal()
  ggsave(filename, wc, device = ragg::agg_png, width = 1600, height = 1280, units = "px", bg = "white")
}

res2 %>% group_by(id) %>% group_map(~ make_ggwordcloud(.x))


#--------------------------------------------------------------------#

# このへんで "Relaunch Project" する

#--------------------------------------------------------------------#

## 3.2　共起分析

### N = 2のNgramを作成する
### 参考: Chapter 3 N-gram | RとMeCabによる日本語テキストマイニングの前処理
### https://paithiov909.github.io/textmining-ja/ngram.html

library(tidyverse)
# home <- Sys.getenv("HOME")
# dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(gibasa)
library(audubon)
library(igraph)
library(intergraph)
library(ggnetwork)

### 歌詞データを読み込む

df <- read_csv("./pops_lyrics.csv")


mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194],
  "WOW", "Wow", "wow", "WO", "NO", "Hey", "Hey!", "Yeah", "AH", "OH", "Oh", "oh", "Na", "LA", "La", "L\u30fbA"))


res <- df %>%
  select(-url) %>%
  gibasa::tokenize(., text_field = "lyric", docid_field = "title") %>%
  prettify(col_select = c("Original", "POS1", "POS2")) %>%
  mutate(token = dplyr::if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  filter(!token %in% mystopwords) %>%
  filter(str_detect(token, "[^\\p{Hiragana}{1}|\\p{Latin}{1}]")) %>%
  mutate(token = if_else(token == "BIUTIFULビューティフル", "ビューティフル", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "＝LOVE", "LOVE", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "隣る", "隣", token)) %>% # 辞書の原形が変なので
  mutate(token = str_replace_all(token, "-", " ")) %>% # "-" を含む形態素は後の処理で問題が出るので、スペースに置換する
  select(id, artist, doc_id, sentence_id, token)


#--------------------------------------------------------------------#

### Ngramを作成するオブジェクトを生成
bigram <- ngram_tokenizer(n = 2)


make_collocate <- function(df) {
  df %>%
    group_by(artist, doc_id) %>%
    summarise(token = bigram(token, sep = "-"), .groups = "keep") %>% # N = 2のNgramを作成
    count(token) %>%
    select(token, n) %>%
    separate(token, c("N1", "N2"), sep = "-") # Ngramが「単語 - 単語」として出力されるので、1列ずつに分割する
}


ndf <- res %>% group_by(id) %>% group_modify(~ make_collocate(.x))


View(ndf)


#--------------------------------------------------------------------#

### 共起ネットワークを描く

make_collocate_graph <- function(df) {
  filename <- paste0("collocate_", str_replace_all(unique(df$artist), " ", "_"), ".png")
  df <- df %>% select(N1, N2, n) %>% arrange(desc(n)) %>% top_n(50) # 上位50個のNgramだけ描く
  n <- graph.data.frame(df) %>% igraph::simplify(.) # ループなどを除去する
  n2 <- ggnetwork(asNetwork(n), layout = "fruchtermanreingold")
  p <- ggplot(n2, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black", arrow = arrow(length = unit(6, "pt"), type = "closed")) +
    geom_nodes(color = "tomato", size = 6, alpha = 0.5) +
    geom_nodetext(aes(label = vertex.names), size = 6, check_overlap = TRUE) +
    theme_blank(base_family = "IPAexGothic")
  ggsave(filename, p, device = ragg::agg_png, width = 4096, height = 4096, units = "px", bg = "white")
}

ndf %>% group_map(~ make_collocate_graph(.x))


#--------------------------------------------------------------------#

# 4. テキストデータの機械学習

## 4.1　クラスタリング

### 所信表明演説コーパス (https://github.com/tetlabo/GeneralPolicySpeechOfPrimeMinisterOfJapan) を使用
library(tidyverse)
library(readtext)
library(zipangu)
# home <- Sys.getenv("HOME")
# dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(gibasa)

files <- paste0("./speeches/", list.files("./speeches/", pattern = ".txt"))
texts <- readtext(files)

mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194], str_conv_zenhan(0:12, to = "zenkaku"), "〇", "\u30fb", "（拍手）"))

bow_df <- gibasa::tokenize(texts) %>%
  prettify() %>%
  filter(POS1 %in% c("名詞", "動詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  filter(!token %in% mystopwords) %>%
  mutate(token = if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  filter(str_detect(token, "[\\p{Hiragana}|\\p{Katakana}|\\p{Han}|\\p{Latin}]")) %>% # 誤って分類される記号類を除く
  filter(!str_detect(token, "[0-9]+月[0-9]+日")) %>%
  mutate(token = str_conv_normalize(token, "nfkc")) %>%
  select(doc_id, token) %>%
  group_by(doc_id, token) %>%
  summarise(count = n()) %>%
  spread(key = token, value = count, fill = 0)

dim(bow_df)

#--------------------------------------------------------------------#

### k-means法によるクラスタリング

library(factoextra)

res <- kmeans(bow_df[, 2:ncol(bow_df)], centers = 5)

bow_df[["クラスタ番号"]] <- res[["cluster"]]


#--------------------------------------------------------------------#

### 最適なクラスタ数の判断
fviz_nbclust(bow_df[, 2:ncol(bow_df)], kmeans, method = "wss")


#--------------------------------------------------------------------#

### 主成分分析で2次元に圧縮した時のクラスタの分布
### 某元専修大学助教授の演説だけ極めて異質であることがわかる
fviz_cluster(res, bow_df[, 2:ncol(bow_df)])


#--------------------------------------------------------------------#

## 回帰 / 分類
## Scraping Amazon Reviews in R – Musings on R (https://martinctc.github.io/blog/vignette-scraping-amazon-reviews-in-r/)
## を参考に、Amazonの商品レビューと評価 (星) を取得し、分類モデルを作成する

library(tidyverse)
library(rvest)
library(zipangu)
# home <- Sys.getenv("HOME")
# dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(gibasa)
library(audubon)

# ### Amazonからレビューをスクレイピングする関数
# scrape_amazon <- function(ASIN, page_num){
  
#   url_reviews <- paste0("https://www.amazon.co.jp/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
#   doc <- read_html(url_reviews) # Assign results to `doc`
  
#   # Review Title
#   doc %>% 
#     html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
#     html_text() -> review_title
  
#   # Review Text
#   doc %>% 
#     html_nodes("[class='a-size-base review-text review-text-content']") %>%
#     html_text() -> review_text
  
#   # Number of stars in review
#   doc %>%
#     html_nodes("[data-hook='review-star-rating']") %>%
#     html_text() -> review_star
  
#   # Return a tibble
#   tibble(review_title,
#          review_text,
#          review_star,
#          page = page_num) %>% return()
# }

# ### 取得対象のAmazon商品ID
# ASIN <- "B0B3SNZ6CP" # 映画「大怪獣のあとしまつ」
#                      # レビューはPrime Video、DVDなどがすべて統合されているようです。

# ### 先にレビューのページ数を確認しておく
# page_range <- 1:28 # Let's say we want to scrape pages 1 to 28

# ### Create a table that scrambles page numbers using `sample()`
# ### For randomising page reads!
# match_key <- tibble(n = page_range,
#                     key = sample(page_range,length(page_range)))

# lapply(page_range, function(i){
#   j <- match_key[match_key$n==i,]$key

#   message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar

#   Sys.sleep(2) # Take a two seconds break

#   if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
#     message("Taking a break...") # Prints a 'taking a break' message on your console
    
#     Sys.sleep(2) # Take an additional two second break
#   }
#   scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
# }) -> output_list

# df <- bind_rows(output_list)

# write_csv(df, "./amazon_reviews.csv")

df <- read_csv("./amazon_reviews.csv")

#--------------------------------------------------------------------#

### 前処理

#### 評価 (星) を数値に変換する
df[["review_star"]] <- str_replace_all(df[["review_star"]], "^.*のうち", "") %>% as.numeric()

#### タイトルとレビューコメントを結合する
df <- df %>% mutate(review_comment = paste(review_title, review_text, sep = " "))

#### いろいろ不要な文字を除去する
df <- df %>% mutate(review_comment = str_replace_all(review_comment, "[^\\p{Hiragana}|\\p{Katakana}|\\p{Han}|\\p{Latin}|\\p{N}ー ]", "")) %>%
    mutate(review_comment = stringi::stri_trans_nfkc(review_comment)) %>%
    mutate(review_comment = str_replace_all(review_comment, " +", " ")) %>% # 連続するスペースを1つにまとめる
    mutate(review_comment = str_replace_all(review_comment, "^ | $", "")) %>% # 行頭、行末のスペースを除去する
    mutate(review_comment = str_replace_all(review_comment, "大鳳", "太鳳")) %>% # 誤字に対処
    mutate(review_comment = str_replace_all(review_comment, "ストリー", "ストーリー")) %>% # 誤字に対処
    mutate(review_comment = str_replace_all(review_comment, "キャスティングストーリーテンポ", "キャスティング、ストーリー、テンポ")) %>% # 句読点のないレビューに対処
    mutate(review_comment = str_replace_all(review_comment, "ゴジラオマージュ", "ゴジラ オマージュ")) %>% # 句読点のないレビューに対処
    mutate(review_comment = str_replace_all(review_comment, "インザプールダメジン", "インザプール、ダメジン")) %>% # 句読点のないレビューに対処
    mutate(review_comment = str_replace_all(review_comment, "レビュアーアンチレビュー", "レビュアー、アンチレビュー")) # 句読点のないレビューに対処

#### 外国語のレビュー (1文字もひらがなが使われていないレビュー) を除去する
df <- df %>%
  filter(str_detect(review_comment, "^.*\\p{Hiragana}.*$"))

#### 必要な列だけ抽出する
df <- df %>% mutate(doc_id = as.factor(row_number())) %>% select(doc_id, review_comment, review_star)

mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194], str_conv_zenhan(0:12, to = "zenkaku"), "〇〇", "\u30fb", "志摩線", "有馬線", "茶宇", "僂指", "婀娜", "譚"))

### BoWの作成
bow_df <- gibasa::tokenize(df, text_field = "review_comment") %>%
  prettify() %>%
  filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  filter(!token %in% mystopwords) %>%
  mutate(token = if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  mutate(token = if_else(token == "Black Joke", "ブラックジョーク", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "Deus ex Machina", "デウスエクスマキナ", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "Johnny's", "ジャニーズ", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "襤褸糞", "ボロクソ", token)) %>% # 辞書の原形が変なので
  mutate(token = if_else(token == "Pokemon!", "ポケモン", token)) %>% # 辞書の原形が変なので
  mutate(token = str_replace_all(token, " ", "_")) %>%
  mutate(token = str_replace_all(token, "[!&\u30fb。]", "_")) %>%
  filter(str_detect(token, "[^\\p{Latin}{1}|\\p{Hiragana}{1}|\\p{Katakana}{1}]")) %>%
  filter(str_detect(token, "[\\p{Hiragana}|\\p{Katakana}|\\p{Han}|\\p{Latin}]")) %>% # 誤って分類される記号類を除く
  filter(!str_detect(token, "[0-9]+月[0-9]+日")) %>%
  filter(!str_detect(token, "^[0-9].*")) %>% # モデリング時に数字から始まる列名を特徴量に使えないようなので
  mutate(token = str_conv_normalize(token, "nfkc")) %>%
  mutate(token = str_replace_all(token, "_$", "")) %>%
  select(doc_id, review_star, token) %>%
  group_by(doc_id, token) %>%
  summarise(count = n()) %>%
  spread(key = token, value = count, fill = 0) %>%
  inner_join(select(df, doc_id, review_star), by = "doc_id") %>%
  ungroup() %>%
  select(-doc_id) %>%
  relocate(review_star)

dim(bow_df)


#--------------------------------------------------------------------#

### ランダムフォレストでモデルを作る

library(ranger)
library(tuneRanger)
library(cvms)
library(rsvg)
#library(ggnewscale)
#library(ggimage)

#### 最適なmtry数をチューニングする
best_mtry <- tuneMtryFast(as.factor(review_star) ~ ., data = bow_df, num.treesTry = 100, doBest = TRUE)


#### モデルを作成する。レビューは1から5の数値だが、5段階なので分類と捉えたほうがよい
res_rf <- ranger(as.factor(review_star) ~ ., data = bow_df, mtry = best_mtry$mtry, num.trees = 100, importance = "impurity")


#### 予測値の算出
pred <- predict(res_rf, bow_df[, 2:(ncol(bow_df))])$predictions


#### 実測値 (正解) と予測値をまとめる
eval_df <- data.frame(actual = as.factor(pull(bow_df[, "review_star"])), pred = pred)


#### 変数重要度をプロットする

vi_vec <- head(sort(res_rf$variable.importance, decreasing = TRUE), 20)
vi_df <- data.frame(pos = names(vi_vec), importance = unname(vi_vec))

ggplot(vi_df, aes(x = fct_reorder(pos, importance), y = importance)) + geom_bar(stat = "identity") + coord_flip()

#### 混同行列をプロットする (cvmsパッケージ)
library(ggthemes)
theme_set(theme_fivethirtyeight(base_family = "IPAexGothic", base_size = 12))
conf_mat <- confusion_matrix(targets = eval_df$actual, predictions = eval_df$pred)

plot_confusion_matrix(
  conf_mat$`Confusion Matrix`[[1]],
  add_sums = TRUE,
  sums_settings = sum_tile_settings(
    palette = "Oranges",
    label = "Total",
    tc_tile_border_color = "black"
  )
)


#--------------------------------------------------------------------#

# このへんで "Relaunch Project" する

#--------------------------------------------------------------------#

### 4.3　テキストの分散表現

### text2vecのWebサイト http://text2vec.org/glove.html の例

library(tidyverse)
library(text2vec)

### 以下は、Posit Cloud (というか大抵のPC) では処理が重すぎて実行できない
### ので、参考に。あらかじめ作成した単語ベクトルを読み込んで、デモをします。
### 筆者の実行環境はAMD Ryzen 9 3900XT 12コア24スレッド + メモリ64GBですが、
### 最初の形態素解析して不要な要素を取り除いたベクトルを作成する時点で十数時間かかります。

# ### Wikipediaの日本語テキストを使用する
# library(readtext)
# library(zipangu)
# library(gibasa)

# files <- paste0("./texts/", list.files("./texts/", pattern = ".txt", recursive = TRUE))
# texts <- readtext(files)

# mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
#                         stopwords::stopwords(language = "ja", source = "marimo")[11:194], str_conv_zenhan(0:12, to = "zenkaku"), "〇", "みる"))

# for (i in 1:nrow(texts)){
# #for (i in 1:3){
#     train_df <- gibasa::tokenize(texts[i, ]) %>%
#         prettify() %>%
#         filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
#         filter(!POS2 %in% c("接尾", "非自立")) %>%
#         filter(!token %in% mystopwords) %>%
#         filter(str_detect(token, "[\\p{Hiragana}|\\p{Katakana}|\\p{Han}|\\p{Latin}]")) %>% # 誤って分類される記号類を除く
#         filter(str_detect(token, "[^\\p{Hiragana}{1}|\\p{Katakana}{1}|\\p{Latin}{1}]")) %>% # ひらがな1文字、アルファベット1文字の形態素を除去
#         filter(!str_detect(token, "[0-9]+月[0-9]+日")) %>%
#         filter(!str_detect(token, "^-*[0-9].*")) %>%
#         mutate(token = str_conv_normalize(token, "nfkc")) %>%
#         mutate(token = str_replace_all(token, "\n", " ")) %>%
#         mutate(token = str_replace_all(token, "^ | $", "")) %>%
#         collapse_tokens(., POS1 %in% c("名詞", "動詞", "形容詞"), .collapse = " ")
#     outfile <- file("jawiki_text.txt", open = "a")
#     writeLines(train_df$token, outfile)
#     close(outfile)
#     print(paste(i, "files processed."))
# }

# tokens <- readLines("./jawiki_text.txt")

# ### Create iterator over tokens
# tokenizer <- space_tokenizer(tokens)
# ### # Create vocabulary. Terms will be unigrams (simple words).
# it <- itoken(tokenizer, progressbar = TRUE)
# vocab <- create_vocabulary(it)


# ### 頻度の少ない語彙を削除する
# pruned_vocab <- prune_vocabulary(vocab, term_count_min = 50L)


# ### Use our filtered vocabulary
# vectorizer <- vocab_vectorizer(pruned_vocab)


# ### 前後10語を使う
# tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L)


# ### 200次元の分散表現を獲得する
# glove <- GlobalVectors$new(rank = 200, x_max = 10)
# wv_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 20)


# wv_context <- glove$components


# word_vectors <- wv_main + t(wv_context)


# saveRDS(word_vectors, "word_vectors.rds")


### 作成済みの分散表現を読み込む
word_vectors <- readRDS("./word_vectors_small.rds")


#--------------------------------------------------------------------#

### 語彙空間の可視化
### https://medium.com/broadhorizon-cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
library(uwot)
### 以下も重くてPosit Cloudでは実行できないので、実行済みのデータを読み込んで利用します
# glove_umap <- umap(word_vectors, n_components = 2, metric = "cosine", n_neighbors = 50, min_dist = 0.1, spread = 2, n_threads = 20, verbose = TRUE)

# # Put results in a dataframe for ggplot, starting with Word2Vec
# df_glove_umap <- as.data.frame(glove_umap, stringsAsFactors = FALSE)

# # Add the labels of the words to the dataframe
# df_glove_umap$word <- rownames(word_vectors)
# colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")
# df_glove_umap$technique <- "GloVe"
# str(df_glove_umap)

df_glove_umap <- readRDS("./df_glove_umap.rds")

# ggplot(df_glove_umap) +
#       geom_point(aes(x = UMAP1, y = UMAP2), colour = "blue", size = 2) +
#       geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
#       labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
#       theme(plot.title = element_text(hjust = .5, size = 14))


ggplot(df_glove_umap[df_glove_umap$UMAP1 >= 4 & df_glove_umap$UMAP1 <= 5 & df_glove_umap$UMAP2 <= -4,]) +
      geom_point(aes(x = UMAP1, y = UMAP2), colour = "blue", size = 2) +
      geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
      labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
      theme(plot.title = element_text(hjust = .5, size = 12))


#--------------------------------------------------------------------#


# パリからフランスを引いて日本を足すと...
res <- word_vectors["パリ", , drop = FALSE] -
        word_vectors["フランス", , drop = FALSE] +
        word_vectors["日本", , drop = FALSE]
cos_sim <- sim2(x = word_vectors, y = res, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
