### 応用プログラミング3
### 第13回　日本語テキストマイニング (1)
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

# 2. テキストデータの価値

## 2.2　テキストデータの収集

library(jsonlite)

keyword <- URLencode("チャットGPT")
query <- paste0("https://kokkai.ndl.go.jp/api/speech?any=", keyword, "&recordPacking=json")
res <- fromJSON(query, simplifyVector = TRUE)
res[[5]]


# 4. Rによるテキストマイニング


## 4.2　テキストデータの前処理

# ファイルの読み込み
# readLines() 関数は1行を1つのベクトルとして読み込む
txt <- readLines("./sample_texts/sample_text01.txt")
txt

# 空行の削除
# != "" で、要素が空でないものだけTRUEが返ってくる
txt <- txt[txt != ""]
txt


# 行の結合
# paste() 関数や paste0() 関数に collapse = "区切り文字" オプションを
# 指定することででベクトルを結合できる。
txt <- paste(txt, collapse = "")
txt

#--------------------------------------------------------------------#

library(tidyverse)

# ローカル環境で実行する際は以下2行をコメントアウトする
home <- Sys.getenv("HOME")
dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))

library(RMeCab)


txt <- readLines("./long_sample_text.txt")
# URLをあらわす正規表現
url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

parsed_text <- txt %>%
    str_replace_all(url_regex, "") %>% # URLを除去する
    # 日本語の場合
    # ひらがな、カタカナ、漢字、数字、"ー" だけを残す
    str_replace_all("[^\\p{Hiragana}|\\p{Katakana}|\\p{Han}|\\p{N}|ー]", " ") %>%
    str_replace_all(" +", " ") %>% # 連続するスペースを1つにまとめる
    str_replace_all("^ | $", "") %>% # 行頭や行末のスペースを除去する
    str_replace_all("\\n", "") %>%
    str_subset(".+") %>%
    # 形態素解析 (後述) してスペース区切りで出力する
    map(~paste(flatten_chr(RMeCabC(., mypref = 1)), collapse=" ")) %>%
    flatten_chr() %>% as_tibble()


parsed_text


#--------------------------------------------------------------------#

## 4.4　RMeCabパッケージによる形態素解析

# ローカル環境で実行する際は以下2行をコメントアウトする
home <- Sys.getenv("HOME")
dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))

library(RMeCab)


txt <- "RMeCabは、「Rでテキストマイニング」というジャンルにおける定番中の定番です。徳島大学の石田基広教授によって開発されています。"


# mypref = 1 で原形を返す
res <- unlist(RMeCabC(txt, mypref = 1))
res


# 結果から名詞・動詞・形容詞のみ取り出す
res[names(res) %in% c("名詞", "動詞", "形容詞")]


#--------------------------------------------------------------------#

# RMeCabText(): ファイル名を指定して形態素解析する
# 返り値は形態素ごとにリストに格納されたベクトルとして出力される
RMeCabText("sample_texts/sample_text01.txt")


#--------------------------------------------------------------------#

# docDF(): ある種の万能関数。入力はファイルやフォルダー (複数ファイル)、
# データフレームに対応。type = 1 で形態素単位の処理、Genkei = 0 で
# 出現形を原形に変換する、nDF = 1 で出力をデータフレームにする
res <- docDF("sample_texts/sample_text01.txt", type = 1, Genkei = 0, nDF = 1)
res


#--------------------------------------------------------------------#

# pos = 品詞 (大分類) で、処理対象を限定
res <- docDF("sample_texts/sample_text01.txt", type = 1, pos = c("名詞", "動詞", "形容詞"), Genkei = 0, nDF = 1)
res


#--------------------------------------------------------------------#

# N = 2以上でN-gramを抽出。N-gramは形態素の組み合わせのこと
res <- docDF("sample_texts/sample_text01.txt", type = 1, Genkei = 0, nDF = 1, N = 3)
res


#--------------------------------------------------------------------#

# 引数にフォルダーを指定すると複数のテキストファイルを読み込んで解析、出力する
# 出力には、どのテキストで何回出現したかの情報も付与される
res <- docDF("sample_texts", type = 1, pos = c("名詞", "動詞", "形容詞"), Genkei = 0, nDF = 1)
res


#--------------------------------------------------------------------#

## 4.5　gibasaパッケージによる形態素解析

library(readtext)
library(gibasa)


files <- paste0("sample_texts/", list.files("sample_texts", pattern = "^sample"))
df <- readtext(files) # フォルダー内のテキストファイルをまとめて読み込む


res <- gibasa::tokenize(df) # 形態素解析を行う。他のパッケージが提供する関数と名前が被るためパッケージ名を指定する
res


prettify(res)


#--------------------------------------------------------------------#

## 4.6　sudachiによる形態素解析

library(reticulate)


sudachipy <- import("sudachipy")
dict <- sudachipy$Dictionary(dict = "core")
tokenizer <- dict$create()

txt <- "gibasaは、「Rでテキストマイニング」というジャンルにおける新しい選択肢です。加藤秋瑠 (ハンドルネーム) 氏によって開発されています。"
res <- tokenizer$tokenize(txt)


res


res_it <- iterate(res) # 独自構造のPythonオブジェクトをRのリストに変換する
res_it


# tidyverseな map() 系関数でもっとシンプルに書けそうですが
df <- data.frame(
    surface = sapply(res_it, function(x) x$surface()),
    base = sapply(res_it, function(x) x$dictionary_form()),
    pos1 = sapply(res_it, function(x) unlist(x$part_of_speech()[1])),
    pos2 = sapply(res_it, function(x) unlist(x$part_of_speech()[2])),
    pos3 = sapply(res_it, function(x) unlist(x$part_of_speech()[3])),
    pos4 = sapply(res_it, function(x) unlist(x$part_of_speech()[4])),
    pos5 = sapply(res_it, function(x) unlist(x$part_of_speech()[5])),
    pos6 = sapply(res_it, function(x) unlist(x$part_of_speech()[6]))
)


df


#--------------------------------------------------------------------#

# 5. さまざまな分析手法

## 5.1　頻度分析

# 名詞、動詞、形容詞に絞って頻度で並べ替える
res <- RMeCabFreq("./long_sample_text.txt") %>% filter(Info1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!Info2 %in% c("接尾", "非自立", "数")) %>%
  filter(!Term %in% c("する", "ある", "いる", "なる", "あれ", "これ", "それ", "いう", "ない")) %>% # ストップワードの除去
  filter(nchar(Term) > 1) %>% # 1文字の形態素を除去
  filter(Freq >= 3) %>%
  arrange(desc(Freq))


res


#--------------------------------------------------------------------#

# 棒グラフで確認する
ggplot(res, aes(x = reorder(Term, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x="")


#--------------------------------------------------------------------#

# gibasa版

files <- paste0("sample_texts/", list.files("sample_texts", pattern = "^sample"))
df <- readtext(files)


res <- gibasa::tokenize(df)


res <- res %>%
    mutate(pos1 = unlist(str_split(feature, ","))[1],
        pos2 = unlist(str_split(feature, ","))[2],
        pos3 = unlist(str_split(feature, ","))[3]) %>%
    group_by(token, pos1, pos2, pos3) %>%
    summarise(freq = n()) %>%
    filter(pos1 %in% c("名詞", "動詞", "形容詞")) %>%
    filter(!pos2 %in% c("接尾", "非自立", "数")) %>%
    filter(!token %in% c("する", "ある", "いる", "なる", "あれ", "これ", "それ", "いう", "ない", "まし", "から", "こと")) %>%
    filter(nchar(token) > 1) %>%
    filter(freq >= 3) %>%
    arrange(desc(freq))


res


#--------------------------------------------------------------------#

# 棒グラフで確認する
ggplot(res, aes(x = reorder(token, freq), y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x="")
