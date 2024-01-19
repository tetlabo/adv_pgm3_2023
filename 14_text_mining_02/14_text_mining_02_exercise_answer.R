### 応用プログラミング3
### 第14回　日本語テキストマイニング (2)
### 週次課題　解答


#--------------------------------------------------------------------#
# 以下に、問題文の意図を実現するRプログラムを記述してください。
# Posit Cloud上で実行できることを確認し、提出してください。
# プログラムの内容について、適宜コメントを記述してください。
# 1行1行コメントする必要はありませんが、「何をしようとしているか」がわかるように
# してください。コメントの一切ないプログラムは、評価が下がります。
#--------------------------------------------------------------------#

# 今回の課題では、Posit Cloudに配置している「内閣総理大臣の所信表明演説コーパス」を
# 使用します。ローカル環境で課題に取り組む場合は、"speeches" フォルダーをチェックし、
# "More" メニューから "Export" を選択してコーパスをダウンロードしてください。

# Q1: ワードクラウドの作成
#     所信表明演説から任意の1つ (1件) を選び、テキストファイルを読み込んでください。
#     今回は、gibasaパッケージで処理することを意図していますので、gibasaの
#     tokenize() 関数で扱いやすいかたちで読み込むとよいでしょう。
#     読み込んだテキストに適切な前処理を行い、wordcloud2パッケージを使って
#     ワードクラウドを作成してください。画像の提出は必要ありません。

#     もし余裕がある方は、「任意の1人」(複数回再任された人) を選び、まとめて
#     処理し、1つのワードクラウドにしてください。ファイル名が異なるので、
#     どのように条件 (pattern) を指定するか、どうやってひとつのdocidにまとめるか
#     などを検討してみてください。よくできた結果については、採点機能としては
#     100点以上は付けられませんが、最終的な成績を登録する際に加点して計算します。


# 必要なライブラリなどを読み込み、プログラムを記述してください

library(tidyverse)
library(readtext)
library(zipangu)
home <- Sys.getenv("HOME")
dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(gibasa)

files <- paste0("./speeches/", list.files("./speeches/", pattern = ".txt"))
texts <- readtext(files)

texts <- texts %>%
    mutate(pm = str_replace_all(doc_id, "^.*_|\\.txt", ""))

res <- tail(texts, 1) %>% gibasa::tokenize(., text_field = "text", docid_field = "doc_id") %>% prettify()

mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194], str_conv_zenhan(0:12, to = "zenkaku"), "〇", "\u30fb", "（拍手）"))

res2 <- res %>% filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  mutate(token = if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  filter(!token %in% mystopwords) %>%
  filter(str_detect(token, "[^\\p{Hiragana}{1}|\\p{Katakana}{1}|\\p{Latin}{1}|\\p{N}{1}]")) %>% # ひらがな1文字、アルファベット1文字の形態素を除去
  group_by(doc_id, pm, token) %>%
  summarise(count = n()) %>% # 頻度を算出
  select(doc_id, token, count) %>%
  arrange(doc_id, desc(count)) %>%
  top_n(100) %>% # 上位100語を抽出
  ungroup() # グループ化を解除して普通のデータフレームにする


library(wordcloud2)
library(webshot2)
library(htmlwidgets)

tmp_df <- res2 %>% select(token, count)

wc <- wordcloud2(tmp_df, fontFamily = "IPAexMincho", size = 2, minSize = 0.25)
saveWidget(wc, "wc.html", selfcontained = FALSE) # HTMLとして保存

filename <- paste0("wordcloud2_", str_replace_all(unique(res2$pm), " ", "_"), ".png")
webshot("wc.html", filename, delay = 5, vwidth = 1280, vheight = 1280) # ヘッドレスでスクリーンショットを取得


#--------------------------------------------------------------------#


# Q2: 共起ネットワーク図の作成
#     所信表明演説から任意の1つ (1件) を選び、テキストファイルを読み込んでください。
#     今回は、gibasaパッケージで処理することを意図していますので、gibasaの
#     tokenize() 関数で扱いやすいかたちで読み込むとよいでしょう。
#     読み込んだテキストに適切な前処理を行い、igraphパッケージなどを使って
#     共起ネットワーク図を作成してください。画像の提出は必要ありません。

#     もし余裕がある方は、「任意の1人」(複数回再任された人) を選び、まとめて
#     処理し、1つの共起ネットワーク図にしてください。ファイル名が異なるので、
#     どのように条件 (pattern) を指定するか、どうやってひとつのdocidにまとめるか
#     などを検討してみてください。よくできた結果については、採点機能としては
#     100点以上は付けられませんが、最終的な成績を登録する際に加点して計算します。


# 必要なライブラリなどを読み込み、プログラムを記述してください

library(audubon)
library(igraph)
library(intergraph)
library(ggnetwork)

files <- paste0("./speeches/", list.files("./speeches/", pattern = ".txt"))
texts <- readtext(files)

texts <- texts %>%
    mutate(pm = str_replace_all(doc_id, "^.*_|\\.txt", ""))

mystopwords <- unique(c(stopwords::stopwords(language = "ja", source = "stopwords-iso")[1:124],
  stopwords::stopwords(language = "ja", source = "marimo")[11:194], str_conv_zenhan(0:12, to = "zenkaku"), "〇", "\u30fb", "（拍手）"))

res <- tail(texts, 1) %>%
  gibasa::tokenize(., text_field = "text", docid_field = "doc_id") %>%
  prettify(col_select = c("Original", "POS1", "POS2")) %>%
  mutate(token = dplyr::if_else(is.na(Original), token, Original)) %>% # 原形があれば原形を選択
  filter(POS1 %in% c("名詞", "動詞", "形容詞")) %>%
  filter(!POS2 %in% c("接尾", "非自立")) %>%
  filter(!token %in% mystopwords) %>%
  filter(str_detect(token, "[^\\p{Hiragana}{1}|\\p{Katakana}{1}|\\p{Latin}{1}|\\p{N}{1}]")) %>% # ひらがな1文字、アルファベット1文字の形態素を除去
  mutate(token = str_replace_all(token, "-", " ")) %>% # "-" を含む形態素は後の処理で問題が出るので、スペースに置換する
  select(pm, doc_id, sentence_id, token)

### Ngramを作成するオブジェクトを生成
bigram <- ngram_tokenizer(n = 2)

res2 <- res %>%
    group_by(pm, doc_id) %>%
    summarise(token = bigram(token, sep = "-"), .groups = "keep") %>% # N = 2のNgramを作成
    count(token) %>%
    select(token, n) %>%
    separate(token, c("N1", "N2"), sep = "-") %>% # Ngramが「単語 - 単語」として出力されるので、1列ずつに分割する
    ungroup()

filename <- paste0("collocate_", str_replace_all(unique(res2$pm), " ", "_"), ".png")
df <- res2 %>% select(N1, N2, n) %>% arrange(desc(n)) %>% top_n(50) # 上位50個のNgramだけ描く
n <- graph.data.frame(df) %>% igraph::simplify(.) # ループなどを除去する
n2 <- ggnetwork(asNetwork(n), layout = "fruchtermanreingold")
p <- ggplot(n2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodes(color = "tomato", size = 6, alpha = 0.5) +
  geom_nodetext(aes(label = vertex.names), size = 6, check_overlap = TRUE) +
  theme_blank(base_family = "IPAexGothic")
ggsave(filename, p, device = ragg::agg_png, width = 4096, height = 4096, units = "px", bg = "white")


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
