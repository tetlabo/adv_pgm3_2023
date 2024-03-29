### 応用プログラミング3
### 第14回　日本語テキストマイニング (2)
### 週次課題用プログラム
### 提出期限: 2023-01-23 23:59:59
### 
### 提出者の情報を記述してください
### 専修大学ネットワーク情報学部
### 学籍番号: 
### 氏名: 


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


#--------------------------------------------------------------------#
# 作成したプログラムをPosit Cloudからダウンロードし、Google Classroomの
# 課題ページに提出してください。ファイル名を変更する必要はありません。
