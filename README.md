# 応用プログラミング3

<p align="right">
専修大学ネットワーク情報学部<br />
兼任講師<br />
<a href="https://mana.bi/">田中 健太</a>
</p>

[専修大学ネットワーク情報学部](http://ni.ne.senshu-u.ac.jp/)の専門科目「応用プログラミング3」2023年度のリポジトリです。2021年度後期から開講しています。

講義資料などの取り扱いは、リポジトリに設定したMITライセンスに準じます。**自由にお使いいただけますが、代わりに何か仕事 (研修、非常勤講師、教材・書籍執筆など) をいただければうれしいです。**

## シラバス

この講義は、R言語の応用的な話題について、以下のようなテーマで取り上げます (シラバスより抜粋)。

### 到達目標

1. R言語の近年の動向を捉え、適切な開発環境、パッケージを選択してプログラミングできる
1. tidyverseを念頭に置いたモダンなデータ分析フローを理解し、分析を実践できる
1. 機械学習やテキストマイニングなど、分析の目的に応じた手法を選択し、Rプログラムとして実装できる

### 講義概要

データ分析のためのプログラミング言語であるRについて、近年 (2010年代後半以降) の開発動向を紹介し、ビジネスや研究の現場で広く使われるパッケージの活用方法を、実際にプログラミングをしながら学習します。講義を通じて、実践的なRプログラミングのスキルを身に着けることを目標とします。なお、本講義の履修者は、R言語について基本的な知識を有していると想定しています。前提知識として想定するのは以下の事柄です。

#### 【前提知識】※第1回の授業で簡単な振り返り、確認を行います。

1. Rでファイルからデータを読み込むことができる
1. オブジェクト (変数) を作成し、要素 (行や列) を参照できる
1. オブジェクトに関数 (データ加工、グラフィックス、統計処理など) を適用し、結果を得ることができる

#### 講義計画

以下のような、Rを活用したモダンなデータ分析に関するトピックを取り上げます。

1. ガイダンス、Rプログラミングの基本: [教材PDF](./01_guidance/01_guidance_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7220586)
1. Rプログラミングの基本、最近の動向について: [教材PDF](./02_trend/02_trend_lecture_note.pdf) / [補足資料PDF](./02_trend/02_trend_supplement_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7221410)
1. R Markdownによるレポート作成: [教材PDF](./03_rmarkdown/03_rmarkdown_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7221461)
1. GUIによるデータ分析: Radiant等: [教材PDF](./04_radiant/04_radiant_lecture_note.pdf) / [ハンズオン資料](./04_radiant/04_radiant_handson.pdf) / [ハンズオン環境](https://posit.cloud/content/7221491)
1. ggplot2パッケージによるグラフィックス作成 (1): [教材PDF](./05_ggplot2_01/05_ggplot2_01_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7221626)
1. ggplot2パッケージによるグラフィックス作成 (2): [教材PDF](./06_ggplot2_02/06_ggplot2_02_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7221639)
1. tidyverseによるデータハンドリング (1): [教材PDF](./07_tidyverse_01/07_tidyverse_01_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7222464)
1. tidyverseによるデータハンドリング (2): [教材PDF](./08_tidyverse_02/08_tidyverse_02_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7222536)
1. Rによる統計モデリング、機械学習の基礎: [教材PDF](./09_stats_ml/09_stats_ml_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7222568)
1. 機械学習フレームワークによる機械学習 (1): [教材PDF](./10_ml_framework_01/10_ml_framework_01_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7223174)
1. 機械学習フレームワークによる機械学習 (2): [教材PDF](./11_ml_framework_02/11_ml_framework_02_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7223277)
1. RによるWebスクレイピング: [教材PDF](./12_scraping/12_scraping_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7285996)
1. 日本語テキストマイニング (1): [教材PDF](./13_text_mining_01/13_text_mining_01_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7392545)
1. 日本語テキストマイニング (2): [教材PDF](./14_text_mining_02/14_text_mining_02_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7392579)
1. reticulateによるPythonとの連携 / Shinyによるアプリケーション開発: [教材PDF](./15_reticulate_shiny/15_reticulate_shiny_lecture_note.pdf) / [ハンズオン環境](https://posit.cloud/content/7392647)

#### 授業時間外の予習・復習・課題

講義概要で示した、R言語についての前提知識に不安がある方は、以下の参考書等で事前に学習してください。

授業ごとに毎回、Rプログラムを作成する課題を指示します (作業時間2時間程度)。翌週の講義開始時刻までにGoogle Classroom等で提出してください。また、予習・復習として、講義で紹介する各種パッケージに関する情報収集 (Web検索、ドキュメント閲覧) をすると良いでしょう。

### 教科書・参考書

講義資料は、随時Google Classroom等で共有します。以下は、参考書ですので、購入は必須ではありません。R言語の基礎知識に不安がある、またはさらに理解を深めたい方は通読してください。多くの書籍が専修大学の図書館に収蔵されています。

※GitHub上の本文書では、以下の書籍のリンク先として、Amazonアフィリエイトを設定しています。(そら、小銭が欲しいですもの)

1. 松村優哉, 湯谷啓明, 紀ノ定保礼, 前田和寛, 2021, [改訂2版 RユーザのためのRStudio実践入門〜tidyverseによるモダンな分析フローの世界](https://amzn.to/3t4qVfr), 技術評論社
1. 浅野正彦, 中村公亮, 2018, [はじめてのRStudio: エラーメッセージなんかこわくない](https://amzn.to/3ceN7x6), オーム社
1. 松村優哉, 瓜生真也, 吉村広志, 2023, [Rユーザのためのtidymodels［実践］入門〜モダンな統計・機械学習モデリングの世界](https://amzn.to/3Pu5DW9), 技術評論社
1. Jared P. Lander (著), 高柳慎一, 津田真樹, 牧山幸史, 松村杏子, 簑田高志 (翻訳), 2018, [みんなのR 第2版](https://amzn.to/38j9CzL), マイナビ出版
1. 石田 基広, 2017, [Rによるテキストマイニング入門 (第2版)](https://amzn.to/38lon5f), 森北出版
1. 有賀友紀, 大橋俊介, 2021, [RとPythonで学ぶ［実践的］データサイエンス＆機械学習【増補改訂版】](https://amzn.to/3vr5O8V), 技術評論社

### ＜成績評価基準・評価の配分等＞

以下の基準で提出物を評価します。

1. 課題の意図を理解し、パッケージ等を活用して、要求を満たすプログラムを実装できたか
1. 要求を満たした上で、よりシンプルで可読性の高いプログラムを実装できたか
1. グラフィックスのラベルや目盛、レポートのフォーマット等を工夫した、分析結果を第三者に伝えることを意識したプログラムであるか

## 講義資料

### 配布資料

授業で配布する資料を公開します。PowerPointのスライド+ノートの形式です。GitHubでは、「ノート」スタイルで出力したPDFと、授業内のハンズオン演習および週次課題のソースコードを公開します。

資料は、[bookdownパッケージ](https://bookdown.org/)でRMarkdownから作成しました。

### ハンズオン環境

授業では、[Posit Cloud](https://posit.cloud/)にデプロイした環境で、講義内容に関連したハンズオン演習ができます。

### 講義動画

講義動画もそのうち作成・公開予定です。
