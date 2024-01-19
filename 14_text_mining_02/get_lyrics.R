library(tidyverse)
library(httr)
library(rvest)

base_url <- "https://www.uta-net.com"

#urls <- "https://www.uta-net.com/artist/6636/"
# AKB48, 乃木坂46, 櫻坂46, 日向坂46, 米津玄師, キャンディーズ, さだまさし, 吉田拓郎, サンボマスター, BUMP OF CHICKENを選択
urls <- c("https://www.uta-net.com/artist/6636/0/1/", "https://www.uta-net.com/artist/6636/0/2/", "https://www.uta-net.com/artist/12550/", "https://www.uta-net.com/artist/29512/", "https://www.uta-net.com/artist/22163/", "https://www.uta-net.com/artist/12795/", "https://www.uta-net.com/artist/1287/", "https://www.uta-net.com/artist/1399/0/1/", "https://www.uta-net.com/artist/1399/0/2/", "https://www.uta-net.com/artist/1399/0/3/", "https://www.uta-net.com/artist/2100/0/1/", "https://www.uta-net.com/artist/2100/0/2/", "https://www.uta-net.com/artist/1416/", "https://www.uta-net.com/artist/126/")

# User Agentを偽装する
pseudo_user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"

# データフレームの初期化
df <- data.frame(id = numeric(), artist = character(), title = character(), lyric = character(), url = character())

for (i in 1:length(urls)) {
    res <- GET(urls[i], user_agent(pseudo_user_agent))
    html <- content(res)
    links <- html %>% html_elements("td.sp-w-100")
    for (link in links) {
        href <- link %>% html_element("a") %>% html_attr("href")
        song_url <- paste0(base_url, href, collapse = "")
        song_res <- GET(song_url, user_agent(pseudo_user_agent))
        song_html <- content(song_res)
        song_title <- song_html %>% html_element("h2") %>% html_text2()
        artist_name <- song_html %>% html_element("h3") %>% html_text2()
        lyric <- song_html %>% html_element("div#kashi_area") %>% html_text2() %>% str_replace_all("\\n+", " ")
        tmp_df <- data.frame(id = 0, artist = artist_name, title = song_title, lyric = lyric, url = song_url)
        df <- df %>% bind_rows(tmp_df)
        print(paste0("now processing '", artist_name, "', '", song_title, "'"))
        Sys.sleep(1)
    }
}

df[["id"]] <- as.numeric(factor(df[["artist"]], levels = c("AKB48", "乃木坂46", "櫻坂46", "日向坂46", "米津玄師", "キャンディーズ", "さだまさし", "吉田拓郎", "サンボマスター", "BUMP OF CHICKEN")))

write_csv(df, "pops_lyrics.csv")
