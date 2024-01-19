if(require("withr")){
    print("withr is loaded correctly")
} else {
    print("trying to install withr...")
    install.packages("withr")
    if(require("withr")){
        print("withr installed and loaded")
    } else {
        stop("could not install withr")
    }
}
# 下記の "/opt/R/4.3.2/lib/R/bin" は実行環境によって変わりますので、適宜読み替えてください。
home <- Sys.getenv("HOME")
Sys.setenv(PATH=paste0("/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/opt/R/4.3.2/lib/R/bin:/usr/lib/rstudio-server/bin/postback:", home, "/usr/local/bin"))
Sys.setenv(CPPFLAGS=paste0("-I", home, "/usr/local/include"))
Sys.setenv(LDFLAGS=paste0("-I", home, "/usr/local/lib"))
Sys.setenv(LD_LIBRARY_PATH=paste0(home, "/usr/local/lib"))
withr::with_makevars(c(CPPFLAGS=paste0("-I", home, "/usr/local/include"), LDFLAGS=paste0("-L", home, "/usr/local/lib"), LD_LIBRARY_PATH=paste0(home, "/usr/local/lib")), install.packages("RMeCab", repos = "https://rmecab.jp/R"), assignment="+=")
withr::with_makevars(c(MECAB_DEFAULT_RC = paste0(home,"/usr/local/etc/mecabrc")), remotes::install_github("paithiov909/gibasa"), assignment="=")

# テスト
dyn.load(paste0(home, "/usr/local/lib/libmecab.so.2"))
library(RMeCab)
RMeCabC("すもももももももものうち")