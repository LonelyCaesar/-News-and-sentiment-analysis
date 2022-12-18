#資料輸入與前處理

# Required Library
library("tidyverse")

# Read Dataset
newspaper.articles <- read_csv("20191006_Newspaper Articles.csv")

head(newspaper.articles, 5)

# Remove stop words
my_stop_words <- c(
  "政治", "的", "你", "我", "他", "中時", "記者"
)

newspaper.articles$text <- gsub(
  paste(my_stop_words , collapse = "|"),
  " ",
  newspaper.articles$text 
)

# Remove symbols
newspaper.articles$text <- gsub(
  paste(c(
    "《", "》", "【", "】", "｜", "(",")", "®",
    "「", "」", "！", "？", "：", "\\.\\.\\.")
    , collapse = "|"),
  "",
  newspaper.articles$text
)

# Remove numbers
library(tm)

newspaper.articles$text <- removeNumbers(newspaper.articles$text)

head(newspaper.articles, 5)

#結巴斷詞

library(jiebaR)

# Initialize a JiebaR worker
wk <- worker(stop_word = jiebaR::STOPPATH)

# Add customized terms
customized_terms <- c(
  "辣台妹", "蔡英文", "賴清德", "威廉賴", "韓國瑜", "朱立倫", "黃捷",  "陳其邁",
  "自經區", "柯P", "柯文哲", "小馬", "扁", "阿扁", "陳水扁",  "時力", "黃捷",
  "吳敦義", "潘恆旭", "潘孟安",  "旗津", "中和", "板橋", "新北", "高雄",
  "春吶", "中共", "藍委", "綠委", "盧秀燕", "侯友宜", "王金平", "羅文嘉", "民進黨",
  "國民黨", "高嘉瑜", "習近平", "傅崐萁", "韓粉", "小英", "韓黑", "張琍敏", "前總統",
  "陳柏惟", "江啟臣", "李佳芬", "蔡政府", "神隱", "王定宇",  "亡國感", "打馬悍將",
  "臉書社團", "陳菊", "噓爆", "台中", "陳致中", "踢爆", "趙天麟", "鹽埕", "苓雅",
  "前金", "香川縣", "瀨戶內", "蔡壁如", "今年度", "時代力量", "芒果乾", "反送中",
  "台東", "連儂牆", "陳家欽", "鴻海", "新北市", "台北", "陳佩琪"
)
new_user_word(wk, customized_terms)

# segment terms and separate by blank
newspaper.articles$text <- sapply(
  as.character(newspaper.articles$text),
  function(char) segment(char, wk) %>% paste(collapse = " "))

head(newspaper.articles, 5)

library(tidytext)
tok99 <- function(t) str_split(t,"[ ]{1,}")
tidy.articles <- newspaper.articles %>%
  filter(newspaper == "Liberty Times") %>%
  unnest_tokens(word, text, token=tok99)

head(tidy.articles, 5)