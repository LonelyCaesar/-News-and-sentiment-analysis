install.packages("rJava")
install.packages("jiebaR")
install.packages("tmcn")

#library(dplyr)
#library(lubridate)
#library(stringr)
#library(jiebaR)
#library(wordcloud) # 非互動式文字雲
#library(wordcloud2) # 互動式文字雲

# 範例文章
content <- c("辣台妹", "蔡英文", "賴清德", "威廉賴", "韓國瑜", "朱立倫", "黃捷",  "陳其邁",
             "自經區", "柯文哲", "小馬", "扁", "阿扁", "陳水扁",  "時力", "黃捷",
             "吳敦義", "潘恆旭", "潘孟安",  "旗津", "中和", "板橋", "新北", "高雄",
             "春吶", "中共", "藍委", "綠委", "盧秀燕", "侯友宜", "王金平", "羅文嘉", "民進黨",
             "國民黨", "高嘉瑜", "習近平", "傅崐萁", "韓粉", "小英", "韓黑", "張琍敏", "前總統",
             "陳柏惟", "江啟臣", "李佳芬", "蔡政府", "神隱", "王定宇",  "亡國感", "打馬悍將",
             "臉書社團", "陳菊", "噓爆", "台中", "陳致中", "踢爆", "趙天麟", "鹽埕", "苓雅",
             "前金", "香川縣", "瀨戶內", "蔡壁如", "今年度", "時代力量", "芒果乾", "反送中",
             "台東", "連儂牆", "陳家欽", "鴻海", "新北市", "台北", "陳佩琪"
)
cutter <- worker(bylines = F)
# 使用斷詞器斷詞(有兩種寫法)
#segment(content, cutter)
cutter[content]

new_words <- c("林昶佐","探勘井","頁岩油","輕值原油", "蕭煌奇")
# 一次只能加入一個詞，常常需要搭配迴圈使用
for (i in 1:length(new_words)) {
  new_user_word(cutter, new_words[i])
}

content <- str_remove_all(content, "[0-9a-zA-Z]+?")
cutter[content]

# 匯出新詞
new_words <- c("紐約商業交易所","探勘井","頁岩油","輕值原油")
writeLines(new_words, "new_words.txt")
# 設定停止詞
stop_words <- c("在","的","下","個","來","至","座","亦","與","或","日","月","年","週")
writeLines(stop_words, "stop_words.txt")
# 重新定義斷詞器，匯入停止詞
cutter <- worker(user = "new_words.txt", stop_word = "stop_words.txt", bylines = FALSE)
seg_words <- cutter[content]
seg_words

# 計算詞彙頻率
txt_freq <- freq(seg_words)
# 由大到小排列
txt_freq <- arrange(txt_freq, desc(freq))
# 檢查前5名
head(txt_freq)

par(family=("NotoSansCJKtc-Medium")) #一般wordcloud需要定義字體，不然會無法顯示中文
# 一般的文字雲 (pkg: wordcloud)
wordcloud(txt_freq$char, txt_freq$freq, min.freq = 2, random.order = F, ordered.colors = F, colors = rainbow(nrow(txt_freq)))
seg_words <- cutter[content]
seg_words

# 互動式文字雲 (pkg: wordcloud2)
wordcloud2(filter(txt_freq, freq > 1), 
           minSize = 2, fontFamily = "NotoSansCJKtc-Medium", size = 1)