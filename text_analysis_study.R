library(stringr)
library(data.table)
library(tidyverse)

gimbapdf <- fread("gimbap.csv")
glimpse(gimbapdf)

txt <- gimbapdf$subject

str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
str_replace_all(string = txt, pattern = "\\W", replacement = "")
str_replace_all(string = txt, pattern = "[ㄱ-ㅎ]+", replacement = "")  %>% head()

# Token화하기

text <- gimbapdf %>%
    select(content) %>%
    mutate(content = str_replace_all(content, "<b>", "") %>%
               str_replace_all("</b>", "")) %>%
    head(10)

library(tidytext)

# 문장 기준 토큰화
text %>%
    unnest_tokens(input = content,
                  output = word,
                  token = "sentences")

# 단어 기준 토큰화
text %>%
    unnest_tokens(input = content,
                  output = word,
                  token = "words")

# 문자 기준 토큰화
text %>%
    unnest_tokens(input = content,
                  output = word,
                  token = "characters")


## ====== 단어 빈도 분석하기 ====== ##

word_space <- text %>%
    unnest_tokens(input = content,
                  output = word,
                  token = "words")

word_sum <- word_space %>%
    count(word, sort = T)

# 한 글자로 된 단어 제거하기
str_count("배")
str_count("사과")

# 두 글자 이상만 남기기
word_sum %>%
    filter(str_count(word) > 1)

# 자주 사용된 단어 추출하기

top_20 <- word_sum %>%
    head(20)
top_20
