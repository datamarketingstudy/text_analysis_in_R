library(N2H4)
library(tidyverse)
library(magrittr)

# News Data
url_2022 <- "https://news.naver.com/main/read.naver?mode=LSD&mid=sec&sid1=100&oid=001&aid=0013004709"
url_2021 <- "https://news.naver.com/main/read.naver?mode=LSD&mid=sec&sid1=102&oid=001&aid=0012217101"

cont_2022 <- getContent(url_2022)
cont_2021 <- getContent(url_2021)

# Create 'gubun' column
bd_22 <- cont_2022 %>%
    select(body) %>%
    mutate(gubun = '2022기사')
glimpse(bd_22)

bd_21 <- cont_2021 %>%
    select(body) %>%
    mutate(gubun = '2021기사')
glimpse(bd_21)

# Data Merge
bind_news <- bind_rows(bd_22, bd_21)
glimpse(bind_news)

# Text preprocessing (Only Korean & remove multiple spaces)
bind_news %<>%
    mutate(body = str_replace_all(body, "[^가-힣]", " ") %>%
               str_squish())
head(bind_news)

# Token
library(tidytext)
library(KoNLP)

text_news <- bind_news %>%
    unnest_tokens(input = body,
                  output = word,
                  token = extractNoun)
head(text_news)
tail(text_news)


# summary keywords by group
freq_news <- text_news %>%
    count(gubun, word) %>%
    filter(str_count(word) > 1) %>% # 두 글자 이상 추출
    filter(!word %in% c('연합', '뉴스', '반장', '박향', '총괄')) # 언론사명/직책 제외

head(freq_news)

# Top 10 Keywords
top_10 <- freq_news %>%
    group_by(gubun) %>%
    slice_max(n, n = 10) # 상위 10개 추출

top_10 %>%
    print(n = Inf)

# 동점 제외
top_10 <- freq_news %>%
    group_by(gubun) %>%
    slice_max(n, n = 10, with_ties = F) # 상위 10개 추출

library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()
theme_set(theme_bw(base_family = "nanumgothic"))

# Bar Plot 
ggplot(top_10, aes(x = reorder_within(word, n, gubun), y = n, fill = gubun)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ gubun, scales = "free_y") +
    scale_x_reordered() +
    labs(x = NULL, y = NULL, fill = "구분") +
    theme(text = element_text(family = "blackhansans"))

## 오즈비 - 상대적으로 중요한 단어 비교하기

# long form to wide form
library(tidyr)

df_wide <- freq_news %>%
    pivot_wider(names_from = gubun,
                values_from = n)
df_wide

# NA를 0으로 바꾸기
df_wide <- freq_news %>%
    pivot_wider(names_from = gubun,
                values_from = n,
                values_fill = list(n = 0))
df_wide

# Keywords Ratio
freq_wide <- df_wide %>%
    mutate(ratio_2021 = ((`2021기사`+1)/(sum(`2021기사`+1))), # 2021년에서 단어의 비중
           ratio_2022 = ((`2022기사`+1)/(sum(`2022기사`+1)))) # 2022년에서 단어의 비중
# Keywords Odds Ratio
freq_wide %<>%
    mutate(odds_ratio = ratio_2022/ratio_2021)

# 2021년 정부 발표 기사 언급 키워드 대비 많이 사용된 단어
freq_wide %>%
    arrange(-odds_ratio)

# 2022년 정부 발표 기사 언급 키워드 대비 2021년 많이 사용되었던 단어
freq_wide %>%
    arrange(odds_ratio)

## Odd Ratio 기준 TOP 10 추출
top10 <- freq_wide %>%
    filter(rank(odds_ratio, ties.method = 'first') <= 10 |
               rank(-odds_ratio, ties.method = 'first') <= 10)

top10 %>%
    arrange(-odds_ratio) %>%
    print(n = Inf)

# Add Columns - gubun & n
top10 %<>%
    mutate(gubun = ifelse(odds_ratio > 1, '2022년', '2021년'),
           n = ifelse(odds_ratio > 1, `2022기사`, `2021기사`))

# Odds Ratio - Bar Plot
ggplot(top10, aes(x = reorder_within(word, n, gubun), y = n, fill = gubun)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ gubun, scales = "free") +
    scale_x_reordered() +
    labs(x = NULL, y = NULL, fill = "구분") +
    theme(text = element_text(family = "blackhansans"))

# 주요 단어가 사용된 문장 살펴보기
bind_news2 <- bind_rows(bd_22, bd_21)
bind_news2 %<>%
    mutate(body = str_replace_all(body, "[\\,]", " ") %>%
               str_squish()) # . 기준으로 sentence 구분하기 때문에

sentence_news <- bind_news2 %>%
    unnest_tokens(input = body,
                  output = sentence,
                  token = "sentences")
head(sentence_news)
tail(sentence_news)

# 2021년 '예방접종'
sentence_news %>%
    filter(gubun == '2021기사' & str_detect(sentence, "예방접종")) 
# 2022년 '일상'
sentence_news %>%
    filter(gubun == '2022기사' & str_detect(sentence, "일상")) 

# 중요도가 비슷한 단어 살펴보기
freq_wide %>%
    arrange(abs(1 - odds_ratio)) %>%
    head(10)

freq_wide %>%
    filter(`2021기사` > 1 & `2022기사` > 1) %>%
    arrange(abs(1 - odds_ratio)) %>%
    head(5)

