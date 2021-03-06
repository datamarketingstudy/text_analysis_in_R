library(N2H4)
library(tidyverse)
library(magrittr)
library(data.table)
library(stringr)

# Read Data
workdf <- fread("workdf.csv")
str(workdf)

workdf %>%
    distinct() %>%
    str()

content_data <- workdf %>%
    select(ym, clubname, content)

content_data$ym <- as.factor(content_data$ym)

content_data %>% head()

# tidy text
content_data %<>%
    mutate(content = str_replace_all(content, "<b>", "") %>%
               str_replace_all("</b>", "") %>%
               str_replace_all("[ㄱ-ㅎ]+","") %>%
               str_replace_all("[/~,/.^&?]", "") %>%
               str_replace_all("\\[|\\]","") %>%
               str_squish())
head(content_data)
tail(content_data)

str(content_data)

# 통신3사 키워드 통일
refine_data <- content_data %>%
    mutate(content = str_replace_all(content, "LG|lg|lgu|lgu+|유플러스|엘지|lg유플러스|LGU|U+|u+|유플|lg u", "유플러스") %>%
               str_replace_all("SKT|skt|sk|SK|에스케이|sk텔레콤|SK텔레콤", "에스케이티") %>%
               str_replace_all("KT|kt|케이티|Kt|kT", "케이티"))

refine_data %>%
    filter(str_detect(content, "유플러스")) %>%
    count()

refine_data %>%
    filter(str_detect(content, "에스케이티")) %>%
    count()

refine_data %>%
    filter(str_detect(content, "케이티")) %>%
    count()

refine_data %<>%
    distinct()

glimpse(refine_data)



# Token
library(tidytext)
library(KoNLP)

useNIADic()
user_dic <- data.frame(term = c("엘지", "유플러스", "케이티", "에스케이", "에스케이티",
                                "sk텔레콤", "SK텔레콤", "lg유플러스", "LG유플러스", "유플"),
                       tag = "ncn")
buildDictionary(ext_dic = 'woorimalsam',
                user_dic = user_dic)

# user_dic <- data.frame(term = c("LG", "LG U+", "엘지", "sk", "SK", "lg", "lg u+",
#                          "kt", "KT", "유플러스", "lgu+", "LGU+", "SKT", "skt",
#                          "Thank U+", "U+", "케이티", "에스케이", "에스케이티",
#                          "sk텔레콤", "SK텔레콤"),
#                        tag = "ncn")

# Token

text_data <- refine_data %>%
    unnest_tokens(input = content,
                  output = word,
                  token = SimplePos09) %>%
    mutate(pos_order = 1:n())

head(text_data)
tail(text_data)

n_done <- text_data %>%
    filter(str_detect(word, "/n|/f")) %>%
    mutate(pos_done = str_remove(word, "/.*$"))

p_done <- text_data %>%
    filter(str_detect(word, "/p")) %>%
    mutate(pos_done = str_replace_all(word, "/.*$", "다"))

pos_done <- bind_rows(n_done, p_done) %>%
    arrange(pos_order) %>%
    filter(nchar(pos_done) > 1) %>%
    select(ym, clubname, pos_done)
pos_done

# Thank U+
pos_done$pos_done <- ifelse(pos_done$pos_done == "유플러스+", "Thank U+", pos_done$pos_done)

# Keyword Count
pos_count <- pos_done %>%
    group_by(ym) %>%
    count(pos_done, sort = T) %>%
    filter(str_count(pos_done) > 1) 

# Top 50 Keywords
top_50 <- pos_done %>%
    count(pos_done, sort = T) %>%
    filter(str_count(pos_done) > 1) %>%
    slice_max(n, n = 50)
top_50

## -- 월별 Top 10
# 동점 제외
top_10 <- pos_done %>%
    group_by(ym) %>%
    count(pos_done) %>%
    slice_max(n, n = 10, with_ties = F) # 상위 10개 추출

top_10 %>%
    print(n = Inf)

library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()
theme_set(theme_bw(base_family = "nanumgothic"))

# 워드 클라우드
library(ggwordcloud)

ggplot(top_50, aes(label = pos_done, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = "blackhansans") +
    scale_radius(limits = c(2, NA), range = c(2, 15)) +
    scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
    theme_minimal()

# Bar Plot 
ggplot(top_10, aes(x = reorder_within(pos_done, n, ym), y = n, fill = ym)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ ym, scales = "free_y") +
    scale_x_reordered() +
    labs(x = NULL, y = NULL, fill = "구분") +
    scale_fill_discrete(name = "연월",
                        labels = c("2021년 1월", "2021년 2월", "2021년 3월", "2021년 4월",
                                   "2021년 5월", "2021년 6월", "2021년 7월", "2021년 8월",
                                   "2021년 9월", "2021년 10월", "2021년 11월", "2021년 12월")) +
    theme(text = element_text(family = "blackhansans"))


# 장기고객 관련 키워드 게시물 추이

## 월별 통계
refine_data %>%
    # select(ym, content) %>%
    # distinct() %>%
    group_by(ym) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = ym, y = n, group = 1)) +
    geom_line(stat = "identity") +
    geom_point() +
    labs(x = "연월", y = "게시글 수", title = "네이버 카페 [장기고객] 키워드 등록 월별 게시글 수",
         caption = "*장기 렌트카 등 관련 자동차, 캠핑 카페 게시글 제외 \n 동일 내용 복수 게시글 중복 제거") +
    scale_x_discrete(labels = c("1월","2월","3월","4월","5월","6월",
                                "7월","8월","9월","10월","11월","12월")) +
    theme(text = element_text(family = "blackhansans"))

# 8월 통계
pos_count %>%
    filter(ym == '202108') %>%
    slice_max(n, n = 50, with_ties = F) %>%
    ggplot(aes(label = pos_done, size = n, color = n)) +
    geom_text_wordcloud(seed = 1234, family = "blackhansans") +
    scale_radius(limits = c(2, NA), range = c(2, 15)) +
    scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
    theme_minimal()

# 카페별 게시글 통계
# Bar Plot

content_data %>%
    group_by(clubname) %>%
    summarise(n = n()) %>%
    slice_max(n, n = 30) %>%
    ggplot(aes(x = order(clubname, -n), y = n)) +
    geom_col() +
    coord_flip() +
    # scale_x_reordered() +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(family = "blackhansans"))




# 동시 출현 빈도
library(widyr)
refine_data %<>%
    mutate(id = row_number())

pair_data <- refine_data %>%
    unnest_tokens(input = content,
                  output = word,
                  token = SimplePos22,
                  drop = F)

pair_data %>%
    select(word, content)

library(tidyr)
pair_data %<>%
    separate_rows(word, sep = "[+]")

pair_data %>%
    select(word, content)

# Noun Extract

noun <- pair_data %>%
    filter(str_detect(word, "/n|/f")) %>%
    mutate(word = str_remove(word, "/.*$"))

noun %>%
    select(word, content)

noun %>%
    filter(str_count(word) > 1) %>%
    count(word, sort = T)

# PV / PA Extract

pvpa <- pair_data %>%
    filter(str_detect(word, "/pv|/pa")) %>%
    mutate(word = str_replace(word, "/.*$", "다"))

pvpa %>%
    select(word, content)

pvpa %>%
    count(word, sort = T)

# Data Merge

text_keyword <- bind_rows(noun, pvpa) %>%
    filter(str_count(word) > 1) %>%
    arrange(id)

text_keyword %>%
    select(word, content)

# 동시 출현 빈도

library(widyr)

pair <- text_keyword %>%
    pairwise_count(item = word,
                   feature = id,
                   sort = T)
pair

pair %>%
    filter(item1 == "유플러스")

pair %>%
    filter(item1 == "케이티")

pair %>%
    filter(item1 == "에스케이티")

# 동시 출현 네트워크
library(tidygraph)

graph_comment <- pair %>%
    filter(n >= 35) %>%
    as_tbl_graph()

graph_comment

# network graph
library(ggraph)

ggraph(graph_comment) +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name))

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

set.seed(123)
ggraph(graph_comment, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "blackhansans") +
    theme_graph()

# Making Function
word_network <- function(x){
    ggraph(x, layout = "fr") +
        geom_edge_link(color = "gray50",
                       alpha = 0.5) +
        geom_node_point(color = "lightcoral",
                        size = 5) +
        geom_node_text(aes(label = name),
                       repel = T,
                       size = 5,
                       family = "blackhansans") +
        theme_graph()
}

set.seed(1234)
word_network(graph_comment)

# 유의어 처리

comment <- text_keyword %>%
    mutate(word = ifelse(str_detect(word, "장기"), "장기고객", word),
           word = ifelse(str_detect(word, "고객"), "장기고객", word))

pair2 <- comment %>%
    pairwise_count(item = word,
                   feature = id,
                   sort = T)

graph_comment2 <- pair2 %>%
    filter(n >= 35) %>%
    as_tbl_graph()

set.seed(4321)
word_network(graph_comment2)

# 연결 중심성과 커뮤니티 표현하기

# 연결 중심성(degree centrality)는 노드가 다른 노드들과 밀접하게 연결되는지 나타낸 값
# 커뮤니티(community)란 단어 간의 관계가 가까워 빈번하게 연결된 노드 집단을 의미

set.seed(707)
graph_comment3 <- pair2 %>%
    filter(n >= 35) %>%
    as_tbl_graph(directed = F) %>%
    mutate(centrality = centrality_degree(), # 연결 중심성
           group = as.factor(group_infomap())) # 커뮤니티
graph_comment3

set.seed(808)
ggraph(graph_comment3, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(aes(size = centrality,
                        color = group),
                    show.legend = F) +
    scale_size(range = c(5, 15)) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "blackhansans") +
    theme_graph()

# 네트워크 주요 단어 살펴보기

graph_comment3 %>%
    filter(name == "장기고객")

# 같은 커뮤니티로 분류된 단어 살펴보기
graph_comment3 %>%
    filter(group == 1) %>%
    arrange(-centrality) %>%
    data.frame()

# 연결 중심성이 높은 주요 단어 살펴보기
graph_comment3 %>%
    arrange(-centrality)

# 2번 그룹
graph_comment3 %>%
filter(group == 2) %>%
    arrange(-centrality) %>%
    data.frame()

# 키워드 원문 파악 (통신사 & 혜택)
text_keyword %>%
    filter(str_detect(content, "통신사") & str_detect(content, "혜택")) %>%
    select(content) %>%
    distinct()

# 키워드 원문 파악 (유플러스 & 에스케이티)
text_keyword %>%
    filter(str_detect(content, "유플러스") & str_detect(content, "에스케이티")) %>%
    select(content) %>%
    distinct()

## 단어 간 상관 분석 - 파이 계수

# 파이 계수(phi cofficient)는 두 단어가 함께 사용되는 경우가 각각 사용되는 경우에 비해 얼마나 많은지 나타낸 지표

word_cors <- comment %>%
    add_count(word) %>%
    filter(n >= 20) %>%
    pairwise_cor(item = word,
                 feature = id,
                 sort = T)
word_cors

word_cors %>%
    filter(item1 == "유플러스")

# 관심 단어별로 파이 계수가 큰 단어 추출하기
target <- c("장기고객", "혜택", "통신사", "유플러스", "에스케이티", "케이티")

top_cors <- word_cors %>%
    filter(item1 %in% target) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 8)

# bar graph
top_cors$item1 <- factor(top_cors$item1, levels = target)

ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
    geom_col(show.legend = F) +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL) +
    theme(text = element_text(family = "blackhansans"))

# 파이 계수로 네트워크 그래프 만들기

set.seed(1234)
graph_cors <- word_cors %>%
    filter(correlation >= 0.25) %>%
    as_tbl_graph(directed = F) %>%
    mutate(centrality = centrality_degree(),
           group = as.factor(group_infomap()))

set.seed(1234)
ggraph(graph_cors, layout = "fr") +
    geom_edge_link(color = "gray50",
                   aes(edge_alpha = correlation,
                       edge_width = correlation),
                   show.legend = F) +
    scale_edge_width(range = c(1, 4)) +
    geom_node_point(aes(size = centrality,
                        color = group),
                    show.legend = F) +
    scale_size(range = c(5, 10)) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "blackhansans") +
    theme_graph()
