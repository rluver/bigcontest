require("readxl")
require("KoNLP")
require("tidyverse")
require("reshape")
require("lubridate")
require("wordcloud2")
require("arules")
require("igraph")




# 데이터 로드 
sns1 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_1.xlsx", col_names = T)
sns2 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_2.xlsx", col_names = T)
sns3 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_3.xlsx", col_names = T)
sns4 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_4.xlsx", col_names = T)
sns5 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_5.xlsx", col_names = T)
sns6 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_6.xlsx", col_names = T)
sns7 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_7.xlsx", col_names = T)
sns8 = read_excel("d:/공모전_시작이반이다/raw/SNS데이터/SNS_8.xlsx", col_names = T)

# 데이결 결합
sns = rbind(sns1, sns2, sns3, sns4, sns5, sns6, sns7, sns8)

# 시간 변환
sns = sns %>% mutate(date = ymd(substr(sns$DATE, 1, 8))) %>% arrange(date)

# 사전 로드
useSejongDic()




# 단어 담을 매트릭스 생성
title_matrix = 0
contents_matrix = 0

# 워드클라우드 (50개 샘플)

for(j in 1){
  title = list()
  contents = list()
  for (i in 1:50){
    if(sns$date[i] == unique(sns$date)[j]){
      title = list(title, sns$TITLE[i] %>% melt %>% as_tibble)
      contents = list(contents, sns$CONTENT[i] %>% melt %>% as_tibble)
    }
  }
  title = unlist(title)
  title = as.vector(title)
  title = sapply(title, extractNoun, USE.NAMES = F)
  title_unlist = unlist(title)
  
  title_unlist = Filter(function(x) {nchar(x) >= 2}, title_unlist)
  title_unlist = gsub("[:punct:]","", title_unlist)
  title_unlist = gsub("[:blank:]","", title_unlist)
  title_unlist = gsub("[:alnum:]","", title_unlist)
  title_unlist = gsub("^미세$","미세먼지", title_unlist)
  title_unlist = gsub("^먼지$","미세먼지", title_unlist)
  title_unlist = gsub("^미세먼$","미세먼지", title_unlist)
  title_unlist = gsub("^세먼지$","미세먼지", title_unlist)
  
  title_wordcount = table(title_unlist)
  title_ordered = head(sort(title_wordcount, decreasing = T), 100)
  title_matrix = cbind(title_matrix, title_ordered)
  
  contents = unlist(contents)
  contents = as.vector(contents)
  contents = sapply(contents, extractNoun, USE.NAMES = F)
  contents_unlist = unlist(contents)
  
  contents_unlist = Filter(function(x){nchar(x)>=2}, contents_unlist)
  contents_unlist = gsub("[:punct:]","", contents_unlist)
  contents_unlist = gsub("[:blank:]","", contents_unlist)
  contents_unlist = gsub("[:alnum:]","", contents_unlist)
  contents_unlist = gsub("^미세$","미세먼지", contents_unlist)
  contents_unlist = gsub("^먼지$","미세먼지", contents_unlist)
  contents_unlist = gsub("^미세먼$","미세먼지",contents_unlist)
  contents_unlist = gsub("^세먼지$","미세먼지",contents_unlist)
  
  contents_wordcount = table(contents_unlist)
  contents_ordered = head(sort(contents_wordcount, decreasing = T), 100)
  contents_matrix = cbind(contents_matrix, contents_ordered)
}





# 열에 단어 추가, 형 변환

title_matrix = as.data.frame(title_matrix)
title_matrix[, 1] = as.character(row.names(title_matrix))
title_matrix[, 2] = as.numeric(as.character(title_matrix$title_ordered))

contents_matrix = as.data.frame(contents_matrix)
contents_matrix[, 1] = as.character(row.names(contents_matrix))
contents_matrix[, 2] = as.numeric(as.character(contents_matrix$contents_ordered))

title_matrix




## SNA (500개 샘플)

# 500개 열에 대해 명사 추출
etitle = Map(extractNoun, sns$TITLE[1:500])
econtents = Map(extractNoun, sns$CONTENT[1:500])




# 중복 내용이 있으제 제거
tran_title = unique(etitle)
tran_contents = unique(econtents)

# 각 리스트에서 중복값 제거
tran_title = sapply(tran_title, unique)
tran_contents = sapply(tran_contents, unique)

# 두 글자 이상 단어 추출
tran_title = sapply(tran_title, function(x){Filter(function(y){nchar(y) > 1 && is.hangul(y)},x)})
tran_contents = sapply(tran_contents, function(x){Filter(function(y){nchar(y) > 1 && is.hangul(y)},x)})

# transaction 클래스로 변화
word_tran_title = as(tran_title, "transactions")
word_tran_contents = as(tran_contents, "transactions")

# co-occurance table
title_table = crossTable(word_tran_title)
contents_table = crossTable(word_tran_contents)

# 연관규칙분석 (0.05를 변경하여 조절 가능)
ares_title = apriori(word_tran_title, parameter = list(supp = 0.05, conf = 0.05))




ares_contents = apriori(word_tran_contents, parameter = list(supp = 0.05, conf = 0.05))




# 라벨링
title_rules = labels(ares_title, ruleSep=" ")
contents_rules = labels(ares_contents, ruleSep=" ")

# 공백 기준으로 문자열 분리
title_rules = sapply(title_rules, strsplit, " ",  USE.NAMES = F)
contents_rules = sapply(contents_rules, strsplit, " ",  USE.NAMES = F)

# 위결 결과를 rbind
title_rulemat = do.call("rbind", title_rules)
contents_rulemat = do.call("rbind", contents_rules)

# edge 리스트 대입
title_ruleg <- graph.edgelist(title_rulemat, directed=F)
contents_ruleg <- graph.edgelist(contents_rulemat, directed=F)

# 결과 확인
plot.igraph(title_ruleg, vertex.label=V(title_ruleg)$name, vertex.label.cex=0.5, vertex.size=20)




plot.igraph(contents_ruleg, vertex.label=V(contents_ruleg)$name, vertex.label.cex=0.5, vertex.size=20)


# 아래는 1년 추출 코드

# for(j in unique(sns$date)){
#     title = list()
#     contents = list()
#     for (i in 1:dim(sns)[1]){
#         if(sns$date[i] == j){
#             title = list(a, sns$TITLE[i] %>% melt %>% as_tibble)
#             contents = list(b, sns$CONTENT[i] %>% melt %>% as_tibble)
#         }
#     }
# 
#     title = unlist(title)
#     title = as.vector(title)
#     title = sapply(title, extractNoun, USE.NAMES = F)
#     title_unlist = unlist(title)
#     title_unlist = Filter(function(x) {nchar(x) >= 2}, title_unlist)
#     title_unlist = gsub("[:punct:]","", title_unlist)
#     title_unlist = gsub("[:blank:]","", title_unlist)
#     title_unlist = gsub("[:alnum:]","", title_unlist)
#     title_unlist = gsub("^미세$","미세먼지", title_unlist)
#     title_unlist = gsub("^먼지$","미세먼지", title_unlist)
#     title_unlist = gsub("^미세먼$","미세먼지", title_unlist)
#     title_unlist = gsub("^세먼지$","미세먼지", title_unlist)
# 
#     title_wordcount = table(title_unlist)
#     title_ordered = head(sort(title_wordcount, decreasing = T), 100)
#     title_matrix = cbind(title_matrix, title_ordered)
# 
#     
#     contents = unlist(contents)
#     contents = as.vector(contents )
#     contents = sapply(contents, extractNoun, USE.NAMES = F)
#     contents_unlist = unlist(contents)
# 
#     contents_unlist = Filter(function(x){nchar(x)>=2}, contents_unlist)
#     contents_unlist = gsub("[:punct:]","", contents_unlist)
#     contents_unlist = gsub("[:blank:]","", contents_unlist)
#     contents_unlist = gsub("[:alnum:]","", contents_unlist)
#     contents_unlist = gsub("^미세$","미세먼지", contents_unlist)
#     contents_unlist = gsub("^먼지$","미세먼지", contents_unlist)
#     contents_unlist = gsub("^미세먼$","미세먼지",contents_unlist)
#     contents_unlist = gsub("^세먼지$","미세먼지",contents_unlist)
# 
#     contents_wordcount = table(contents_unlist)
#     contents_ordered = head(sort(contents_wordcount, decreasing = T), 100)
#     contents_matrix = cbind(contents_matrix, contents_ordered)
# }
#
#
# title_matrix = title_matrix[, - 1]
# contents_matrix = contents_matrix[, - 1]
# 
# 
# colnames(title_matrix) = c(unique(sns$date))
# colnames(contents_matrix) = c(unique(sns$date))
# 
# 
# t(title_matrix)
# t(contents_matrix)