require("readxl")
require("data.table")
require("dplyr")
require("tidyr")
require("stringr")
require("lubridate")
require("KoNLP")
require("ggplot2")
require("plotly")
require("wordcloud2")
require("arules")
require("arulesViz")
require("igraph")
require("visNetwork")




# preprocessing

sns = bind_rows(list.files("D:/bigcon/SNS데이터", pattern = "*.xlsx", full.names = T) %>% 
                  purrr::map_df(~ read_excel(., col_names = T))) %>% 
  rename(date = DATE) %>% 
  mutate(date = date %>% str_sub(1, 8) %>% ymd(),
         content_space = str_detect(CONTENT, " "),
         CONTENT = str_replace_all(CONTENT, "[^가-힣|^A-z|^:space:|^0-9|^~`!@#$%^&*(){};:',.<>/?+-_|\\[\\]]", " ")) %>% 
  arrange(date) %>% 
  mutate(content_space = zoo::na.fill0(content_space, 0))




# word change function

word_change = function(comment){
  
  comment = comment %>% 
    str_split("[~`!@#$%^&*(){};':,.<>/?+-_|\\[\\]]+", simplify = T) %>%
    str_split("[♡+♥+]", simplify = T) %>% 
    str_split("[z+k+]", simplify = T) %>%
    str_split("[ㄱ-ㅎ]", simplify = T) %>%
    str_split("[ㅏ-ㅣ]", simplify = T) %>%
    str_split("[\\^]", simplify = T) %>%
    str_split("[\\’]", simplify = T) %>%
    str_split("습니다$", simplify = T) %>% 
    str_split("됩니다$", simplify = T) %>%
    str_split("으로$", simplify = T) %>%
    str_split("입니다$", simplify = T) %>%
    str_split("까지$", simplify = T) %>%
    str_split("하는$", simplify = T) %>%
    str_split("있겠고$", simplify = T) %>%
    str_replace("내·외", "내외") %>%
    str_split("·", simplify = T) %>%
    str_replace("^미세먼$", "미세먼지") %>%
    str_replace("^세먼지$", "미세먼지") %>% 
    str_replace("^초미세$", "초미세먼지") %>% 
    str_replace("미세먼지PM10", "미세먼지 PM10") %>% 
    str_replace("미세먼지PM25", "미세먼지 PM25") %>% 
    str_split(" ", simplify = T) %>%
    str_replace("[♡♥]", "") %>% 
    str_replace("에요$", "") %>% 
    str_replace("예요$", "") %>% 
    str_replace("^해서$", "") %>% 
    str_replace("^근데$", "") %>% 
    str_replace("^하시$", "") %>% 
    str_replace("^하니$", "") %>% 
    str_replace("^안간$", "") %>% 
    str_replace("^때문$", "") %>% 
    str_replace("^때문$", "") %>% 
    str_replace("한테$", "") %>% 
    str_replace("해도$", "") %>% 
    str_replace("들은$", "") %>% 
    str_replace("때문에$", "") %>% 
    str_replace("같은$", "") %>% 
    str_replace("이라는$", "") %>% 
    Filter(function(x){str_length(x) >= 2}, .) %>% 
    Filter(function(x){str_length(x) < 10}, .)
  
  
  if(((comment %>% str_detect("^미세$") %>% sum) * (comment %>% str_detect("^먼지$") %>% sum) !=0) &&
     (comment %>% str_detect("^미세$") %>% sum) > (comment %>% str_detect("^먼지$") %>% sum)){
    comment = comment %>% str_remove_all("^미세$") %>% str_replace_all("^먼지$", "미세먼지") %>% c("미세")
  } else if(((comment %>% str_detect("^미세$") %>% sum) * (comment %>% str_detect("^먼지$") %>% sum) !=0) &&
            (comment %>% str_detect("^미세$") %>% sum) < (comment %>% str_detect("^먼지$") %>% sum)){
    comment = comment %>% str_remove_all("^먼지$") %>% str_replace_all("^미세$", "미세먼지") %>% c("먼지")
  } else if(((comment %>% str_detect("^미세$") %>% sum) * (comment %>% str_detect("^먼지$") %>% sum) !=0) &&
            (comment %>% str_detect("^미세$") %>% sum) == (comment %>% str_detect("^먼지$") %>% sum)){
    comment = comment %>% str_remove_all("^먼지$") %>% str_replace_all("^미세$", "미세먼지")
  }
    
  comment = comment %>% stringi::stri_remove_empty()
    
  return(comment)
}




# no space ratio in content visualization

sns %>% group_by(date, SECTION, content_space) %>% tally() %>% 
  spread(key = "content_len", "n") %>% 
  ggplot(aes(x = date, y = `0`/(`0` + `1`), color = SECTION)) + geom_line()




# extract noun 

sns_noun = data.frame()

for(i in 1:50){
  sns_noun = sns_noun %>% 
    bind_rows(
      data.table(sns %>% filter(content_space == 1) %>% slice(seq((2000*(i-1) + 1), 2000*i)) %>% select(date),
                 sns %>% filter(content_space == 1) %>% slice(seq((2000*(i-1) + 1), 2000*i)) %>% select(SECTION),
                 title = mapply(word_change, mapply(extractNoun, sns %>% filter(content_space == 1) %>% slice(seq((2000*(i-1) + 1), 2000*i)) %>% select(TITLE))),
                 content = mapply(word_change, mapply(extractNoun, sns %>% filter(content_space == 1) %>% slice(seq((2000*(i-1) + 1), 2000*i)) %>% select(CONTENT)))
                 )
    )
  print(i)
}

" below code can extractd all noun at once but it takes much time
# extract noun data frame

sns_noun = bind_rows(
  data.table(
    sns %>% filter(content_space == 1) %>% select(date),
    sns %>% filter(content_space == 1) %>% select(SECTION),
    title = mapply(word_change, 
                   purrr::map(sns %>% 
                                filter(content_space == 1) %>% 
                                select(TITLE), 
                              extractNoun)$TITLE),
    content = mapply(word_change, 
                     purrr::map(sns %>% 
                                  filter(content_space == 1) %>% 
                                  select(CONTENT), 
                                extractNoun)$CONTENT)
    ),
  data.table(
    sns %>% filter(content_space == 0) %>% select(date),
    sns %>% filter(content_space == 0) %>% select(SECTION),
    title = mapply(word_change, 
                   purrr::map(sns %>% 
                                filter(content_space == 0) %>% 
                                select(TITLE), 
                              extractNoun, autoSpacing = T)$TITLE),
    content = mapply(word_change, 
                     purrr::map(sns %>% 
                                  filter(content_space == 0) %>% 
                                  select(CONTENT), 
                                extractNoun, autoSpacing = T)$CONTENT)
    )
  ) %>% arrange(date)
"




# make long form of title, content for bar, line, network
# title

title_pre = do.call(bind_rows,
                    (sns_noun  %>% group_by(date, SECTION) %>% select(-content) %>% nest() %>% 
                       mutate(data = (data %>% unlist() %>% table() %>% 
                                        sort(decreasing = T))[data %>% unlist() %>% table() %>% 
                                                                sort(decreasing = T)>= 10] %>% list()
                              )
                     )$data
                    ) %>% 
  mutate(date = rep(sns_noun$date %>% unique() %>% head(40), each = 3),
         section = rep(c('뉴스', '카페', '블로그'), 40)) %>% 
  select(date, section, everything()) %>% 
  gather(Word, freq, names(.)[3:length(.)]) %>% 
  drop_na() %>% 
  rename(Freq = freq) %>% 
  group_by(date, Word) %>% 
  mutate(sum = sum(Freq)) %>% 
  ungroup()

title_table = title_pre %>% 
  left_join(title_pre %>% group_by(date, Word) %>% summarise(sum = sum(Freq)) %>% 
              arrange(date, desc(sum)) %>% 
              group_by(date) %>% 
              mutate(rank = frankv(sum, order = -1, ties.method = 'random')),
            by = c('date', 'Word', 'sum')
            )


# content

content_table = do.call(bind_rows,
                        (sns_noun  %>% group_by(date, SECTION) %>% select(-title) %>% nest() %>% 
                           mutate(data = (data %>% unlist() %>% table() %>% 
                                            sort(decreasing = T))[data %>% unlist() %>% table() %>% 
                                                                    sort(decreasing = T)>= 50] %>% list()
                           )
                        )$data
) %>% 
  mutate(date = rep(sns_noun$date %>% unique() %>% head(40), each = 3),
         section = rep(c('뉴스', '카페', '블로그'), 40)) %>% 
  select(date, section, everything()) %>% 
  gather(Word, freq, names(.)[3:length(.)]) %>% 
  drop_na() %>% 
  rename(Freq = freq) %>% 
  mutate(sum = sum(Freq)) %>% 
  group_by(date) %>% 
  mutate(rank = frankv(sum, order = -1, ties.method = "dense"))
  

title_table %>% filter(rank <= 20) %>% 
  spread(key = "section", value = "Freq") %>% 
  zoo::na.fill0(0) %>% 
  group_by() %>% 
  mutate(date = as.character(date)) %>% 
  plot_ly(x = ~ `뉴스`, 
          y = ~ reorder(Word, sum),
          frame = ~ date,
          type = "bar",
          orientation = "h",
          showlegend = T,
          name = "뉴스") %>%
  add_trace(x = ~ `블로그`,
            y = ~ reorder(Word, sum),
            name = "블로그",
            orientation = "h") %>% 
  add_trace(x = ~ `카페`,
            y = ~ reorder(Word, sum),
            name = "카페",
            orientation = "h") %>% 
  layout(
    xaxis = list(title = "빈도", autorange = T),
    yaxis = list(title = "단어"),
    barmode = "stack"
  ) %>% 
  animation_slider(
    currentvalue = list(prefix = "Date: ", font = list(color = "black"))
  )



# visualization
# bar chart
# title

title_table %>% filter(date == "2018-04-01") %>% 
  group_by(date) %>% filter(rank <= 20) %>% 
  group_by() %>% 
  mutate(date = as.character(date)) %>% 
  plot_ly(x = ~ Freq, 
          y = ~ reorder(Word, Freq),
          frame = ~ date,
          color = ~ Word,
          type = "bar",
          orientation = "h",
          showlegend = F,
          name = "뉴스") %>%
  add_trace(y ~ reorder(Word, Freq),
            name = "카페") %>% 
  add_trace(y ~ reorder(Word, Freq),
            name = "블로그") %>% 
  layout(
    xaxis = list(title = "빈도"),
    yaxis = list(title = "단어")
    ) %>% 
  animation_slider(
    currentvalue = list(prefix = "Date: ", font = list(color = "black"))
  )


# content

content_table %>% mutate(date = as.character(date)) %>% 
  plot_ly(x = ~ Freq, 
          y = ~ reorder(Word, Freq),
          frame = ~ date,
          color = ~ Word,
          type = "bar",
          orientation = "h",
          showlegend = F) %>%
  layout(
    xaxis = list(title = "빈도"),
    yaxis = list(title = "단어")
  ) %>% 
  animation_slider(
    currentvalue = list(prefix = "Date: ", font = list(color = "black"))
  )



# line graph
# title

# content




# sna
# title
content_table_sna = sns_noun %>% group_by(date, SECTION) %>% 
  select(-title) %>% nest() %>% 
  mutate(data = lapply(data, unlist))

# content

