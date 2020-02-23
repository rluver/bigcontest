require("data.table")
require("readxl")
require("dplyr")




# data load

source("revised/preprocessing_weather.R", encoding = "UTF-8")

card = fread("D:/bigcon/카드메출데이터/CARD_SPENDING_190809.txt", header = T) %>% 
  rename(days = STD_DD) %>% 
  mutate(days = ymd(days))

codetable = read_excel("D:/bigcon/카드메출데이터/2019빅콘테스트_신한카드소비데이터_레이아웃_최종_190711.xlsx", 
                       sheet = "(참고) 행정동코드", col_names = T) %>% 
  mutate(`행정동명` = str_replace_all(`행정동명`, "\\.", ","))




# var setting

"%!in%" = Negate("%in%")
jongro = (weather_day %>% filter(gu == "종로구", 
                                 dong %!in% c("명동", "천연동")))$dong %>% unique()
nowon = (weather_day %>% filter(gu == "노원구"))$dong %>% unique()
dong = c(jongro, nowon)
dongcd = (codetable %>% filter(`행정동명` %in% dong) %>% 
            arrange(desc(`구명`), `행정동명`))$`행정동코드` %>% as.numeric()
gender = c("M", "F")
age =  seq(20, 65, by = 5)




# matrix setting

mat = array(matrix("NA", nc = 33, nr = 61),
            dim = c(61, 33, card$MCT_CAT_CD %>% unique() %>% length()), 
                    dimnames = list(rep(" ", 61), 
                                    c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon),
                                    str_c("cat", card$MCT_CAT_CD %>% unique() %>% sort())
                                    )
            )

mat[1, 1, ] = "종로구" ; mat[1, 18, ] = "노원구"
mat[, 17, ] = ""
mat[2:61, 1,] = "" ; mat[2:61, 18, ] = "" ; mat[31, , ] = ""
mat[1, 2, ] = "남자" ; mat[1, 19, ] = "남자"
mat[32, 2, ] = "여자" ; mat[32, 19, ] = "여자"
mat[2:30, 2, ] = "" ; mat[2:30, 19, ] = ""
mat[33:61, 2, ] = "" ; mat[33:61, 19, ] = ""
mat[seq(1, 30, 3), 3, ] = age ; mat[seq(32, 61, 3), 3, ] = age
mat[seq(2, 30, 3), 3, ] = "CNT" ; mat[seq(33, 61, 3), 3, ] = "CNT"
mat[seq(3, 30, 3), 3, ] = "AMT" ; mat[seq(34, 61, 3), 3, ] = "AMT"
mat[seq(3, 30, 3), 20, ] = age ; mat[seq(32, 61, 3), 20, ] = age
mat[seq(2, 30, 3), 20, ] = "CNT" ; mat[seq(33, 61, 3), 20, ] = "CNT"
mat[seq(3, 30, 3), 20, ] = "AMT" ; mat[seq(34, 61, 3), 20, ] = "AMT"
mat[-c(seq(1, 30, 3), seq(32, 61, 3)), 3, ] = ""
mat[-c(seq(1, 30, 3), seq(32, 61, 3)), 20, ] = ""




# cal

for(y in c(110, 350)){
  for(o in c(1:13)){
    for(i in gender){
      for(u in 1:length(age)){
        for(p in c(1:(card$MCT_CAT_CD %>% unique() %>% length()))){
          
          if(y == 110){
              
              join = left_join(card %>% filter(GU_CD == y, 
                                      DONG_CD == ifelse(y == 110, dongcd[o], dongcd[o + 13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>% 
                                 group_by(days) %>% 
                                 summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT)), 
                               weather_day %>% 
                filter(dong == ifelse(y == 110, dong[o], dong[o+13])), 
                by = "days")
              join1 = join %>% filter(pm10 >= 80)
              join2 = join %>% filter(pm10 < 80)
              
              if(dim(join1)[1] < 2 | dim(join2)[1] < 2)
                next
              
              a = var.test(join1$AMT, join2$AMT)
              if(is.na(a$p.value) == T)
                next
              
              b = t.test(join1$AMT, join2$AMT, alt = "greater", var.equal =
                           ifelse(a$p.value < 0.05, F, T))
              
              sum1 = join1 %>% summarise(CNT = sum(CNT, na.rm = T) / dim(join1)[1],
                                         AMT = sum(AMT, na.rm = T) / dim(join1)[1])
              sum2 = join2 %>% summarise(CNT = sum(CNT, na.rm = T) / dim(join2)[1],
                                         AMT = sum(AMT, na.rm= T) / dim(join2)[1])
              
              c = ifelse(b$p.value >= 0.05, "같음", "다름")
              d = sum1 / sum2
              
              if(i == "M"){
                mat[c(3 * u - 2), c(o + 3), p] = c
                mat[c(3 * u - 1), c(o + 3), p] = round(d$CNT,3)
                mat[c(3 * u), c(o + 3), p] = round(d$AMT, 3)
              } else {
                mat[c(3 * u + 29), c(o + 3), p] = c
                mat[c(3 * u + 30), c(o + 3), p] = round(d$CNT, 3)
                mat[c(3 * u + 31), c(o + 3), p] = round(d$AMT, 3)
              }
            } 
          else {
            
              join = left_join(card %>% filter(GU_CD == y, 
                                      DONG_CD == ifelse(y == 110, dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>% 
                group_by(days) %>% summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT)), 
                               weather_day %>% filter(region == ifelse(y == 110, dong[o], dong[o+13])), 
                               by = "days")
              join1 = join %>% filter(pm10 >= 80)
              join2 = join %>% filter(pm10 < 80)
              
              if(dim(join1)[1] < 2 | dim(join2)[1] < 2)
                next
              
              a = var.test(join1$AMT, join2$AMT)
              if(is.na(a$p.value) == T)
                next
              b = t.test(join1$AMT, join2$AMT, alt="greater", var.equal =
                           ifelse(a$p.value<0.05, F, T))
              
              sum1 = join1 %>% summarise(CNT = sum(CNT, na.rm = T)/dim(join1)[1],
                                         AMT = sum(AMT, na.rm = T)/dim(join1)[1])
              sum2 = join2 %>% summarise(CNT = sum(CNT, na.rm = T)/dim(join2)[1],
                                         AMT = sum(AMT, na.rm= T)/dim(join2)[1])
              
              c = ifelse(b$p.value >= 0.05, "같음", "다름")
              d = sum1 / sum2
              
              if(i == "M"){
                mat21[c(3 * u - 2), c(o + 20), p] = c
                mat21[c(3 * u - 1), c(o + 20), p] = round(d$CNT,3)
                mat21[c(3 * u), c(o + 20), p] = round(d$AMT, 3)
              } else {
                mat21[c(3 * u + 29), c(o + 20), p] = c
                mat21[c(3 * u + 30), c(o + 20), p] = round(d$CNT, 3)
                mat21[c(3 * u + 31), c(o + 20), p] = round(d$AMT, 3)
              }
          }
        }
      }        
    }
  }           
}
