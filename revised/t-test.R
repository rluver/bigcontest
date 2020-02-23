require("data.table")
require("dplyr")
require("ztable")




# data load

source("revised/preprocessing_weather.R", encoding = "UTF-8")
card = fread("D:/bigcon/카드메출데이터/CARD_SPENDING_190809.txt", header = T) %>% 
  rename(days = STD_DD)




# var setting

"%!in%" = Negate("%in%")
goodscode = card$MCT_CAT_CD %>% unique()
dongcd_110 = c(600, 580, 550, 530, 710, 640, 615, 630, 670, 690, 515, 560, 650)
dongcd_350 = c(595, 600, 630, 640, 665, 670, 695, 720, 560, 580, 625, 619, 611)
dongcd = c(dongcd_110, dongcd_350)


jongro = (weather_day %>% filter(gu == "종로구", dong %!in% c("명동", "천연동")))$dong %>% unique()
nowon = (weather_day %>% filter(gu == "노원구"))$dong %>% unique()
dong = weather_day$r
gender = c("M", "F")
age =  seq(20, 65, by = 5)


# 매트릭스 생성
mat = array(matrix("NA", nc = 33, nr = 61),
            dim = c(61, 33, 1), 
                    dimnames = list(rep(" ", 61), 
                                    c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon),
                                    card$MCT_CAT_CD %>% unique() %>% length())
            )


colnames(mat) = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)

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






for(y in c(110, 350)){
  for(o in c(1:13)){
    for(i in gender){
      for(u in 1:length(age)){
        for(p in goodscode){
          if(y == 110){
            if(p == 21){
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat21[c(3 * u - 2), c(o + 3)] = c
                mat21[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat21[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat21[c(3 * u + 29), c(o + 3)] = c
                mat21[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat21[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            } else if(p == 22)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% summarise(CNT = sum(USE_CNT), 
                                                                                                                                                                       AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat22[c(3 * u - 2), c(o + 3)] = c
                mat22[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat22[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat22[c(3 * u + 29), c(o + 3)] = c
                mat22[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat22[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            } else if(p == 35)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat35[c(3 * u - 2), c(o + 3)] = c
                mat35[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat35[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat35[c(3 * u + 29), c(o + 3)] = c
                mat35[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat35[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            } else if(p == 40)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat40[c(3 * u - 2), c(o + 3)] = c
                mat40[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat40[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat40[c(3 * u + 29), c(o + 3)] = c
                mat40[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat40[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            }
            else if(p == 70)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat70[c(3 * u - 2), c(o + 3)] = c
                mat70[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat70[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat70[c(3 * u + 29), c(o + 3)] = c
                mat70[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat70[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            } else if(p == 71)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat71[c(3 * u - 2), c(o + 3)] = c
                mat71[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat71[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat71[c(3 * u + 29), c(o + 3)] = c
                mat71[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat71[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            } else
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat80[c(3 * u - 2), c(o + 3)] = c
                mat80[c(3 * u - 1), c(o + 3)] = round(d$CNT,3)
                mat80[c(3 * u), c(o + 3)] = round(d$AMT, 3)
              } else {
                mat80[c(3 * u + 29), c(o + 3)] = c
                mat80[c(3 * u + 30), c(o + 3)] = round(d$CNT, 3)
                mat80[c(3 * u + 31), c(o + 3)] = round(d$AMT, 3)
              }
            }
          } else {
            if(p == 21){
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              join = left_join(card1, weather1, by = "days")
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
                mat21[c(3 * u - 2), c(o + 20)] = c
                mat21[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat21[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat21[c(3 * u + 29), c(o + 20)] = c
                mat21[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat21[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else if(p == 22)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% summarise(CNT = sum(USE_CNT), 
                                                                                                                                                                       AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat22[c(3 * u - 2), c(o + 20)] = c
                mat22[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat22[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat22[c(3 * u + 29), c(o + 20)] = c
                mat22[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat22[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else if(p == 35)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat35[c(3 * u - 2), c(o + 20)] = c
                mat35[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat35[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat35[c(3 * u + 29), c(o + 20)] = c
                mat35[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat35[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else if(p == 40)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat40[c(3 * u - 2), c(o + 20)] = c
                mat40[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat40[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat40[c(3 * u + 29), c(o + 20)] = c
                mat40[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat40[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else if(p == 70)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat70[c(3 * u - 2), c(o + 20)] = c
                mat70[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat70[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat70[c(3 * u + 29), c(o + 20)] = c
                mat70[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat70[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else if(p == 71)
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat71[c(3 * u - 2), c(o + 20)] = c
                mat71[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat71[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat71[c(3 * u + 29), c(o + 20)] = c
                mat71[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat71[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            } else
            {
              card1 = card %>% filter(GU_CD == y, DONG_CD == ifelse(y == 110,                                             dongcd[o], dongcd[o+13]),
                                      AGE_CD == age[u], SEX_CD == i, MCT_CAT_CD == p) %>%                                                 group_by(days) %>% 
                summarise(CNT = sum(USE_CNT), AMT = sum(USE_AMT))
              weather1 = weather_day %>% filter(region == ifelse(y == 110,                                                   dong[o], dong[o+13])) 
              
              join = left_join(card1, weather1, by = "days")
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
                mat80[c(3 * u - 2), c(o + 20)] = c
                mat80[c(3 * u - 1), c(o + 20)] = round(d$CNT,3)
                mat80[c(3 * u), c(o + 20)] = round(d$AMT, 3)
              } else {
                mat80[c(3 * u + 29), c(o + 20)] = c
                mat80[c(3 * u + 30), c(o + 20)] = round(d$CNT, 3)
                mat80[c(3 * u + 31), c(o + 20)] = round(d$AMT, 3)
              }
            }
          }
        }        
      }
    }           
  }
}