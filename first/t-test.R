require("data.table")
require("dplyr")
require("ztable")




# 코드 입력 및 데이터 로드
goodscode = c(21, 22, 35, 40, 70, 71, 80)
dongcd_110 = c(600, 580, 550, 530, 710, 640, 615, 630, 670, 690, 515, 560, 650)
dongcd_350 = c(595, 600, 630, 640, 665, 670, 695, 720, 560, 580, 625, 619, 611)
dongcd = c(dongcd_110, dongcd_350)

card = fread("D:/공모전_시작이반이다/raw/카드매출데이터/CARD_SPENDING_190809.txt", header = T) %>% rename(days = STD_DD)
a = fread("D:공모전_시작이반이다/weather_종로.csv", header = T)
b = fread("D:공모전_시작이반이다/weather_노원.csv", header = T)
weather_day = rbind(a,b)
jongro = unique(a$region)
nowon = unique(b$region)
dong = unique(weather_day$region)
gender = c("M", "F")
age =  seq(20, 65, by = 5)


# 매출코드 21 매트릭스 생성
mat21 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat21) = name
mat21[1, 1] = "종로구" ; mat21[1, 18] = "노원구"
mat21[, 17] = ""
mat21[2:61, 1] = "" ; mat21[2:61, 18] = "" ; mat21[31, ] = ""
mat21[1, 2] = "남자" ; mat21[1, 19] = "남자"
mat21[32,2] = "여자" ; mat21[32, 19] = "여자"
mat21[2:30,2] = "" ; mat21[2:30, 19] = ""
mat21[33:61,2] = "" ; mat21[33:61, 19] = ""
mat21[seq(1,30,3),3] = age ; mat21[seq(32, 61, 3), 3] = age
mat21[seq(2,30,3),3] = "CNT" ; mat21[seq(33, 61, 3), 3] = "CNT"
mat21[seq(3,30,3),3] = "AMT" ; mat21[seq(34, 61, 3), 3] = "AMT"
mat21[seq(3,30,3),20] = age ; mat21[seq(32, 61, 3), 20] = age
mat21[seq(2,30,3),20] = "CNT" ; mat21[seq(33, 61, 3), 20] = "CNT"
mat21[seq(3,30,3),20] = "AMT" ; mat21[seq(34, 61, 3), 20] = "AMT"
mat21[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat21[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 22 매트릭스 생성
mat22 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat22) = name
mat22[1, 1] = "종로구" ; mat22[1, 18] = "노원구"
mat22[, 17] = ""
mat22[2:61, 1] = "" ; mat22[2:61, 18] = "" ; mat22[31, ] = ""
mat22[1, 2] = "남자" ; mat22[1, 19] = "남자"
mat22[32,2] = "여자" ; mat22[32, 19] = "여자"
mat22[2:30,2] = "" ; mat22[2:30, 19] = ""
mat22[33:61,2] = "" ; mat22[33:61, 19] = ""
mat22[seq(1,30,3),3] = age ; mat22[seq(32, 61, 3), 3] = age
mat22[seq(2,30,3),3] = "CNT" ; mat22[seq(33, 61, 3), 3] = "CNT"
mat22[seq(3,30,3),3] = "AMT" ; mat22[seq(34, 61, 3), 3] = "AMT"
mat22[seq(3,30,3),20] = age ; mat22[seq(32, 61, 3), 20] = age
mat22[seq(2,30,3),20] = "CNT" ; mat22[seq(33, 61, 3), 20] = "CNT"
mat22[seq(3,30,3),20] = "AMT" ; mat22[seq(34, 61, 3), 20] = "AMT"
mat22[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat22[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 35 매트릭스 생성
mat35 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat35) = name
mat35[1, 1] = "종로구" ; mat35[1, 18] = "노원구"
mat35[, 17] = ""
mat35[2:61, 1] = "" ; mat35[2:61, 18] = "" ; mat35[31, ] = ""
mat35[1, 2] = "남자" ; mat35[1, 19] = "남자"
mat35[32,2] = "여자" ; mat35[32, 19] = "여자"
mat35[2:30,2] = "" ; mat35[2:30, 19] = ""
mat35[33:61,2] = "" ; mat35[33:61, 19] = ""
mat35[seq(1,30,3),3] = age ; mat35[seq(32, 61, 3), 3] = age
mat35[seq(2,30,3),3] = "CNT" ; mat35[seq(33, 61, 3), 3] = "CNT"
mat35[seq(3,30,3),3] = "AMT" ; mat35[seq(34, 61, 3), 3] = "AMT"
mat35[seq(3,30,3),20] = age ; mat35[seq(32, 61, 3), 20] = age
mat35[seq(2,30,3),20] = "CNT" ; mat35[seq(33, 61, 3), 20] = "CNT"
mat35[seq(3,30,3),20] = "AMT" ; mat35[seq(34, 61, 3), 20] = "AMT"
mat35[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat35[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 40 매트릭스 생성
mat40 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat40) = name
mat40[1, 1] = "종로구" ; mat40[1, 18] = "노원구"
mat40[, 17] = ""
mat40[2:61, 1] = "" ; mat40[2:61, 18] = "" ; mat40[31, ] = ""
mat40[1, 2] = "남자" ; mat40[1, 19] = "남자"
mat40[32,2] = "여자" ; mat40[32, 19] = "여자"
mat40[2:30,2] = "" ; mat40[2:30, 19] = ""
mat40[33:61,2] = "" ; mat40[33:61, 19] = ""
mat40[seq(1,30,3),3] = age ; mat40[seq(32, 61, 3), 3] = age
mat40[seq(2,30,3),3] = "CNT" ; mat40[seq(33, 61, 3), 3] = "CNT"
mat40[seq(3,30,3),3] = "AMT" ; mat40[seq(34, 61, 3), 3] = "AMT"
mat40[seq(3,30,3),20] = age ; mat40[seq(32, 61, 3), 20] = age
mat40[seq(2,30,3),20] = "CNT" ; mat40[seq(33, 61, 3), 20] = "CNT"
mat40[seq(3,30,3),20] = "AMT" ; mat40[seq(34, 61, 3), 20] = "AMT"
mat40[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat40[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 70 매트릭스 생성
mat70 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat70) = name
mat70[1, 1] = "종로구" ; mat70[1, 18] = "노원구"
mat70[, 17] = ""
mat70[2:61, 1] = "" ; mat70[2:61, 18] = "" ; mat70[31, ] = ""
mat70[1, 2] = "남자" ; mat70[1, 19] = "남자"
mat70[32,2] = "여자" ; mat70[32, 19] = "여자"
mat70[2:30,2] = "" ; mat70[2:30, 19] = ""
mat70[33:61,2] = "" ; mat70[33:61, 19] = ""
mat70[seq(1,30,3),3] = age ; mat70[seq(32, 61, 3), 3] = age
mat70[seq(2,30,3),3] = "CNT" ; mat70[seq(33, 61, 3), 3] = "CNT"
mat70[seq(3,30,3),3] = "AMT" ; mat70[seq(34, 61, 3), 3] = "AMT"
mat70[seq(3,30,3),20] = age ; mat70[seq(32, 61, 3), 20] = age
mat70[seq(2,30,3),20] = "CNT" ; mat70[seq(33, 61, 3), 20] = "CNT"
mat70[seq(3,30,3),20] = "AMT" ; mat70[seq(34, 61, 3), 20] = "AMT"
mat70[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat70[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 71 매트릭스 생성
mat71 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat71) = name
mat71[1, 1] = "종로구" ; mat71[1, 18] = "노원구"
mat71[, 17] = ""
mat71[2:61, 1] = "" ; mat71[2:61, 18] = "" ; mat71[31, ] = ""
mat71[1, 2] = "남자" ; mat71[1, 19] = "남자"
mat71[32,2] = "여자" ; mat71[32, 19] = "여자"
mat71[2:30,2] = "" ; mat71[2:30, 19] = ""
mat71[33:61,2] = "" ; mat71[33:61, 19] = ""
mat71[seq(1,30,3),3] = age ; mat71[seq(32, 61, 3), 3] = age
mat71[seq(2,30,3),3] = "CNT" ; mat71[seq(33, 61, 3), 3] = "CNT"
mat71[seq(3,30,3),3] = "AMT" ; mat71[seq(34, 61, 3), 3] = "AMT"
mat71[seq(3,30,3),20] = age ; mat71[seq(32, 61, 3), 20] = age
mat71[seq(2,30,3),20] = "CNT" ; mat71[seq(33, 61, 3), 20] = "CNT"
mat71[seq(3,30,3),20] = "AMT" ; mat71[seq(34, 61, 3), 20] = "AMT"
mat71[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat71[-c(seq(1,30,3), seq(32,61,3)), 20] = ""


# 매출코드 80 매트릭스 생성
mat80 = matrix("NA", nc = 33, nr = 61)
name = c("지역", "성별", "연령", jongro, " ", "지역", "성별", "연령", nowon)
colnames(mat80) = name
mat80[1, 1] = "종로구" ; mat80[1, 18] = "노원구"
mat80[, 17] = ""
mat80[2:61, 1] = "" ; mat80[2:61, 18] = "" ; mat80[31, ] = ""
mat80[1, 2] = "남자" ; mat80[1, 19] = "남자"
mat80[32,2] = "여자" ; mat80[32, 19] = "여자"
mat80[2:30,2] = "" ; mat80[2:30, 19] = ""
mat80[33:61,2] = "" ; mat80[33:61, 19] = ""
mat80[seq(1,30,3),3] = age ; mat80[seq(32, 61, 3), 3] = age
mat80[seq(2,30,3),3] = "CNT" ; mat80[seq(33, 61, 3), 3] = "CNT"
mat80[seq(3,30,3),3] = "AMT" ; mat80[seq(34, 61, 3), 3] = "AMT"
mat80[seq(3,30,3),20] = age ; mat80[seq(32, 61, 3), 20] = age
mat80[seq(2,30,3),20] = "CNT" ; mat80[seq(33, 61, 3), 20] = "CNT"
mat80[seq(3,30,3),20] = "AMT" ; mat80[seq(34, 61, 3), 20] = "AMT"
mat80[-c(seq(1,30,3), seq(32,61,3)), 3] = ""
mat80[-c(seq(1,30,3), seq(32,61,3)), 20] = ""



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