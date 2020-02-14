require("data.table")
require("bit64")
require("dplyr")
require("tsDyn")
require("vars")




# 데이로 로드 및 코드 입력
dongcd_110 = c(600, 580, 550, 530, 710, 640, 615, 630, 670, 690, 515, 560, 650)
dongcd_350 = c(595, 600, 630, 640, 665, 670, 695, 720, 560, 580, 625, 619, 611)
card = fread("D:/공모전_시작이반이다/raw/카드매출데이터/CARD_SPENDING_190809.txt",
             header = T) %>% rename(days = STD_DD)
code = sort(unique(card$MCT_CAT_CD))
weather_day = rbind(fread("D:/공모전_시작이반이다/weather_종로.csv", header = T),
                    fread("D:/공모전_시작이반이다/weather_노원.csv", header = T))
weather_time = fread("D:/공모전_시작이반이다/weather_time.csv", header = T) %>% 
  mutate(days = as.numeric(paste(substr(days, 1, 4), substr(days, 6, 7), 
                                 substr(days, 9, 10), substr(days, 12, 13), sep = "")))
jongro = unique(weather_day$region)[1:13]
nowon = unique(weather_day$region)[14:26]
flowtime = fread("D:/공모전_시작이반이다/flowtime.csv", header = T, encoding = "UTF-8")
flowage = fread("D:/공모전_시작이반이다/flowage.csv", header = T, encoding = "UTF-8")
sa = unique(flowage$sexageflow)[c(-1:-4, -15:-19, -30)]
word = fread("D:/공모전_시작이반이다/wordcount.csv", header = T, 
             col.names = c("days", "word"))


# sample
for(w in code[1]){
  for (i in 1){
    for(j in sa[13]){
      if(j == "MAN_FLOW_POP_CNT_2024"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "MAN_FLOW_POP_CNT_6569"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "WMAN_FLOW_POP_CNT_2024"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "WMAN_FLOW_POP_CNT_6569"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
          group_by(days) %>% summarise(number = sum(number))
      }
      else{
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow == j) %>% 
          group_by(days) %>% summarise(number = sum(number))
      }
      
      weather1 = weather_day %>% filter(region == jongro[i]) %>% dplyr::select(1, 2, 5)
      card1 = card %>% filter(GU_CD == 110, DONG_CD == dongcd_110[i], SEX_CD ==                                                     ifelse(substr(j, 1, 1) == "W", "F", substr(j, 1, 1)), 
                              AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
                                               substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
                                                                                                                                                               USE_AMT = sum(USE_AMT))
      
      join = left_join(card1, weather1, by = "days") %>% 
        left_join(flow1, by = "days") %>% 
        left_join(word, by = "days") %>% 
        dplyr::select(-1) %>% dplyr::select(3:6, 1, 2) %>% 
        mutate("CNT/number" = USE_CNT/number, 
               "AMT/number" = USE_AMT/number, 
               "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-5)
      
      join1 = join %>% dplyr::select(1:4, 5) %>% filter(!is.na(USE_AMT)) %>% rename(AMT = USE_AMT)
      join2 = join %>% dplyr::select(1:4, 6) %>% filter(!is.na(`CNT/number`))
      join3 = join %>% dplyr::select(1:4, 7) %>% filter(!is.na(`AMT/number`))
      join4 = join %>% dplyr::select(1:4, 8) %>% filter(!is.na(`AMT/CNT`))
      
      vs1 = VARselect(join1, lag.max = 10, type = "const")
      vs2 = VARselect(join2, lag.max = 10, type = "const")
      vs3 = VARselect(join3, lag.max = 10, type = "const")
      vs4 = VARselect(join4, lag.max = 10, type = "const")
      
      cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      
      test1 = cbind(cj1@teststat, cj1@cval[,2])
      test2 = cbind(cj2@teststat, cj2@cval[,2])
      test3 = cbind(cj3@teststat, cj3@cval[,2])
      test4 = cbind(cj4@teststat, cj4@cval[,2])
      
      vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
                   r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
                   r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
                   r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
                   r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      
      
      plot(irf(vecm1, response = "AMT"), names = "pm10")
      plot(irf(vecm1, response = "AMT"), names = "temp")
      plot(irf(vecm1, response = "AMT"), names = "number")
      plot(irf(vecm1, response = "AMT"), names = "word")
      plot(irf(vecm1, response = "word"), names = "temp")
      plot(irf(vecm1, response = "number"), names = "word")
      plot(irf(vecm1, response = "word"), names = "number")
      
      plot(irf(vecm2, response = "CNT/number"), names = "pm10")
      plot(irf(vecm2, response = "CNT/number"), names = "temp")
      plot(irf(vecm2, response = "CNT/number"), names = "number")
      plot(irf(vecm2, response = "CNT/number"), names = "word")
      plot(irf(vecm2, response = "word"), names = "temp")
      plot(irf(vecm2, response = "number"), names = "word")
      plot(irf(vecm2, response = "word"), names = "number")
      
      plot(irf(vecm3, response = "AMT/number"), names = "pm10")
      plot(irf(vecm3, response = "AMT/number"), names = "temp")
      plot(irf(vecm3, response = "AMT/number"), names = "number")
      plot(irf(vecm3, response = "AMT/number"), names = "word")
      plot(irf(vecm3, response = "word"), names = "temp")
      plot(irf(vecm3, response = "number"), names = "word")
      plot(irf(vecm3, response = "word"), names = "number")
      
      plot(irf(vecm4, response = "AMT/CNT"), names = "pm10")
      plot(irf(vecm4, response = "AMT/CNT"), names = "temp")
      plot(irf(vecm4, response = "AMT/CNT"), names = "number")
      plot(irf(vecm4, response = "AMT/CNT"), names = "word")
      plot(irf(vecm4, response = "word"), names = "temp")
      plot(irf(vecm4, response = "number"), names = "word")      
      plot(irf(vecm4, response = "word"), names = "number")
    }
  }
}




## 종로구
# for(w in code){
#   for (i in 1:length(dongcd_110)){
#     for(j in sa){
#       try({
#       if(j == "MAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "MAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#       else{
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow == j) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#      
#       weather1 = weather_day %>% filter(region == jongro[i]) %>% dplyr::select(1, 2, 5)
#       card1 = card %>% filter(GU_CD == 110, DONG_CD == dongcd_110[i], SEX_CD ==                                                     ifelse(substr(j, 1, 1) == "W", "F", substr(j, 1, 1)), 
#                               AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
#                                                substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
#                                                            USE_AMT = sum(USE_AMT))
#      
#      join = left_join(card1, weather1, by = "days") %>% 
#             left_join(flow1, by = "days") %>% 
#             left_join(word, by = "days") %>% 
#             dplyr::select(-1) %>% dplyr::select(3:6, 1, 2) %>% 
#             mutate("CNT/number" = USE_CNT/number, 
#                    "AMT/number" = USE_AMT/number, 
#                    "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-5)
#      
#      
#       join1 = join %>% dplyr::select(1:4, 5) %>% filter(!is.na(USE_AMT)) %>% rename(AMT = USE_AMT)
#       join2 = join %>% dplyr::select(1:4, 6) %>% filter(!is.na(`CNT/number`))
#       join3 = join %>% dplyr::select(1:4, 7) %>% filter(!is.na(`AMT/number`))
#       join4 = join %>% dplyr::select(1:4, 8) %>% filter(!is.na(`AMT/CNT`))
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       vs2 = VARselect(join2, lag.max = 10, type = "const")
#       vs3 = VARselect(join3, lag.max = 10, type = "const")
#       vs4 = VARselect(join4, lag.max = 10, type = "const")
#       
#       cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       
#       test1 = cbind(cj1@teststat, cj1@cval[,2])
#       test2 = cbind(cj2@teststat, cj2@cval[,2])
#       test3 = cbind(cj3@teststat, cj3@cval[,2])
#       test4 = cbind(cj4@teststat, cj4@cval[,2])
#       
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
#                    r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
#                    r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
#                    r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
#                    r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       
#       
#       plot(irf(vecm1, response = "AMT"), names = "pm10")
#       plot(irf(vecm1, response = "AMT"), names = "temp")
#       plot(irf(vecm1, response = "AMT"), names = "number")
#       plot(irf(vecm1, response = "AMT"), names = "word")
#       plot(irf(vecm1, response = "word"), names = "temp")
#       plot(irf(vecm1, response = "number"), names = "word")
#       plot(irf(vecm1, response = "word"), names = "number")
#       
#       plot(irf(vecm2, response = "CNT/number"), names = "pm10")
#       plot(irf(vecm2, response = "CNT/number"), names = "temp")
#       plot(irf(vecm2, response = "CNT/number"), names = "number")
#       plot(irf(vecm2, response = "CNT/number"), names = "word")
#       plot(irf(vecm2, response = "word"), names = "temp")
#       plot(irf(vecm2, response = "number"), names = "word")
#       plot(irf(vecm2, response = "word"), names = "number")
#       
#       plot(irf(vecm3, response = "AMT/number"), names = "pm10")
#       plot(irf(vecm3, response = "AMT/number"), names = "temp")
#       plot(irf(vecm3, response = "AMT/number"), names = "number")
#       plot(irf(vecm3, response = "AMT/number"), names = "word")
#       plot(irf(vecm3, response = "word"), names = "temp")
#       plot(irf(vecm3, response = "number"), names = "word")
#       plot(irf(vecm3, response = "word"), names = "number")
#       
#       plot(irf(vecm4, response = "AMT/CNT"), names = "pm10")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "temp")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "number")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "word")
#       plot(irf(vecm4, response = "word"), names = "temp")
#       plot(irf(vecm4, response = "number"), names = "word")      
#       plot(irf(vecm4, response = "word"), names = "number")
#     })
#     }
#   }
# }


## 노원구
# for(w in code){
#   for (i in 1:length(dongcd_350)){
#     for(j in sa){
#       try({
#       if(j == "MAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "MAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#       else{
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow == j) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#      
#       weather1 = weather_day %>% filter(region == nowon[i]) %>% dplyr::select(1, 2, 5)
#       card1 = card %>% filter(GU_CD == 110, DONG_CD == dongcd_110[i], SEX_CD ==                                                     ifelse(substr(j, 1, 1) == "W", "F", substr(j, 1, 1)), 
#                               AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
#                                                substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
#                                                            USE_AMT = sum(USE_AMT))
#
#      join = left_join(card1, weather1, by = "days") %>% 
#             left_join(flow1, by = "days") %>% 
#             left_join(word, by = "days") %>% 
#             dplyr::select(-1) %>% dplyr::select(3:6, 1, 2) %>% 
#             mutate("CNT/number" = USE_CNT/number, 
#                    "AMT/number" = USE_AMT/number, 
#                    "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-5)
#      
#      
#       join1 = join %>% dplyr::select(1:4, 5) %>% filter(!is.na(USE_AMT)) %>% rename(AMT = USE_AMT)
#       join2 = join %>% dplyr::select(1:4, 6) %>% filter(!is.na(`CNT/number`))
#       join3 = join %>% dplyr::select(1:4, 7) %>% filter(!is.na(`AMT/number`))
#       join4 = join %>% dplyr::select(1:4, 8) %>% filter(!is.na(`AMT/CNT`))
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       vs2 = VARselect(join2, lag.max = 10, type = "const")
#       vs3 = VARselect(join3, lag.max = 10, type = "const")
#       vs4 = VARselect(join4, lag.max = 10, type = "const")
#       
#       cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       
#       test1 = cbind(cj1@teststat, cj1@cval[,2])
#       test2 = cbind(cj2@teststat, cj2@cval[,2])
#       test3 = cbind(cj3@teststat, cj3@cval[,2])
#       test4 = cbind(cj4@teststat, cj4@cval[,2])
#       
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
#                    r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
#                    r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
#                    r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
#                    r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       
#       
#       plot(irf(vecm1, response = "AMT"), names = "pm10")
#       plot(irf(vecm1, response = "AMT"), names = "temp")
#       plot(irf(vecm1, response = "AMT"), names = "number")
#       plot(irf(vecm1, response = "AMT"), names = "word")
#       plot(irf(vecm1, response = "word"), names = "temp")
#       plot(irf(vecm1, response = "number"), names = "word")
#       plot(irf(vecm1, response = "word"), names = "number")
#       
#       plot(irf(vecm2, response = "CNT/number"), names = "pm10")
#       plot(irf(vecm2, response = "CNT/number"), names = "temp")
#       plot(irf(vecm2, response = "CNT/number"), names = "number")
#       plot(irf(vecm2, response = "CNT/number"), names = "word")
#       plot(irf(vecm2, response = "word"), names = "temp")
#       plot(irf(vecm2, response = "number"), names = "word")
#       plot(irf(vecm2, response = "word"), names = "number")
#       
#       plot(irf(vecm3, response = "AMT/number"), names = "pm10")
#       plot(irf(vecm3, response = "AMT/number"), names = "temp")
#       plot(irf(vecm3, response = "AMT/number"), names = "number")
#       plot(irf(vecm3, response = "AMT/number"), names = "word")
#       plot(irf(vecm3, response = "word"), names = "temp")
#       plot(irf(vecm3, response = "number"), names = "word")
#       plot(irf(vecm3, response = "word"), names = "number")
#       
#       plot(irf(vecm4, response = "AMT/CNT"), names = "pm10")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "temp")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "number")
#       plot(irf(vecm4, response = "AMT/CNT"), names = "word")
#       plot(irf(vecm4, response = "word"), names = "temp")
#       plot(irf(vecm4, response = "number"), names = "word")      
#       plot(irf(vecm4, response = "word"), names = "number")
#     })
#     }
#   }
# }
Floating Number, Card, Weather
작업량이 많기에 위에 샘플 코드를 작성 실제 사용한 코드는 아래 주석처리 주석 아래 구분선 부분은 그래프 추출을 위해 사용했던 코드

코드 실행 시 연산이 안 되는 부분이 발생 이 경우 코드에서 다음 부분을 실행하게끔 작성

# jongro
# nowon
# dongcd_110
# dongcd_350
# card
# weather_day 
# flowtime, flowage
# code
# sa


# sample
for(w in code[1]){
  for (i in 1){
    for(j in sa[13]){
      if(j == "MAN_FLOW_POP_CNT_2024"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "MAN_FLOW_POP_CNT_6569"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "WMAN_FLOW_POP_CNT_2024"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
          group_by(days) %>% summarise(number = sum(number))
      }
      else if(j == "WMAN_FLOW_POP_CNT_6569"){
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
          group_by(days) %>% summarise(number = sum(number))
      }
      else{
        flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow == j) %>% 
          group_by(days) %>% summarise(number = sum(number))
      }
      
      weather1 = weather_day %>% filter(region == jongro[i]) %>% dplyr::select(1, 2, 5)
      card1 = card %>% filter(GU_CD == 110, DONG_CD == dongcd_110[i], SEX_CD ==                                                     ifelse(substr(j, 1, 1) == "W", "F", substr(j, 1, 1)), 
                              AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
                                               substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
                                                                                                                                                               USE_AMT = sum(USE_AMT))
      
      join = left_join(card1, weather1, by = "days") %>% 
        left_join(flow1, by = "days") %>% dplyr::select(-1) %>%
        dplyr::select(3:5, 1, 2) %>% 
        mutate("CNT/number" = USE_CNT/number, 
               "AMT/number" = USE_AMT/number, 
               "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-4)
      
      
      join1 = join %>% dplyr::select(1,2,3,4) %>% filter(!is.na(USE_AMT)) %>% rename(AMT = USE_AMT)
      join2 = join %>% dplyr::select(1,2,3,5) %>% filter(!is.na(`CNT/number`))
      join3 = join %>% dplyr::select(1,2,3,6) %>% filter(!is.na(`AMT/number`))
      join4 = join %>% dplyr::select(1,2,3,7) %>% filter(!is.na(`AMT/CNT`))
      
      
      vs1 = VARselect(join1, lag.max = 10, type = "const")
      vs2 = VARselect(join2, lag.max = 10, type = "const")
      vs3 = VARselect(join3, lag.max = 10, type = "const")
      vs4 = VARselect(join4, lag.max = 10, type = "const")
      
      
      cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
      
      test1 = cbind(cj1@teststat, cj1@cval[,2])
      test2 = cbind(cj2@teststat, cj2@cval[,2])
      test3 = cbind(cj3@teststat, cj3@cval[,2])
      test4 = cbind(cj4@teststat, cj4@cval[,2])
      
      vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
                   r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
                   r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
                   r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
                   r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
                   include = "const", estim = "ML")
      
      
      plot(irf(vecm1, response = "AMT"), names="pm10")
      plot(irf(vecm1, response = "AMT"), names="temp")
      plot(irf(vecm1, response = "AMT"), names="number")
      
      plot(irf(vecm2, response = "CNT/number"), names="pm10")
      plot(irf(vecm2, response = "CNT/number"), names="temp")
      plot(irf(vecm2, response = "CNT/number"), names="number")
      
      plot(irf(vecm3, response = "AMT/number"), names="pm10")
      plot(irf(vecm3, response = "AMT/number"), names="temp")
      plot(irf(vecm3, response = "AMT/number"), names="number")
      
      plot(irf(vecm4, response = "AMT/CNT"), names="pm10")
      plot(irf(vecm4, response = "AMT/CNT"), names="temp")
      plot(irf(vecm4, response = "AMT/CNT"), names="number")
    }
  }
}





## 종로구
# for(w in code){
#   for (i in 1:length(jongro)){
#     for(j in sa){
#       try({
#       if(j == "MAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "MAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#       else{
#         flow1 = flowage %>% filter(HDONG_NM == jongro[i], sexageflow == j) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#      
#       weather1 = weather_day %>% filter(region == jongro[i]) %>% dplyr::select(1, 2, 5)
#       card1 = card %>% filter(GU_CD == 110, DONG_CD == dongcd_110[i], SEX_CD ==                                                     ifelse(substr(j,1,1)=="W", "F", substr(j,1,1)), 
#                               AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
#                                                substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
#                                                            USE_AMT = sum(USE_AMT))
#      
#       join = left_join(card1, weather1, by = "days") %>% 
#              left_join(flow1, by = "days") %>% dplyr::select(-1) %>%
#              dplyr::select(3:5, 1, 2) %>% 
#              mutate("CNT/number" = USE_CNT/number, 
#                     "AMT/number" = USE_AMT/number, 
#                     "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-4)
#      
#       join1 = join %>% dplyr::select(1,2,3,4) %>% filter(!is.na(USE_AMT)) %>% 
#                        rename(AMT = USE_AMT)
#       join2 = join %>% dplyr::select(1,2,3,5) %>% filter(!is.na(`CNT/number`))
#       join3 = join %>% dplyr::select(1,2,3,6) %>% filter(!is.na(`AMT/number`))
#       join4 = join %>% dplyr::select(1,2,3,7) %>% filter(!is.na(`AMT/CNT`))
#      
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       vs2 = VARselect(join2, lag.max = 10, type = "const")
#       vs3 = VARselect(join3, lag.max = 10, type = "const")
#       vs4 = VARselect(join4, lag.max = 10, type = "const")
#      
#       
#       cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       
#       test1 = cbind(cj1@teststat, cj1@cval[,2])
#       test2 = cbind(cj2@teststat, cj2@cval[,2])
#       test3 = cbind(cj3@teststat, cj3@cval[,2])
#       test4 = cbind(cj4@teststat, cj4@cval[,2])
#       
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
#                    r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
#                    r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
#                    r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
#                    r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       
#      
#       plot(irf(vecm1, response = "AMT"), names="pm10")
#       plot(irf(vecm1, response = "AMT"), names="temp")
#       plot(irf(vecm1, response = "AMT"), names="number")
#      
#       plot(irf(vecm2, response = "CNT/number"), names="pm10")
#       plot(irf(vecm2, response = "CNT/number"), names="temp")
#       plot(irf(vecm2, response = "CNT/number"), names="number")
#       
#       plot(irf(vecm3, response = "AMT/number"), names="pm10")
#       plot(irf(vecm3, response = "AMT/number"), names="temp")
#       plot(irf(vecm3, response = "AMT/number"), names="number")
#       
#       plot(irf(vecm4, response = "AMT/CNT"), names="pm10")
#       plot(irf(vecm4, response = "AMT/CNT"), names="temp")
#       plot(irf(vecm4, response = "AMT/CNT"), names="number")
#     })
#     }
#   }
# }
# 
# 
## 노원구
# for(w in code){
#   for (i in 1:length(nowon)){
#     for(j in sa){
#       try({
#       if(j == "MAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_0004", "MAN_FLOW_POP_CNT_0509",                                                   "MAN_FLOW_POP_CNT_1014", "MAN_FLOW_POP_CNT_1519",                                                   "MAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "MAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("MAN_FLOW_POP_CNT_6569", "MAN_FLOW_POP_CNT_70U")) %>%                          group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_2024"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                                             c("WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",                                                 "WMAN_FLOW_POP_CNT_1014", "WMAN_FLOW_POP_CNT_1519",                                                 "WMAN_FLOW_POP_CNT_2024")) %>%
#                 group_by(days) %>% summarise(number = sum(number))
#       }
#       else if(j == "WMAN_FLOW_POP_CNT_6569"){
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow %in%                                          c("WMAN_FLOW_POP_CNT_6569", "WMAN_FLOW_POP_CNT_70U")) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#       else{
#         flow1 = flowage %>% filter(HDONG_NM == nowon[i], sexageflow == j) %>% 
#             group_by(days) %>% summarise(number = sum(number))
#       }
#      
#       weather1 = weather_day %>% filter(region == nowon[i]) %>% dplyr::select(1, 2, 5)
#       card1 = card %>% filter(GU_CD == 350, DONG_CD == dongcd_350[i], SEX_CD ==                                                     ifelse(substr(j,1,1)=="W", "F", substr(j,1,1)), 
#                               AGE_CD == ifelse(substr(j, 1, 1) == "M", substr(j, 18, 19), 
#                                                substr(j, 19, 20)), MCT_CAT_CD == w) %>%                                           group_by(days) %>% summarise(USE_CNT = sum(USE_CNT), 
#                                                            USE_AMT = sum(USE_AMT))
#      
#       join = left_join(card1, weather1, by = "days") %>% 
#              left_join(flow1, by = "days") %>% dplyr::select(-1) %>%
#              dplyr::select(3:5, 1, 2) %>% 
#              mutate("CNT/number" = USE_CNT/number, 
#                     "AMT/number" = USE_AMT/number, 
#                     "AMT/CNT" = USE_AMT/USE_CNT) %>% dplyr::select(-4)
#      
#       join1 = join %>% dplyr::select(1,2,3,4) %>% filter(!is.na(USE_AMT)) %>% 
#                                                   rename(AMT = USE_AMT)
#       join2 = join %>% dplyr::select(1,2,3,5) %>% filter(!is.na(`CNT/number`))
#       join3 = join %>% dplyr::select(1,2,3,6) %>% filter(!is.na(`AMT/number`))
#       join4 = join %>% dplyr::select(1,2,3,7) %>% filter(!is.na(`AMT/CNT`))
#      
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       vs2 = VARselect(join2, lag.max = 10, type = "const")
#       vs3 = VARselect(join3, lag.max = 10, type = "const")
#       vs4 = VARselect(join4, lag.max = 10, type = "const")
#      
#       
#       cj1 = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj2 = summary(ca.jo(join2, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj3 = summary(ca.jo(join3, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       cj4 = summary(ca.jo(join4, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       
#       test1 = cbind(cj1@teststat, cj1@cval[,2])
#       test2 = cbind(cj2@teststat, cj2@cval[,2])
#       test3 = cbind(cj3@teststat, cj3@cval[,2])
#       test4 = cbind(cj4@teststat, cj4@cval[,2])
#       
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),
#                    r = ifelse(test1[1, 1] > test1[1, 2], 3, ifelse(test1[2, 1] > test1[2, 2], 2,                                  ifelse(test1[3, 1] > test1[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm2 = VECM(join2, lag = ifelse(vs2$selection[3] > 3, 1, vs2$selection[3]),
#                    r = ifelse(test2[1, 1] > test2[1, 2], 3, ifelse(test2[2, 1] > test2[2, 2], 2,                                  ifelse(test2[3, 1] > test2[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm3 = VECM(join3, lag = ifelse(vs3$selection[3] > 3, 1, vs3$selection[3]),
#                    r = ifelse(test3[1, 1] > test3[1, 2], 3, ifelse(test3[2, 1] > test3[2, 2], 2,                                  ifelse(test3[3, 1] > test3[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       vecm4 = VECM(join4, lag = ifelse(vs4$selection[3] > 3, 1, vs4$selection[3]),
#                    r = ifelse(test4[1, 1] > test4[1, 2], 3, ifelse(test4[2, 1] > test4[2, 2], 2,                                  ifelse(test4[3, 1] > test4[3, 2], 1, 0))),
#                    include = "const", estim = "ML")
#       
#       plot(irf(vecm1, response = "AMT"), names="pm10")
#       plot(irf(vecm1, response = "AMT"), names="temp")
#       plot(irf(vecm1, response = "AMT"), names="number")
#      
#       plot(irf(vecm2, response = "CNT/number"), names="pm10")
#       plot(irf(vecm2, response = "CNT/number"), names="temp")
#       plot(irf(vecm2, response = "CNT/number"), names="number")
#       
#       plot(irf(vecm3, response = "AMT/number"), names="pm10")
#       plot(irf(vecm3, response = "AMT/number"), names="temp")
#       plot(irf(vecm3, response = "AMT/number"), names="number")
#       
#       plot(irf(vecm4, response = "AMT/CNT"), names="pm10")
#       plot(irf(vecm4, response = "AMT/CNT"), names="temp")
#       plot(irf(vecm4, response = "AMT/CNT"), names="number")
#       dev.off()
#     
#     }
#   }
# }


############################################################################################ 
#
#       path1 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "pm10_AMT.png" , sep = "")
#       path1 = file.path(path1)
#       png(path1)
#       plot(irf(vecm1, response = "AMT"), names="pm10")
#       dev.off()
# 
#       path2 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "temp_AMT.png", sep = "")
#       path2 = file.path(path2)
#       png(path2)
#       plot(irf(vecm1, response = "AMT"), names="temp")
#       dev.off()
# 
#       path3 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "number_AMT.png", sep = "")
#       path3 = file.path(path3)
#       png(path3)
#       plot(irf(vecm1, response = "AMT"), names="number")
#       dev.off()
# 
#       ##
# 
#       path4 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "pm10_CNT／number.png" , sep = "")
#       path4 = file.path(path4)
#       png(path4)
#       plot(irf(vecm2, response = "CNT/number"), names="pm10")
#       dev.off()
# 
#       path5 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "temp_CNT／number.png" , sep = "")
#       path5 = file.path(path5)
#       png(path5)
#       plot(irf(vecm2, response = "CNT/number"), names="temp")
#       dev.off()
# 
#       path6 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "number_CNT／number.png" , sep = "")
#       path6 = file.path(path6)
#       png(path6)
#       plot(irf(vecm2, response = "CNT/number"), names="number")
#       dev.off()
# 
#       ##
# 
#       path7 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "pm10_AMT／number.png" , sep = "")
#       path7 = file.path(path7)
#       png(path7)
#       plot(irf(vecm3, response = "AMT/number"), names="pm10")
#       dev.off()
# 
#       path8 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "temp_AMT／number.png" , sep = "")
#       path8 = file.path(path8)
#       png(path8)
#       plot(irf(vecm3, response = "AMT/number"), names="temp")
#       dev.off()
# 
#       path9 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                     "_", "number_AMT／number.png" , sep = "")
#       path9 = file.path(path9)
#       png(path9)
#       plot(irf(vecm3, response = "AMT/number"), names="number")
#       dev.off()
# 
#       ##
# 
#       path10 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                      "_", "pm10_AMT／CNT.png" , sep = "")
#       path10 = file.path(path10)
#       png(path10)
#       plot(irf(vecm4, response = "AMT/CNT"), names="pm10")
#       dev.off()
# 
#       path11 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                      "_", "temp_AMT／CNT.png" , sep = "")
#       path11 = file.path(path11)
#       png(path11)
#       plot(irf(vecm4, response = "AMT/CNT"), names="temp")
#       dev.off()
# 
#       path12 = paste("d:/충격반응/유동카드환경/성연령별/노원구/", i, "/", w,  "_", j,
#                      "_", "number_AMT／CNT.png" , sep = "")
#       path12 = file.path(path12)
#       png(path12)
#       plot(irf(vecm4, response = "AMT/CNT"), names="number")
#       dev.off()
#     })
#     }
#   }
# }




# jongro
# nowon
# dongcd_110 = c(600, 580, 550, 530, 710, 640, 615, 630, 670, 690, 515, 650)
# dongcd_350 = c(595, 600, 630, 640, 665, 670, 695, 720, 560, 580, 625, 619, 611)
# weather_day 
# flowtime, flowage
# time
# sa

time = substr(unique(flowtime$time), 6, 7)


# 종로구 시간 
for (i in jongro[1]){
  for(j in time[1]){
    flow1 = flowtime %>% filter(substr(time, 6, 7) == j) %>% 
      group_by(days) %>% summarise(number = sum(number))
    
    weather1 = weather_time %>% filter(region == i, substr(days, 9, 10) == j) %>% 
      dplyr::select(1, 2, 5) %>% mutate(days = as.numeric(substr(days, 1, 8)))
    
    join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
    
    vs1 = VARselect(join1, lag.max = 10, type = "const")
    cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
    test = cbind(cj@teststat, cj@cval[, 2])
    vecm1 = VECM(join1, lag = ifelse(vs1$selection[3]>3, 1, vs1$selection[3]),
                 r = ifelse(test[1, 1] > test[1, 2], 2, ifelse(test[2, 1] > test[2, 2], 2, 3)),
                 include = "const", estim = "ML")
    
    plot(irf(vecm1, response = "number"), names = "pm10")
    plot(irf(vecm1, response = "number"), names = "temp")
  }
}




# for (i in jongro){
#     for(j in time){
#     try({
#         flow1 = flowtime %>% filter(substr(time, 6, 7) == j) %>% 
#           group_by(days) %>% summarise(number = sum(number))
#       
#       weather1 = weather_time %>% filter(region == i, substr(days, 9, 10) == j) %>% 
#                  dplyr::select(1, 2, 5) %>% mutate(days = as.numeric(substr(days, 1, 8)))
#       
#       join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
#
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       test = cbind(cj@teststat, cj@cval[, 2])
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
#                    r = ifelse(test[1, 1] > test[1, 2], 2, 
#                               ifelse(test[2, 1] > test[2, 2], 2, 3)),
#                    include = "const", estim = "ML")
#       
#       plot(irf(vecm1, response = "number"), names = "pm10")
#       plot(irf(vecm1, response = "number"), names = "temp")
#      })
#     }
# }


# 종로구 성연령
for (i in jongro[1]){
  for(j in sa[1]){
    flow1 = flowage %>% filter(sexageflow == j, HDONG_NM == i) %>%
      group_by(days) %>% summarise(number = sum(number))
    
    weather1 = weather_day %>% filter(region == i) %>% dplyr::select(1, 2, 5)
    
    join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
    
    vs1 = VARselect(join1, lag.max = 10, type = "const")
    cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
    test = cbind(cj@teststat, cj@cval[, 2])
    vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]),  
                 r = ifelse(test[1, 1] > test[1, 2], 2, ifelse(test[2, 1] > test[2, 2], 2, 3)),
                 include = "const", estim = "ML")
    
    plot(irf(vecm1, response = "number"), names = "pm10")
    plot(irf(vecm1, response = "number"), names = "temp")
  }
}




# for (i in jongro){
#     for(j in sa){
#     try({
#         flow1 = flowage %>% filter(sexageflow == j, HDONG_NM == i) %>% 
#           group_by(days) %>% summarise(number = sum(number))
#       
#         weather1 = weather_day %>% filter(region == i) %>% dplyr::select(1, 2, 5)
#       
#         join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
#      
#         vs1 = VARselect(join1, lag.max = 10, type = "const")
#         cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#         test = cbind(cj@teststat, cj@cval[, 2])
#         vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
#                      r = ifelse(test[1, 1] > test[1, 2], 2, 
#                                  ifelse(test[2, 1] > test[2, 2], 2, 3)),
#                      include = "const", estim = "ML")
#       
#         plot(irf(vecm1, response = "number"), names = "pm10")
#         plot(irf(vecm1, response = "number"), names = "temp")
#     })
#     }
# }




# 노원구 시간
for (i in nowon[1]){
  for(j in time[1]){
    flow1 = flowtime %>% filter(substr(time, 6, 7) == j) %>% 
      group_by(days) %>% summarise(number = sum(number))
    
    weather1 = weather_time %>% filter(region == i, substr(days, 9, 10) == j) %>% 
      dplyr::select(1, 2, 5) %>% mutate(days = as.numeric(substr(days, 1, 8)))
    
    join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
    
    vs1 = VARselect(join1, lag.max = 10, type = "const")
    cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
    test = cbind(cj@teststat, cj@cval[, 2])
    vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
                 r = ifelse(test[1, 1] > test[1, 2], 2, ifelse(test[2, 1] > test[2, 2], 2, 3)),
                 include = "const", estim = "ML")
    
    plot(irf(vecm1, response = "number"), names = "pm10")
    plot(irf(vecm1, response = "number"), names = "temp")
  }
}





# for (i in nowon){
#     for(j in time){
#     try({
#         flow1 = flowtime %>% filter(substr(time, 6, 7) == j) %>% 
#           group_by(days) %>% summarise(number = sum(number))
#       
#       weather1 = weather_time %>% filter(region == i, substr(days, 9, 10) == j) %>% 
#                  dplyr::select(1, 2, 5) %>% mutate(days = as.numeric(substr(days, 1, 8)))
#       
#       join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
#      
#       vs1 = VARselect(join1, lag.max = 10, type = "const")
#       cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#       test = cbind(cj@teststat, cj@cval[, 2])
#       vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
#                    r = ifelse(test[1, 1] > test[1, 2], 2, ifelse(test[2, 1] > test[2, 2], 2, 3)),
#                    include = "const", estim = "ML")
#       
#       plot(irf(vecm1, response = "number"), names = "pm10")
#       plot(irf(vecm1, response = "number"), names = "temp")
#     })
#     }
# }


# 노원구 성연령
for (i in nowon[1]){
  for(j in sa[1]){
    flow1 = flowage %>% filter(sexageflow == j, HDONG_NM == i) %>%
      group_by(days) %>% summarise(number = sum(number))
    
    weather1 = weather_day %>% filter(region == i) %>% dplyr::select(1, 2, 5)
    
    join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
    
    vs1 = VARselect(join1, lag.max = 10, type = "const")
    cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
    test = cbind(cj@teststat, cj@cval[, 2])
    vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
                 r = ifelse(test[1, 1] > test[1, 2], 2, ifelse(test[2, 1] > test[2, 2], 2, 3)),
                 include = "const", estim = "ML")
    
    plot(irf(vecm1, response = "number"), names = "pm10")
    plot(irf(vecm1, response = "number"), names = "temp")
  }
}




# for (i in nowon){
#     for(j in sa){
#     try({
#         flow1 = flowage %>% filter(sexageflow == j, HDONG_NM == i) %>% 
#           group_by(days) %>% summarise(number = sum(number))
#       
#         weather1 = weather_day %>% filter(region == i) %>% dplyr::select(1, 2, 5)
#       
#         join = left_join(weather1, flow1, by = "days") %>% dplyr::select(-1)
#      
#         vs1 = VARselect(join1, lag.max = 10, type = "const")
#         cj = summary(ca.jo(join1, type = "trace", ecdet = "const", K = 2, spec="transitory"))
#         test = cbind(cj@teststat, cj@cval[, 2])
#         vecm1 = VECM(join1, lag = ifelse(vs1$selection[3] > 3, 1, vs1$selection[3]), 
#                      r = ifelse(test[1, 1] > test[1, 2], 2, 
#                                  ifelse(test[2, 1] > test[2, 2], 2, 3)),
#                      include = "const", estim = "ML")
#       
#         plot(irf(vecm1, response = "number"), names = "pm10")
#         plot(irf(vecm1, response = "number"), names = "temp")
#     })
#     }
# }


###############################################################################################
#
#       path1 = paste("d:/충격반응/유동환경/시간별/종로구/", i, "/", j, "_", 
#       "pm10_number.png", sep = "")
#       
#       path1 = file.path(path1)
#       png(path1)
#       plot(irf(vecm1, response = "number"), names = "pm10")
#       dev.off()
#       
#       path2 = paste("d:/충격반응/유동환경/시간별/종로구/", i, "/", j, "_", "temp_number.png" , 
#                     sep = "")
#       path2 = file.path(path2)
#       png(path1)
#       plot(irf(vecm1, response = "number"), names = "temp")
#       dev.off()
#     }
# }
