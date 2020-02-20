require("data.table")
require("readxl")
require("bit64")
require("dplyr")
require("tidyr")
require("stringr")
require("lubridate")




weather = bind_rows(list.files("D:/bigcon/환경기상데이터/종로구", pattern = "*.csv", full.names = T) %>% 
                     purrr::map_df(~fread(., fill = T, select = c("tm", "serial", "pm10", "pm25", "temp", "humi"))),
                    list.files("D:/bigcon/환경기상데이터/노원구", pattern = "*.csv", full.names = T) %>% 
                     purrr::map_df(~fread(., fill = T, select = c("tm", "serial", "pm10", "pm25", "temp", "humi")))) %>% 
  # make time var
  mutate(days = ymd(str_sub(tm, 1, 8)),
         hours = ymd_h(str_sub(tm, 1, 10))) %>% 
  # remove na
  na_if(-999) %>% na_if(-9999) %>% 
  # attach area
  left_join(bind_rows(read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(2, 5) %>% rename(serial = "스테이션...2", dong = "행정동...5") %>% 
                        mutate(dong = str_replace(dong, " ", ""),
                               gu = "종로구"),
                      read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(8, 11) %>% rename(serial = "스테이션...8", dong = "행정동...11") %>% 
                        mutate(gu = "노원구")), 
            by = "serial")




# by day 

weather %>% select(-hours) %>% group_by(days, dong) %>% summarise(pm10 = mean(pm10, na.rm = T),
                                                                  pm25 = mean(pm10, na.rm = T),
                                                                  temp = mean(temp, na.rm = T),
                                                                  humid = mean(humi, na.rm = T)) %>% 
  # fill day in dong
  group_by(dong) %>% complete(days = seq.Date(min(days), max(days), by = "days")) %>% 
  # add gu
  left_join(bind_rows(read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(5) %>% rename(dong = "행정동...5") %>% 
                        mutate(dong = str_replace(dong, " ", ""),
                               gu = "종로구") %>% 
                        unique(),
                      read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(11) %>% rename(dong = "행정동...11") %>% 
                        mutate(gu = "노원구") %>% 
                        unique()), 
            by = "dong") %>%
  # fill na with gu average
  group_by(days, gu) %>% mutate(pm10 = replace_na(pm10, mean(pm10, na.rm = T)),
                                pm25 = replace_na(pm25, mean(pm25, na.rm = T)),
                                temp = replace_na(temp, mean(temp, na.rm = T)),
                                humid = replace_na(humid, mean(humid, na.rm = T))) %>% 
  select(days, pm10, pm25, temp, humid, dong, gu) -> 
  # save
  weather_day


# by hour

weather %>% select(-days) %>% group_by(hours, dong) %>% summarise(pm10 = mean(pm10, na.rm = T),
                                                                  pm25 = mean(pm10, na.rm = T),
                                                                  temp = mean(temp, na.rm = T),
                                                                  humid = mean(humi, na.rm = T)) %>% 
  # fill day in dong
  group_by(dong) %>% complete(hours = seq(min(weather$hours), max(weather$hours), by = "hour")) %>% 
  # add gu
  left_join(bind_rows(read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(5) %>% rename(dong = "행정동...5") %>% 
                        mutate(dong = str_replace(dong, " ", ""),
                               gu = "종로구") %>% 
                        unique(),
                      read_excel("D:/bigcon/환경기상데이터/04_Innovation 분야_환경기상데이터(케이웨더)_데이터정의서(행정동추가).xlsx", sheet = "경진대회용 후보 지점") %>% 
                        select(11) %>% rename(dong = "행정동...11") %>% 
                        mutate(gu = "노원구") %>% 
                        unique()), 
            by = "dong") %>%
  # fill na with gu average
  group_by(hours, gu) %>% mutate(pm10 = replace_na(pm10, mean(pm10, na.rm = T)),
                                 pm25 = replace_na(pm25, mean(pm25, na.rm = T)),
                                 temp = replace_na(temp, mean(temp, na.rm = T)),
                                 humid = replace_na(humid, mean(humid, na.rm = T))) %>% 
  # arrange var
  select(hours, pm10, pm25, temp, humid, dong, gu) -> 
  # save
  weather_hour