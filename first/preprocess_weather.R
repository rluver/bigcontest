require("data.table")
require("dplyr")
require("lubridate")




# 데이터 로드
flowage = fread("c:/공모전_시작이반이다/flowage.csv", header = T, encoding = "UTF-8")

# 관측소별 데이터 결합, -999, -9999를 NA 처리 후 pm10, pm25, temp, humid를 열평균 처리
# 시간을 일별, 시간별 가공 후 평균처리

## 종로구

# 가회동
weather_110_600 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611634.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611698.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611722.csv", 
                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_600_time = weather_110_600 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_600_day = weather_110_600 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

# 교남동
weather_110_580 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610252.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_580_time = weather_110_580 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_580_day = weather_110_580 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 부암동
weather_110_550 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611170.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999)%>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_550_time = weather_110_550 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_550_day = weather_110_550 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 사직동
weather_110_530 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V01o1610468.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611172.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>%
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_530_time = weather_110_530 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_530_day = weather_110_530 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 숭인2동 
weather_110_710 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1612106.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_710_time = weather_110_710 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_710_day = weather_110_710 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 이화동
weather_110_640 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611658.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_640_time = weather_110_640 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_640_day = weather_110_640 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 종로1,2,3,4가동
weather_110_615 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610546.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610540.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610542.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610543.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610544.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610545.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1610567.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1612113.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611145.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611750.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611684.csv", 
                  header = T, fill = T), by = "tm") %>%
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611173.csv", 
                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_615_time = weather_110_615 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_615_day = weather_110_615 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 종로5,6가동
weather_110_630 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611639.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_630_time = weather_110_630 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_630_day = weather_110_630 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 창신1동
weather_110_670 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611151.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611220.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_670_time = weather_110_670 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_670_day = weather_110_670 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 창신3동
weather_110_690 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611251.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_690_time = weather_110_690 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_690_day = weather_110_690 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 청운효자동
weather_110_515 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611255.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_515_time = weather_110_515 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_515_day = weather_110_515 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 평창동
weather_110_560 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611258.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_560_time = weather_110_560 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_560_day = weather_110_560 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 혜화동
weather_110_650 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611623.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/종로구/V10O1611645.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_110_650_time = weather_110_650 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_110_650_day = weather_110_650 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


## 노원구

# 공릉1동
weather_350_595 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610629.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610630.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_595_time = weather_350_595 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_595_day = weather_350_595 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 공릉2동
weather_350_600 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610642.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611652.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_600_time = weather_350_600 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_600_day = weather_350_600 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계1동
weather_350_630 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610356.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_630_time = weather_350_630 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_630_day = weather_350_630 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계2동
weather_350_640 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610616.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610200.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610312.csv", 
                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611100.csv", 
                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_640_time = weather_350_640 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_640_day = weather_350_640 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계3,4동
weather_350_665 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611150.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_665_time = weather_350_665 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_665_day = weather_350_665 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계5동
weather_350_670 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610297.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611102.csv", 
                                  header = T, fill = T), by = "tm")  %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_670_time = weather_350_670 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_670_day = weather_350_670 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계6,7동
weather_350_695 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610376.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610351.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611104.csv", 
                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_695_time = weather_350_695 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_695_day = weather_350_695 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 상계10동
weather_350_720 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610293.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_720_time = weather_350_720 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_720_day = weather_350_720 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 월계1동
weather_350_560 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611229.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_560_time = weather_350_560 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_560_day = weather_350_560 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 월계3동
weather_350_580 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610643.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_580_time = weather_350_580 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_580_day = weather_350_580 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 중계2,3동
weather_350_625 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1611097.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_625_time = weather_350_625 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_625_day = weather_350_625 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 중게본동
weather_350_619 = fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1612126.csv", 
                        header = T, fill = T) %>% na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_619_time = weather_350_619 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_619_day = weather_350_619 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 하계1동
weather_350_611 = full_join(fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610610.csv", 
                                  header = T, fill = T),
                            fread("c:/공모전_시작이반이다/raw/환경기상데이터/노원구/V10O1610102.csv", 
                                  header = T, fill = T), by = "tm") %>% 
  na_if(-999) %>% na_if(-9999) %>% 
  mutate(pm10 = rowMeans(dplyr::select(., contains("pm10")), na.rm = T),
         pm25 = rowMeans(dplyr::select(., contains("pm25")), na.rm = T),
         temp = rowMeans(dplyr::select(., contains("temp")), na.rm = T),
         humid = rowMeans(dplyr::select(., contains("humi")), na.rm = T)
  ) %>% 
  dplyr::select(1, pm10, pm25, temp, humid) %>% 
  mutate(timedays = substr(tm, 1, 10),
         days = substr(tm, 1, 8))

weather_350_611_time = weather_350_611 %>% dplyr::select(timedays, 2:5) %>% group_by(timedays) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))

weather_350_611_day = weather_350_611 %>% dplyr::select(days, 2:5) %>% group_by(days) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            humid = mean(humid, na.rm = T))


# 종로구 시간별 결합
weather_110_time = full_join(weather_110_600_time, weather_110_580_time, by = "timedays") %>%                          full_join(weather_110_550_time, by = "timedays") %>% 
  full_join(weather_110_530_time, by = "timedays") %>% 
  full_join(weather_110_710_time, by = "timedays") %>% 
  full_join(weather_110_640_time, by = "timedays") %>% 
  full_join(weather_110_615_time, by = "timedays") %>% 
  full_join(weather_110_630_time, by = "timedays") %>% 
  full_join(weather_110_670_time, by = "timedays") %>% 
  full_join(weather_110_690_time, by = "timedays") %>% 
  full_join(weather_110_515_time, by = "timedays") %>% 
  full_join(weather_110_560_time, by = "timedays") %>% 
  full_join(weather_110_650_time, by = "timedays") %>% 
  arrange(timedays)
# 시간 변환
weather_110_time$timedays = ymd_h(weather_110_time$timedays)

# 종로구 일별 결합
weather_110_day = full_join(weather_110_600_day, weather_110_580_day, by = "days") %>%                                full_join(weather_110_550_day, by = "days") %>% 
  full_join(weather_110_530_day, by = "days") %>% 
  full_join(weather_110_710_day, by = "days") %>% 
  full_join(weather_110_640_day, by = "days") %>% 
  full_join(weather_110_615_day, by = "days") %>% 
  full_join(weather_110_630_day, by = "days") %>% 
  full_join(weather_110_670_day, by = "days") %>% 
  full_join(weather_110_690_day, by = "days") %>% 
  full_join(weather_110_515_day, by = "days") %>% 
  full_join(weather_110_560_day, by = "days") %>% 
  full_join(weather_110_650_day, by = "days") %>% 
  arrange(days)
weather_110_day$days = ymd(weather_110_day$days)

# 노원구 시간별 결합
weather_350_time = full_join(weather_350_595_time, weather_350_600_time, by = "timedays") %>%                          full_join(weather_350_630_time, by = "timedays") %>% 
  full_join(weather_350_640_time, by = "timedays") %>% 
  full_join(weather_350_665_time, by = "timedays") %>% 
  full_join(weather_350_670_time, by = "timedays") %>% 
  full_join(weather_350_695_time, by = "timedays") %>% 
  full_join(weather_350_720_time, by = "timedays") %>% 
  full_join(weather_350_560_time, by = "timedays") %>% 
  full_join(weather_350_580_time, by = "timedays") %>% 
  full_join(weather_350_625_time, by = "timedays") %>% 
  full_join(weather_350_619_time, by = "timedays") %>% 
  full_join(weather_350_611_time, by = "timedays") %>% 
  arrange(timedays)
# 시간 변환
weather_350_time$timedays = ymd_h(weather_350_time$timedays)

# 노원구 일별 결합
weather_350_day = full_join(weather_350_595_day, weather_350_600_day, by = "days") %>%                                full_join(weather_350_630_day, by = "days") %>% 
  full_join(weather_350_640_day, by = "days") %>% 
  full_join(weather_350_665_day, by = "days") %>% 
  full_join(weather_350_670_day, by = "days") %>% 
  full_join(weather_350_695_day, by = "days") %>% 
  full_join(weather_350_720_day, by = "days") %>% 
  full_join(weather_350_560_day, by = "days") %>% 
  full_join(weather_350_580_day, by = "days") %>% 
  full_join(weather_350_625_day, by = "days") %>% 
  full_join(weather_350_619_day, by = "days") %>% 
  full_join(weather_350_611_day, by = "days") %>% 
  arrange(days)
weather_350_day$days = ymd(weather_350_day$days)

# 기상데이터에 없었던 지역 제외
jongro = sort(unique(flowage$HDONG_NM)[1:17])[-c(3, 6, 7, 13)]
nowon = sort(unique(flowage$HDONG_NM)[18:36])[-c(9, 10, 12, 14, 16, 19)]

# 열 이름 설정
weather_110_time_pm10 = weather_110_time %>% dplyr::select(contains("pm10")) %>% 
  setNames(paste("pm10", jongro, sep = "_"))
weather_110_time_pm25 = weather_110_time %>% dplyr::select(contains("pm25")) %>% 
  setNames(paste("pm25", jongro, sep = "_"))
weather_110_time_temp = weather_110_time %>% dplyr::select(contains("temp")) %>% 
  setNames(paste("temp", jongro, sep = "_"))
weather_110_time_humid = weather_110_time %>% dplyr::select(contains("humid")) %>% 
  setNames(paste("humid", jongro, sep = "_"))

# 열 평균으로 NA 대체 (시간대별 구 평균)
for(i in 1:dim(weather_110_time_pm10)[1]){
  for(j in 1:dim(weather_110_time_pm10)[2]){
    if(is.na(weather_110_time_pm10[i, j]) == T){
      weather_110_time_pm10[i, j] = apply(weather_110_time_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_time_pm25[i, j]) == T){
      weather_110_time_pm25[i, j] = apply(weather_110_time_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_time_temp[i, j]) == T){
      weather_110_time_temp[i, j] = apply(weather_110_time_temp[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_time_humid[i, j]) == T){
      weather_110_time_humid[i, j] = apply(weather_110_time_humid[i, ], 1, mean, na.rm = T)
    }
  }
}

# 데이터 결합
weather_110_time1 = cbind(weather_110_time$timedays, weather_110_time_pm10,
                          weather_110_time_pm25, weather_110_time_temp, weather_110_time_humid) %>% 
  rename(days = `weather_110_time$timedays`)


# 데이터 저장을 위해 동별 분해
weather_110_time1_600 = weather_110_time1 %>% dplyr::select(1, contains("가회동")) %>%
  rename(pm10 = pm10_가회동,
         pm25 = pm25_가회동,
         humid = humid_가회동,
         temp = temp_가회동) %>%
  mutate(region = paste("가회동"))

weather_110_time1_580 = weather_110_time1 %>% dplyr::select(1, contains("교남동")) %>%
  rename(pm10 = pm10_교남동,
         pm25 = pm25_교남동,
         humid = humid_교남동,
         temp = temp_교남동) %>%
  mutate(region = paste("교남동"))

weather_110_time1_550 = weather_110_time1 %>% dplyr::select(1, contains("부암동")) %>%
  rename(pm10 = pm10_부암동,
         pm25 = pm25_부암동,
         humid = humid_부암동,
         temp = temp_부암동) %>%
  mutate(region = paste("부암동"))

weather_110_time1_530 = weather_110_time1 %>% dplyr::select(1, contains("사직동")) %>%
  rename(pm10 = pm10_사직동,
         pm25 = pm25_사직동,
         humid = humid_사직동,
         temp = temp_사직동) %>%
  mutate(region = paste("사직동"))

weather_110_time1_710 = weather_110_time1 %>% dplyr::select(1, contains("숭인2동")) %>%
  rename(pm10 = pm10_숭인2동,
         pm25 = pm25_숭인2동,
         humid = humid_숭인2동,
         temp = temp_숭인2동) %>%
  mutate(region = paste("숭인2동"))

weather_110_time1_640 = weather_110_time1 %>% dplyr::select(1, contains("이화동")) %>%
  rename(pm10 = pm10_이화동,
         pm25 = pm25_이화동,
         humid = humid_이화동,
         temp = temp_이화동) %>%
  mutate(region = paste("이화동"))

weather_110_time1_615 = weather_110_time1 %>% dplyr::select(1, contains("종로1,2,3,4가동")) %>%
  rename(pm10 = `pm10_종로1,2,3,4가동`,
         pm25 = `pm25_종로1,2,3,4가동`,
         humid = `humid_종로1,2,3,4가동`,
         temp = `temp_종로1,2,3,4가동`) %>%
  mutate(region = paste("종로1,2,3,4가동"))

weather_110_time1_630 = weather_110_time1 %>% dplyr::select(1, contains("종로5,6가동")) %>%
  rename(pm10 = `pm10_종로5,6가동`,
         pm25 = `pm25_종로5,6가동`,
         humid = `humid_종로5,6가동`,
         temp = `temp_종로5,6가동`) %>%
  mutate(region = paste("종로5,6가동"))

weather_110_time1_670 = weather_110_time1 %>% dplyr::select(1, contains("창신1동")) %>%
  rename(pm10 = pm10_창신1동,
         pm25 = pm25_창신1동,
         humid = humid_창신1동,
         temp = temp_창신1동) %>%
  mutate(region = paste("창신1동"))

weather_110_time1_690 = weather_110_time1 %>% dplyr::select(1, contains("창신3동")) %>%
  rename(pm10 = pm10_창신3동,
         pm25 = pm25_창신3동,
         humid = humid_창신3동,
         temp = temp_창신3동) %>%
  mutate(region = paste("창신3동"))

weather_110_time1_515 = weather_110_time1 %>% dplyr::select(1, contains("청운효자동")) %>%
  rename(pm10 = pm10_청운효자동,
         pm25 = pm25_청운효자동,
         humid = humid_청운효자동,
         temp = temp_청운효자동) %>%
  mutate(region = paste("청운효자동"))

weather_110_time1_560 = weather_110_time1 %>% dplyr::select(1, contains("평창동")) %>%
  rename(pm10 = pm10_평창동,
         pm25 = pm25_평창동,
         humid = humid_평창동,
         temp = temp_평창동) %>%
  mutate(region = paste("평창동"))

weather_110_time1_650 = weather_110_time1 %>% dplyr::select(1, contains("혜화동")) %>%
  rename(pm10 = pm10_혜화동,
         pm25 = pm25_혜화동,
         humid = humid_혜화동,
         temp = temp_혜화동) %>%
  mutate(region = paste("혜화동"))


# 종로구 시간별 결합
weather_110_t = rbind(weather_110_time1_600, weather_110_time1_580, weather_110_time1_550,
                      weather_110_time1_530, weather_110_time1_710, weather_110_time1_640,
                      weather_110_time1_615, weather_110_time1_630, weather_110_time1_670,
                      weather_110_time1_690, weather_110_time1_515, weather_110_time1_560,
                      weather_110_time1_650)


# 열 이름 설정
weather_110_day_pm10 = weather_110_day %>% dplyr::select(contains("pm10")) %>% 
  setNames(paste("pm10", jongro, sep = "_"))
weather_110_day_pm25 = weather_110_day %>% dplyr::select(contains("pm25")) %>% 
  setNames(paste("pm25", jongro, sep = "_"))
weather_110_day_temp = weather_110_day %>% dplyr::select(contains("temp")) %>% 
  setNames(paste("temp", jongro, sep = "_"))
weather_110_day_humid = weather_110_day %>% dplyr::select(contains("humid")) %>% 
  setNames(paste("humid", jongro, sep = "_"))


# 열 평균으로 NA 대체 (일별 구 평균)
for(i in 1:dim(weather_110_day_pm10)[1]){
  for(j in 1:dim(weather_110_day_pm10)[2]){
    if(is.na(weather_110_day_pm10[i, j]) == T){
      weather_110_day_pm10[i, j] = apply(weather_110_day_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_day_pm25[i, j]) == T){
      weather_110_day_pm25[i, j] = apply(weather_110_day_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_day_temp[i, j]) == T){
      weather_110_day_temp[i, j] = apply(weather_110_day_temp[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_110_day_humid[i, j]) == T){
      weather_110_day_humid[i, j] = apply(weather_110_day_humid[i, ], 1, mean, na.rm = T)
    }
  }
}

# 데이터 결합
weather_110_day1 = cbind(weather_110_day$days, weather_110_day_pm10,
                         weather_110_day_pm25, weather_110_day_temp, weather_110_day_humid) %>% 
  rename(days = `weather_110_day$days`)

# 데이터 저장을 위해 동별 분해
weather_110_day1_600 = weather_110_day1 %>% dplyr::select(1, contains("가회동")) %>%
  rename(pm10 = pm10_가회동,
         pm25 = pm25_가회동,
         humid = humid_가회동,
         temp = temp_가회동) %>%
  mutate(region = paste("가회동"))

weather_110_day1_580 = weather_110_day1 %>% dplyr::select(1, contains("교남동")) %>%
  rename(pm10 = pm10_교남동,
         pm25 = pm25_교남동,
         humid = humid_교남동,
         temp = temp_교남동) %>%
  mutate(region = paste("교남동"))

weather_110_day1_550 = weather_110_day1 %>% dplyr::select(1, contains("부암동")) %>%
  rename(pm10 = pm10_부암동,
         pm25 = pm25_부암동,
         humid = humid_부암동,
         temp = temp_부암동) %>%
  mutate(region = paste("부암동"))

weather_110_day1_530 = weather_110_day1 %>% dplyr::select(1, contains("사직동")) %>%
  rename(pm10 = pm10_사직동,
         pm25 = pm25_사직동,
         humid = humid_사직동,
         temp = temp_사직동) %>%
  mutate(region = paste("사직동"))

weather_110_day1_710 = weather_110_day1 %>% dplyr::select(1, contains("숭인2동")) %>%
  rename(pm10 = pm10_숭인2동,
         pm25 = pm25_숭인2동,
         humid = humid_숭인2동,
         temp = temp_숭인2동) %>%
  mutate(region = paste("숭인2동"))

weather_110_day1_640 = weather_110_day1 %>% dplyr::select(1, contains("이화동")) %>%
  rename(pm10 = pm10_이화동,
         pm25 = pm25_이화동,
         humid = humid_이화동,
         temp = temp_이화동) %>%
  mutate(region = paste("이화동"))

weather_110_day1_615 = weather_110_day1 %>% dplyr::select(1, contains("종로1,2,3,4가동")) %>%
  rename(pm10 = `pm10_종로1,2,3,4가동`,
         pm25 = `pm25_종로1,2,3,4가동`,
         humid = `humid_종로1,2,3,4가동`,
         temp = `temp_종로1,2,3,4가동`) %>%
  mutate(region = paste("종로1,2,3,4가동"))

weather_110_day1_630 = weather_110_day1 %>% dplyr::select(1, contains("종로5,6가동")) %>%
  rename(pm10 = `pm10_종로5,6가동`,
         pm25 = `pm25_종로5,6가동`,
         humid = `humid_종로5,6가동`,
         temp = `temp_종로5,6가동`) %>%
  mutate(region = paste("종로5,6가동"))

weather_110_day1_670 = weather_110_day1 %>% dplyr::select(1, contains("창신1동")) %>%
  rename(pm10 = pm10_창신1동,
         pm25 = pm25_창신1동,
         humid = humid_창신1동,
         temp = temp_창신1동) %>%
  mutate(region = paste("창신1동"))

weather_110_day1_690 = weather_110_day1 %>% dplyr::select(1, contains("창신3동")) %>%
  rename(pm10 = pm10_창신3동,
         pm25 = pm25_창신3동,
         humid = humid_창신3동,
         temp = temp_창신3동) %>%
  mutate(region = paste("창신3동"))

weather_110_day1_515 = weather_110_day1 %>% dplyr::select(1, contains("청운효자동")) %>%
  rename(pm10 = pm10_청운효자동,
         pm25 = pm25_청운효자동,
         humid = humid_청운효자동,
         temp = temp_청운효자동) %>%
  mutate(region = paste("청운효자동"))

weather_110_day1_560 = weather_110_day1 %>% dplyr::select(1, contains("평창동")) %>%
  rename(pm10 = pm10_평창동,
         pm25 = pm25_평창동,
         humid = humid_평창동,
         temp = temp_평창동) %>%
  mutate(region = paste("평창동"))

weather_110_day1_650 = weather_110_day1 %>% dplyr::select(1, contains("혜화동")) %>%
  rename(pm10 = pm10_혜화동,
         pm25 = pm25_혜화동,
         humid = humid_혜화동,
         temp = temp_혜화동) %>%
  mutate(region = paste("혜화동"))

# 종로구 일별 결합
weather_110_d = rbind(weather_110_day1_600, weather_110_day1_580, weather_110_day1_550,
                      weather_110_day1_530, weather_110_day1_710, weather_110_day1_640,
                      weather_110_day1_615, weather_110_day1_630, weather_110_day1_670,
                      weather_110_day1_690, weather_110_day1_515, weather_110_day1_560,
                      weather_110_day1_650)


# 열 이름 설정
weather_350_time_pm10 = weather_350_time %>% dplyr::select(contains("pm10")) %>% 
  setNames(paste("pm10", nowon, sep = "_"))
weather_350_time_pm25 = weather_350_time %>% dplyr::select(contains("pm25")) %>% 
  setNames(paste("pm25", nowon, sep = "_"))
weather_350_time_temp = weather_350_time %>% dplyr::select(contains("temp")) %>% 
  setNames(paste("temp", nowon, sep = "_"))
weather_350_time_humid = weather_350_time %>% dplyr::select(contains("humid")) %>% 
  setNames(paste("humid", nowon, sep = "_"))

# 열 평균으로 NA 대체 (시간대별 구 평균)
for(i in 1:dim(weather_350_time_pm10)[1]){
  for(j in 1:dim(weather_350_time_pm10)[2]){
    if(is.na(weather_350_time_pm10[i, j]) == T){
      weather_350_time_pm10[i, j] = apply(weather_350_time_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_time_pm25[i, j]) == T){
      weather_350_time_pm25[i, j] = apply(weather_350_time_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_time_temp[i, j]) == T){
      weather_350_time_temp[i, j] = apply(weather_350_time_temp[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_time_humid[i, j]) == T){
      weather_350_time_humid[i, j] = apply(weather_350_time_humid[i, ], 1, mean, na.rm = T)
    }
  }
}

# 데이터 결합
weather_350_time1 = cbind(weather_350_time$timedays, weather_350_time_pm10,
                          weather_350_time_pm25, weather_350_time_temp, weather_350_time_humid) %>% 
  rename(days = `weather_350_time$timedays`)

# 데이터 저장을 위해 동별 분해
weather_350_time1_595 = weather_350_time1 %>% dplyr::select(1, contains("공릉1동")) %>%
  rename(pm10 = pm10_공릉1동,
         pm25 = pm25_공릉1동,
         humid = humid_공릉1동,
         temp = temp_공릉1동) %>%
  mutate(region = paste("공릉1동"))

weather_350_time1_600 = weather_350_time1 %>% dplyr::select(1, contains("공릉2동")) %>%
  rename(pm10 = pm10_공릉2동,
         pm25 = pm25_공릉2동,
         humid = humid_공릉2동,
         temp = temp_공릉2동) %>%
  mutate(region = paste("공릉2동"))

weather_350_time1_630 = weather_350_time1 %>% dplyr::select(1, contains("상계1동")) %>%
  rename(pm10 = pm10_상계1동,
         pm25 = pm25_상계1동,
         humid = humid_상계1동,
         temp = temp_상계1동) %>%
  mutate(region = paste("상계1동"))

weather_350_time1_640 = weather_350_time1 %>% dplyr::select(1, contains("상계2동")) %>%
  rename(pm10 = pm10_상계2동,
         pm25 = pm25_상계2동,
         humid = humid_상계2동,
         temp = temp_상계2동) %>%
  mutate(region = paste("상계2동"))

weather_350_time1_665 = weather_350_time1 %>% dplyr::select(1, contains("상계3,4동")) %>%
  rename(pm10 = `pm10_상계3,4동`,
         pm25 = `pm25_상계3,4동`,
         humid = `humid_상계3,4동`,
         temp = `temp_상계3,4동`) %>%
  mutate(region = paste("상계3,4동"))

weather_350_time1_670 = weather_350_time1 %>% dplyr::select(1, contains("상계5동")) %>%
  rename(pm10 = pm10_상계5동,
         pm25 = pm25_상계5동,
         humid = humid_상계5동,
         temp = temp_상계5동) %>%
  mutate(region = paste("상계5동"))

weather_350_time1_695 = weather_350_time1 %>% dplyr::select(1, contains("상계6,7동")) %>%
  rename(pm10 = `pm10_상계6,7동`,
         pm25 = `pm25_상계6,7동`,
         humid = `humid_상계6,7동`,
         temp = `temp_상계6,7동`) %>%
  mutate(region = paste("상계6,7동"))

weather_350_time1_720 = weather_350_time1 %>% dplyr::select(1, contains("상계10동")) %>%
  rename(pm10 = pm10_상계10동,
         pm25 = pm25_상계10동,
         humid = humid_상계10동,
         temp = temp_상계10동) %>%
  mutate(region = paste("상계10동"))

weather_350_time1_560 = weather_350_time1 %>% dplyr::select(1, contains("월계1동")) %>%
  rename(pm10 = pm10_월계1동,
         pm25 = pm25_월계1동,
         humid = humid_월계1동,
         temp = temp_월계1동) %>%
  mutate(region = paste("월계1동"))

weather_350_time1_580 = weather_350_time1 %>% dplyr::select(1, contains("월계3동")) %>%
  rename(pm10 = pm10_월계3동,
         pm25 = pm25_월계3동,
         humid = humid_월계3동,
         temp = temp_월계3동) %>%
  mutate(region = paste("월계3동"))

weather_350_time1_625 = weather_350_time1 %>% dplyr::select(1, contains("중계2,3동")) %>%
  rename(pm10 = `pm10_중계2,3동`,
         pm25 = `pm25_중계2,3동`,
         humid = `humid_중계2,3동`,
         temp = `temp_중계2,3동`) %>%
  mutate(region = paste("중계2,3동"))

weather_350_time1_619 = weather_350_time1 %>% dplyr::select(1, contains("중계본동")) %>%
  rename(pm10 = pm10_중계본동,
         pm25 = pm25_중계본동,
         humid = humid_중계본동,
         temp = temp_중계본동) %>%
  mutate(region = paste("중계본동"))

weather_350_time1_611 = weather_350_time1 %>% dplyr::select(1, contains("하계1동")) %>%
  rename(pm10 = pm10_하계1동,
         pm25 = pm25_하계1동,
         humid = humid_하계1동,
         temp = temp_하계1동) %>%
  mutate(region = paste("하계1동"))

# 노원구 시간별 결합
weather_350_t = rbind(weather_350_time1_595, weather_350_time1_600, weather_350_time1_630,
                      weather_350_time1_640, weather_350_time1_665, weather_350_time1_670,
                      weather_350_time1_695, weather_350_time1_720, weather_350_time1_560,
                      weather_350_time1_580, weather_350_time1_625, weather_350_time1_619,
                      weather_350_time1_611)


# 열 이름 설정
weather_350_day_pm10 = weather_350_day %>% dplyr::select(contains("pm10")) %>% 
  setNames(paste("pm10", nowon, sep = "_"))
weather_350_day_pm25 = weather_350_day %>% dplyr::select(contains("pm25")) %>% 
  setNames(paste("pm25", nowon, sep = "_"))
weather_350_day_temp = weather_350_day %>% dplyr::select(contains("temp")) %>% 
  setNames(paste("temp", nowon, sep = "_"))
weather_350_day_humid = weather_350_day %>% dplyr::select(contains("humid")) %>% 
  setNames(paste("humid", nowon, sep = "_"))

# 열 평균으로 NA 대체 (시간대별 구 평균)
for(i in 1:dim(weather_350_day_pm10)[1]){
  for(j in 1:dim(weather_350_day_pm10)[2]){
    if(is.na(weather_350_day_pm10[i, j]) == T){
      weather_350_day_pm10[i, j] = apply(weather_350_day_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_day_pm25[i, j]) == T){
      weather_350_day_pm25[i, j] = apply(weather_350_day_pm10[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_day_temp[i, j]) == T){
      weather_350_day_temp[i, j] = apply(weather_350_day_temp[i, ], 1, mean, na.rm = T)
    }
    if(is.na(weather_350_day_humid[i, j]) == T){
      weather_350_day_humid[i, j] = apply(weather_350_day_humid[i, ], 1, mean, na.rm = T)
    }
  }
}

# 데이터 결합
weather_350_day1 = cbind(weather_350_time$timedays, weather_350_time_pm10,
                         weather_350_time_pm25, weather_350_time_temp, weather_350_time_humid) %>% 
  rename(days = `weather_350_time$timedays`)

# 데이터 저장을 위해 동별 분해
weather_350_day1_595 = weather_350_day1 %>% dplyr::select(1, contains("공릉1동")) %>%
  rename(pm10 = pm10_공릉1동,
         pm25 = pm25_공릉1동,
         humid = humid_공릉1동,
         temp = temp_공릉1동) %>%
  mutate(region = paste("공릉1동"))

weather_350_day1_600 = weather_350_day1 %>% dplyr::select(1, contains("공릉2동")) %>%
  rename(pm10 = pm10_공릉2동,
         pm25 = pm25_공릉2동,
         humid = humid_공릉2동,
         temp = temp_공릉2동) %>%
  mutate(region = paste("공릉2동"))

weather_350_day1_630 = weather_350_day1 %>% dplyr::select(1, contains("상계1동")) %>%
  rename(pm10 = pm10_상계1동,
         pm25 = pm25_상계1동,
         humid = humid_상계1동,
         temp = temp_상계1동) %>%
  mutate(region = paste("상계1동"))

weather_350_day1_640 = weather_350_day1 %>% dplyr::select(1, contains("상계2동")) %>%
  rename(pm10 = pm10_상계2동,
         pm25 = pm25_상계2동,
         humid = humid_상계2동,
         temp = temp_상계2동) %>%
  mutate(region = paste("상계2동"))

weather_350_day1_665 = weather_350_day1 %>% dplyr::select(1, contains("상계3,4동")) %>%
  rename(pm10 = `pm10_상계3,4동`,
         pm25 = `pm25_상계3,4동`,
         humid = `humid_상계3,4동`,
         temp = `temp_상계3,4동`) %>%
  mutate(region = paste("상계3,4동"))

weather_350_day1_670 = weather_350_day1 %>% dplyr::select(1, contains("상계5동")) %>%
  rename(pm10 = pm10_상계5동,
         pm25 = pm25_상계5동,
         humid = humid_상계5동,
         temp = temp_상계5동) %>%
  mutate(region = paste("상계5동"))

weather_350_day1_695 = weather_350_day1 %>% dplyr::select(1, contains("상계6,7동")) %>%
  rename(pm10 = `pm10_상계6,7동`,
         pm25 = `pm25_상계6,7동`,
         humid = `humid_상계6,7동`,
         temp = `temp_상계6,7동`) %>%
  mutate(region = paste("상계6,7동"))

weather_350_day1_720 = weather_350_day1 %>% dplyr::select(1, contains("상계10동")) %>%
  rename(pm10 = pm10_상계10동,
         pm25 = pm25_상계10동,
         humid = humid_상계10동,
         temp = temp_상계10동) %>%
  mutate(region = paste("상계10동"))

weather_350_day1_560 = weather_350_day1 %>% dplyr::select(1, contains("월계1동")) %>%
  rename(pm10 = pm10_월계1동,
         pm25 = pm25_월계1동,
         humid = humid_월계1동,
         temp = temp_월계1동) %>%
  mutate(region = paste("월계1동"))

weather_350_day1_580 = weather_350_day1 %>% dplyr::select(1, contains("월계3동")) %>%
  rename(pm10 = pm10_월계3동,
         pm25 = pm25_월계3동,
         humid = humid_월계3동,
         temp = temp_월계3동) %>%
  mutate(region = paste("월계3동"))

weather_350_day1_625 = weather_350_day1 %>% dplyr::select(1, contains("중계2,3동")) %>%
  rename(pm10 = `pm10_중계2,3동`,
         pm25 = `pm25_중계2,3동`,
         humid = `humid_중계2,3동`,
         temp = `temp_중계2,3동`) %>%
  mutate(region = paste("중계2,3동"))

weather_350_day1_619 = weather_350_day1 %>% dplyr::select(1, contains("중계본동")) %>%
  rename(pm10 = pm10_중계본동,
         pm25 = pm25_중계본동,
         humid = humid_중계본동,
         temp = temp_중계본동) %>%
  mutate(region = paste("중계본동"))

weather_350_day1_611 = weather_350_day1 %>% dplyr::select(1, contains("하계1동")) %>%
  rename(pm10 = pm10_하계1동,
         pm25 = pm25_하계1동,
         humid = humid_하계1동,
         temp = temp_하계1동) %>%
  mutate(region = paste("하계1동"))

# 노원구 일별 결합
weather_350_d = rbind(weather_350_day1_595, weather_350_day1_600, weather_350_day1_630,
                      weather_350_day1_640, weather_350_day1_665, weather_350_day1_670,
                      weather_350_day1_695, weather_350_day1_720, weather_350_day1_560,
                      weather_350_day1_580, weather_350_day1_625, weather_350_day1_619,
                      weather_350_day1_611)


# 시간, 일결 결합
weather_time = rbind(weather_110_t, weather_350_t)
weather_day = rbind(weather_110_d, weather_350_d)