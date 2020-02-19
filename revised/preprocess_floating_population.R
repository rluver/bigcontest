require("data.table")
require("stringr")
require("lubridate")
require("dplyr")
require("tidyr")




# flow by time
# rowb bind
flowtime = bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201804.csv",
                           header=T, sep="|", encoding = "UTF-8"),
                     fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201805.csv",
                           header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201806.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201807.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201808.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201809.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201810.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201811.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201812.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201901.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201902.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201903.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  # col rename
  rename(date = STD_YMD) %>% 
  # long form
  gather(key = "time", value = "number", names(.)[-1:-4]) %>% 
  # format change
  mutate(date = ymd(date),
         HDONG_NM = str_replace_all(HDONG_NM, "\\.", ","))
  



# flow by gender, age
# rowb bind
flowage = bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201804.csv",
                          header=T, sep="|", encoding = "UTF-8"),
                    fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201805.csv",
                          header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201806.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201807.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201808.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201809.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201810.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201811.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201812.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201901.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201902.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>% 
  bind_rows(fread("D:/bigcon/유동인구데이터/성연령유동/노원_종로_FLOW_AGE_201903.csv",
                  header=T, sep="|", encoding = "UTF-8")) %>%
  # col rename
  rename(date = STD_YMD) %>% 
  # long form
  gather(key = "time", value = "number", names(.)[-1:-4]) %>% 
  # format change
  mutate(date = ymd(date),
         HDONG_NM = str_replace_all(HDONG_NM, "\\.", ","))