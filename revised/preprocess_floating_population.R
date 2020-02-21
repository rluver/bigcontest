require("data.table")
require("stringr")
require("lubridate")
require("dplyr")
require("tidyr")




# flow by time
# rowb bind
flowtime = bind_rows(list.files("D:/bigcon/유동인구데이터/시간대유동",
                                pattern = "*.CSV", full.names = T) %>% 
                       purrr::map_df(~ fread(., header = T, sep = "|", encoding = "UTF-8"))) %>% 
  # col rename
  rename(date = STD_YMD) %>% 
  # long form
  gather(key = "time", value = "number", names(.)[-1:-4]) %>% 
  # format change
  mutate(date = ymd(date),
         HDONG_NM = str_replace_all(HDONG_NM, "\\.", ","))
  



# flow by gender, age
# rowb bind
flowage = bind_rows(list.files("D:/bigcon/유동인구데이터/성연령유동",
                               pattern = "*.CSV", full.names = T) %>% 
                      purrr::map_df(~ fread(., header = T, sep = "|", encoding = "UTF-8"))) %>%
  # col rename
  rename(date = STD_YMD) %>% 
  # long form
  gather(key = "time", value = "number", names(.)[-1:-4]) %>% 
  # format change
  mutate(date = ymd(date),
         HDONG_NM = str_replace_all(HDONG_NM, "\\.", ","))
