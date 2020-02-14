require("data.table")
require("dplyr")
require("tidyr")




# 시간대별 데이로 로드 
flowtime1804 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201804.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1805 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201805.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1806 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201806.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1807 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201807.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1808 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201808.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1809 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201809.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1810 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201810.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1811 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201811.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1812 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201812.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1901 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201901.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1902 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201902.csv", 
                     header=T, sep="|", encoding = "UTF-8")
flowtime1903 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/시간대유동/노원_종로_FLOW_TIME_201903.csv", 
                     header=T, sep="|", encoding = "UTF-8")

# 데이터 결합
flowtime = rbind(flowtime1804, flowtime1805, flowtime1806, flowtime1807, 
                 flowtime1808, flowtime1809, flowtime1810, flowtime1811, 
                 flowtime1812, flowtime1901, flowtime1902, flowtime1903)

# 열 이름 변경
names(flowtime)[2] = "days"

# wide를 long으로 변환 
flowtime = gather(flowtime, key = "time", value = "number", names(flowtime)[-1:-4]) 

# 이름의 .을 ,로 변경
flowtime$HDONG_NM = gsub("\\.", ",", flowtime$HDONG_NM)


# 성, 연령별 데이로 로드 
flowage1804 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201804.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1805 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201805.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1806 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201806.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1807 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201807.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1808 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201808.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1809 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201809.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1810 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201810.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1811 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201811.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1812 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201812.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1901 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201901.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1902 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201902.csv", 
                    header=T, sep="|", encoding = "UTF-8")
flowage1903 = fread("c:/공모전_시작이반이다/raw/유동인구데이터/성연령유동/노원_종로_FLOW_age_201903.csv", 
                    header=T, sep="|", encoding = "UTF-8")

# 데이터 결합
flowage = rbind(flowage1804, flowage1805, flowage1806, flowage1807, 
                flowage1808, flowage1809, flowage1810, flowage1811, 
                flowage1812, flowage1901, flowage1902, flowage1903)

# 열 이름 변경
names(flowage)[2] = "days"

# wide를 long으로 변환
flowage = gather(flowage, key = "sexageflow", value = "number", names(flowage)[-1:-4])

# 이름의 .을 ,로 변환 
flowage$HDONG_NM = gsub("\\.", ",", flowage$HDONG_NM)




