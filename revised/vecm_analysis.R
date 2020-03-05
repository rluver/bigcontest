require('data.table')
require('readxl')
require('dplyr')
require('lubridate')
require('bit64')
require('dplyr')
require('tsDyn')
require('vars')
require('plotly')



# data load and preprocessing
# weather
source('revised/preprocessing_weather.R', encoding = 'UTF-8')
# floating
source('revised/preprocess_floating_population.R', encoding = 'UTF-8')

guide = read_excel('d:/bigcon/카드매출데이터/2019빅콘테스트_신한카드소비데이터_레이아웃_최종_190711.xlsx', sheet = 2) %>% 
  rename(GU_CD = '구코드', DONG_CD = '행정동코드') %>% 
  mutate(DONG_CD = DONG_CD %>% as.numeric())

card = fread('d:/bigcon/카드매출데이터/CARD_SPENDING_190809.txt', header = T) %>% 
  rename(date = STD_DD) %>% 
  mutate(date = ymd(date)) %>% 
  left_join(guide, by = c('GU_CD', 'DONG_CD')) %>% 
  dplyr::select(1, '구명', '행정동명', everything(), -GU_CD, -DONG_CD) %>% 
  rename(gu = '구명', dong = '행정동명')

total = card %>% filter(dong == '가회동', SEX_CD == 'F', AGE_CD == 20, MCT_CAT_CD == 71) %>% dplyr::select(1, 7, 8) %>% 
  left_join(weather_day %>% filter(dong == '가회동') %>% dplyr::select(1, 2, 4), by = 'date') %>% 
  left_join(flowage %>% filter(HDONG_NM == '가회동', time == 'WMAN_FLOW_POP_CNT_2024') %>% dplyr::select(2, 6), by = 'date') %>% 
  dplyr::select(1, 4, 5, 3, 2, 6) %>% 
  mutate('amt/number' = USE_AMT/number,
         'cnt/number' = USE_CNT/number)

rm(guide)




# vecm analysis
# select lag
VARselect(total %>% dplyr::select(2, 3, 7, 8), lag.max = 10, type = 'const')
# check cointegration 
summary(ca.jo(total %>% dplyr::select(2, 3, 7, 8), type = 'trace', ecdet = 'const', K = 2, spec = 'transitory'))
# create model
vecm = VECM(total %>% dplyr::select(2, 3, 7, 8), lag = 1, r = 2, include = 'const', estim = 'ML')

# visualization
plot_ly() %>% 
  add_ribbons(x = seq(11),
              ymin = irf(vecm)$Lower$`amt/number`[, 3],
              ymax = irf(vecm)$Upper$`amt/number`[, 3],
              name = '95% CI',
              line = list(color = '#EEEEEE'),
              fillcolor = '#CCCCCC') %>% 
  add_lines(x = seq(11),
            y = 0,
            line = list(color = 'FF0033'),
            name = '0') %>% 
  add_lines(x = seq(11),
            y = irf(vecm)$irf$`amt/number`[, 3],
            name = '유동인구',
            line = list(color = '#0066FF'))