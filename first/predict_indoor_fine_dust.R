require("data.table")
require("keras")
require("DMwR")
require("dplyr")
require("lubridate")
require("caret")





subway1 = fread("c:/공모전_시작이반이다/서울교통공사 역별 일별 시간대별 이용인원_20181231.csv", header = T) # 2018 지하철 이용객
subway2 = fread("c:/공모전_시작이반이다/서울교통공사_관할역별_일별_시간대별이용인원_20190631.csv", header = T) # 2019 지하철 이용객
weather1 = fread("c:/공모전_시작이반이다/weather_종로.csv", header = T)
iweather = fread("c:/공모전_시작이반이다/1201_0331.csv", header = T) # 181201 ~ 190331 동대문 실내 미세먼지


subway1 = subway1[,-2] # 중복 열 이름 제거 


# 2018-12-01 ~ 2019-03-31의 동대문 역의 승하차 인원의 일별 합 
subway1 = subway1 %>% rename(days = `날짜`) %>% filter(역명 == "동대문") %>% 
  select(days, `합 계`) %>% mutate(days = ymd(days)) %>% group_by(days) %>%
  summarise(number = sum(`합 계`)) %>% filter(days >= "2018-12-01") 


subway2 = subway2 %>% rename(days = `날짜`) %>% filter(역명 == "동대문") %>% 
  select(days, `합 계`) %>% mutate(days = ymd(days)) %>% group_by(days) %>%
  summarise(number = sum(`합 계`)) %>% filter(days < "2019-04-01")


# 데이터 결합
subway = rbind(subway1, subway2)


# 기상데이터에서 종로5,6가동(동대문) 2018.12.01 ~ 자료 추출
weather1 = weather1 %>% filter(region == "종로5,6가동", days >= 20181201) %>% 
  select(1:5) %>% mutate(days = ymd(days))


# 실내미세먼지 전처리
iweather = iweather %>% select(측정일, 미세먼지) %>% rename(days = 측정일, pm = 미세먼지) %>% 
  select(days, pm) %>% mutate(days = ymd(days))


# 데이터 결합 및 미세먼지 80기준으로 라벨링
join = left_join(weather1, subway, by = "days") %>% 
  left_join(iweather, by = "days") %>% select(-1)
join$label = as.factor(ifelse(join$pm <= 80, 1, 2))


# 데이터 불균형 확인 및 오버샘플링 
table(join$label)




join2 = SMOTE(label ~., join, perc.over = 600, perc.under = 200)
table(join2$label)




# 7:3으로 자료 분리(학습, 검증)
ind = sample(2, nrow(join2), replace = T, prob = c(0.7, 0.3))


join3 = join2
# 데이터 정규화
join3 = join2 %>% mutate(
  pm10 = as.vector(normalize(as.numeric(as.matrix(join2)[, 1]))),
  pm25 = as.vector(normalize(as.numeric(as.matrix(join2)[, 2]))),
  humid = as.vector(normalize(as.numeric(as.matrix(join2)[, 3]))),
  temp = as.vector(normalize(as.numeric(as.matrix(join2)[, 4]))),
  number = as.vector(normalize(as.numeric(as.matrix(join2)[, 5])))
)



# 학습, 검증 데이터 설정, 라벨링

training = join3 %>% filter(ind == 1)
test = join3 %>% filter(ind == 2)
training$label = to_categorical(as.numeric(training$label) - 1)



# 예측 DL
model = keras_model_sequential()

model %>% layer_dense(units = 100, activation = "relu", input_shape = c(5)) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 70, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 50, activation = "relu") %>% 
  layer_batch_normalization() %>%  
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 30, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 15, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 10, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.05) %>% 
  layer_dense(units = 2, activation = "softmax")

model %>% compile(loss = "categorical_crossentropy",
                  optimizer = "adam",
                  metrics = "accuracy")

mymodel = model %>% fit(
  as.matrix(training[,c(1:5)]),
  as.matrix(training[,7]),
  epochs = 2000,
  batch_size = 30,
  validation_spit = 0.1
)

pred = model %>% predict(as.matrix(test[,c(1:5)]))
confusionMatrix(as.factor(max.col(pred)), test[,7])




