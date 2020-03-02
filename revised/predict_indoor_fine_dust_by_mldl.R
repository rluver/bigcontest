require("data.table")
require("dplyr")
require("lubridate")
require("DMwR")
require("caret")
require('e1071') # svm, naivebayes
# require('MASS') # lda
require('class') #knn
require('tree') # decision tree
require('randomForest') # random forest
require('keras') # mp
require('xgboost') # xgboost
require('lightgbm') # lightgbm
require('catboost') # catboost



# user define
'%!in%' = Negate('%in%')

# data load and preprocessing
# weather
source("revised/preprocessing_weather.R", encoding = "UTF-8")

pm = weather_day %>% filter(dong == '종로5,6가동', days >= '2018-12-01') %>% 
  ungroup() %>% select(1:5) %>% 
  # combine dondaemoon total subway passengers
  left_join(
    fread('d:/bigcon/서울교통공사 2018년 일별 역별 시간대별 승하차인원(1_8호선).csv', header = T,
          select = c('날짜', '역명', '합 계')) %>% 
      filter(역명 == '동대문', `날짜` >= "2018-12-01") %>% 
      rename(days = '날짜', number = `합 계`) %>% 
      group_by(days) %>% 
      summarise(number = sum(number)) %>% 
      bind_rows(fread('d:/bigcon/서울교통공사_관할역별_일별_시간대별이용인원_20190631.csv', header = T,
                      select = c('날짜', '역명', '합 계')) %>% 
                  filter(역명 == '동대문') %>% 
                  rename(days = '날짜', number = `합 계`) %>% 
                  group_by(days) %>% 
                  summarise(number = sum(number))
                ) %>% 
      mutate(days = ymd(days)),
    by = 'days') %>% 
  # combine dongdaemoon inner fine dust
  left_join(fread('d:/bigcon/1201_0331.csv', header = T, select = c('측정일', '미세먼지')) %>% 
              rename(days = '측정일', inner_pm = '미세먼지') %>% 
              mutate(days = ymd(days)),
            by = 'days') %>% 
  # labeling by inner fine dust
  mutate(inner_pm_grade = ifelse(inner_pm < 80, 1, 2) %>% as.factor()) %>% 
  select(-inner_pm)




# check corr
pm %>% select(2:7) %>% GGally::ggpairs()

# check corr after holidays delete
pm %>% filter(days %!in% c(seq.Date(ymd('2018-12-01'), ymd('2019-03-31'), by = "1 week"), # sat
                           seq.Date(ymd('2018-12-02'), ymd('2019-03-31'), by = "1 week"), # sun
                           ymd(c('2018-12-25', '2019-01-01', '2019-02-04', '2019-02-05', '2019-02-06', '2019-03-01')))) %>% # hol
  select(2:7) %>% GGally::ggpairs()

# check imbalance
pm %>% select(inner_pm_grade) %>% table() %>% prop.table()*100

# apply SMOTE
pm_smote = SMOTE(inner_pm_grade ~ ., 
                 pm %>% select(-days) %>% as.data.frame(), 
                 perc.over = 600, 
                 perc.under = 200)

# check
pm_smote %>% select(inner_pm_grade) %>% table() %>% prop.table()*100
pm_smote %>% GGally::ggpairs()

pm_smote_rm_outlier = pm_smote %>% filter(quantile(pm_smote$number)[2] - (quantile(pm_smote$number)[4]-quantile(pm_smote$number)[2])*1.5 <= number,
                     number <= quantile(pm_smote$number)[4] + (quantile(pm_smote$number)[4]-quantile(pm_smote$number)[2])*1.5) %>% 
  GGally::ggpairs()


# classification
# index
ind = sample(2, nrow(pm_smote), replace = T, prob = c(.7, .3))

# scaling
pm_scale = pm_smote %>% mutate(pm10 = (pm10 - min(pm10))/(max(pm10) - min(pm10)),
                               pm25 = (pm25 - min(pm25))/(max(pm25) - min(pm25)),
                               temp = (temp - min(temp))/(max(temp) - min(temp)),
                               humid = (humid - min(humid))/(max(humid) - min(humid)),
                               number = (number - min(number))/(max(number) - min(number)))

## split data
pm_train = pm_scale %>% filter(ind == 1)
pm_test = pm_scale %>% filter(ind == 2)




# ml
## svm
model_svm = svm(inner_pm_grade ~ ., data = pm_train)
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-6)))


model_svm = svm(inner_pm_grade ~ ., data = pm_train %>% select(-2))
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-2, -6)))


model_svm = svm(inner_pm_grade ~ ., data = pm_train %>% select(-3))
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-3, -6)))


model_svm = svm(inner_pm_grade ~ ., data = pm_train %>% select(-2, -4))
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-2, -4, -6)))

model_svm = svm(inner_pm_grade ~ ., data = pm_train %>% select(-4))
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-4, -6)))

model_svm = svm(inner_pm_grade ~ ., data = pm_train %>% select(-3))
confusionMatrix(pm_test$inner_pm_grade,
                model_svm %>% predict(pm_test %>% select(-3, -6)))




## naivebayes
model_naivebayes = naiveBayes(inner_pm_grade ~ ., data = pm_train)
confusionMatrix(pm_test$inner_pm_grade,
                model_naivebayes %>% predict(pm_test %>% select(-6)))


model_naivebayes = naiveBayes(inner_pm_grade ~ ., data = pm_train %>% select(-2))
confusionMatrix(pm_test$inner_pm_grade,
                model_naivebayes %>% predict(pm_test %>% select(-2, -6)))


model_naivebayes = naiveBayes(inner_pm_grade ~ ., data = pm_train %>% select(-2, -4))
confusionMatrix(pm_test$inner_pm_grade,
                model_naivebayes %>% predict(pm_test %>% select(-2, -4, -6)))



## lda
model_lda = MASS::lda(inner_pm_grade ~ ., data = pm_train)
confusionMatrix(pm_test$inner_pm_grade,
                (model_lda %>% predict(pm_test %>% select(-6)))$class)

model_lda = MASS::lda(inner_pm_grade ~ ., data = pm_train %>% select(-2))
confusionMatrix(pm_test$inner_pm_grade,
                (model_lda %>% predict(pm_test %>% select(-2, -6)))$class)

model_lda = MASS::lda(inner_pm_grade ~ ., data = pm_train %>% select(-2, -4))
confusionMatrix(pm_test$inner_pm_grade,
                (model_lda %>% predict(pm_test %>% select(-2, -4, -6)))$class)



## qda
model_qda = MASS::qda(inner_pm_grade ~ ., data = pm_train)
confusionMatrix(pm_test$inner_pm_grade,
                (model_qda %>% predict(pm_test %>% select(-6)))$class)

model_qda = MASS::qda(inner_pm_grade ~ ., data = pm_train %>% select(-2, -4))
confusionMatrix(pm_test$inner_pm_grade,
                (model_qda %>% predict(pm_test %>% select(-2, -4, -6)))$class)



## knn
confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-6), pm_test %>% select(-6), cl = factor(pm_train[, 6]), k = 1))

confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-6), pm_test %>% select(-6), cl = factor(pm_train[, 6]), k = 2))

confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-6), pm_test %>% select(-6), cl = factor(pm_train[, 6]), k = 3))

confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-6), pm_test %>% select(-6), cl = factor(pm_train[, 6]), k = 10))



confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-2, -4, -6), pm_test %>% select(-2, -4, -6), cl = factor(pm_train[, 6]), k = 1))

confusionMatrix(pm_test$inner_pm_grade,
                knn(pm_train %>% select(-2, -4, -6), pm_test %>% select(-2, -4, -6), cl = factor(pm_train[, 6]), k = 9))




## logistic
model_logistic = glm(inner_pm_grade ~ ., family = "binomial", data = pm_train)
summary(model_logistic)
model_logistic = model_logistic %>% update(. ~ . -pm25)
summary(model_logistic)
confusionMatrix(pm_test$inner_pm_grade,
                 ifelse(model_logistic %>% predict(pm_test %>% select(-6), type = "response") <= 0.5, 1, 2) %>% factor())
model_logistic = model_logistic %>% update(. ~ . -temp)
summary(model_logistic)
confusionMatrix(pm_test$inner_pm_grade,
                ifelse(model_logistic %>% predict(pm_test %>% select(-6), type = "response") <= 0.5, 1, 2) %>% factor())
model_logistic = model_logistic %>% update(. ~ . -humid)
summary(model_logistic)
confusionMatrix(pm_test$inner_pm_grade,
                ifelse(model_logistic %>% predict(pm_test %>% select(-6), type = "response") <= 0.5, 1, 2) %>% factor())



## tree
### check prune
tree(inner_pm_grade ~., data = pm_train) %>% cv.tree(FUN = prune.misclass) %>% plot()

model_tree = tree(inner_pm_grade ~., data = pm_train) %>% prune.misclass(best = 8)
confusionMatrix(pm_test$inner_pm_grade,
                model_tree %>% predict(pm_test %>% select(-6), type = 'class'))
                                                             

tree(inner_pm_grade ~., data = pm_train %>% select(-2)) %>% cv.tree(FUN = prune.misclass) %>% plot()

model_tree = tree(inner_pm_grade ~., data = pm_train %>% select(-2)) %>% prune.misclass(best = 8)
confusionMatrix(pm_test$inner_pm_grade,
                model_tree %>% predict(pm_test %>% select(-2, -6), type = 'class'))


tree(inner_pm_grade ~., data = pm_train %>% select(-2, -4)) %>% cv.tree(FUN = prune.misclass) %>% plot()

model_tree = tree(inner_pm_grade ~., data = pm_train %>% select(-2, -4)) %>% prune.misclass(best = 8)
confusionMatrix(pm_test$inner_pm_grade,
                model_tree %>% predict(pm_test %>% select(-2, -4, -6), type = 'class'))




## random forest
model_rf = randomForest(inner_pm_grade ~., data = pm_train, mtry = floor(sqrt(5)), importance = T)
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-6), type = 'class'))

model_rf = model_rf %>% update(. ~ . - pm25)
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-6), type = 'class'))

model_rf = model_rf %>% update(. ~ . - humid, mtry = floor(sqrt(3)))
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-6), type = 'class'))



# dl
## keras
model_keras = keras_model_sequential() %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = 5) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 25, activation = 'relu') %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 10) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 5) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 2, activation = 'softmax') %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'Nadam',
          metrics = 'accuracy') %>% 
  fit(x = pm_train %>% select(-6) %>% as.matrix(),
      y = pm_train %>% select(6) %>% as.matrix(),
      epochs = 1000,
      batch_size = 20,
      validation_split = 0.3
  )
confusionMatrix(pm_test$inner_pm_grade,
                model_keras %>% predict(pm_test %>% select(-6) %>% as.matrix(), type = 'class'))

model_keras = keras_model_sequential() %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = 5) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 25, activation = 'relu') %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 10) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 5) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 2, activation = 'softmax') %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'Nadam',
          metrics = 'accuracy') %>% 
  fit(x = pm_train %>% select(-2, -6) %>% as.matrix(),
      y = pm_train %>% select(6) %>% as.matrix(),
      epochs = 1000,
      batch_size = 20,
      validation_split = 0.3
  )
confusionMatrix(pm_test$inner_pm_grade,
                model_keras %>% predict(pm_test %>% select(-2, -6) %>% as.matrix(), type = 'class'))

model_keras = keras_model_sequential() %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = 5) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 25, activation = 'relu') %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 10) %>% 
  layer_dropout(0.5) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 5) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 2, activation = 'softmax') %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'Nadam',
          metrics = 'accuracy') %>% 
  fit(x = pm_train %>% select(-2, -4, -6) %>% as.matrix(),
      y = pm_train %>% select(6) %>% as.matrix(),
      epochs = 1000,
      batch_size = 20,
      validation_split = 0.3
  )
confusionMatrix(pm_test$inner_pm_grade,
                model_keras %>% predict(pm_test %>% select(-2, -4, -6) %>% as.matrix(), type = 'class'))

  

## xgboost
model_xgboost = xgboost(data = pm_train %>% select(-6) %>% as.matrix(),
                        label = pm_train %>% select(6) %>% as.matrix() %>% as.numeric() - 1,
                        eta = 0.001,
                        nrounds = 1000,
                        objective = 'binary:logistic',
                        eval_metric = 'auc')
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-6), type = 'class'))


model_xgboost = xgboost(data = pm_train %>% select(-2, -6) %>% as.matrix(),
                        label = pm_train %>% select(6) %>% as.matrix() %>% as.numeric() - 1,
                        eta = 0.001,
                        nrounds = 1000,
                        objective = 'binary:logistic',
                        eval_metric = 'auc')
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-2, -6), type = 'class'))


model_xgboost = xgboost(data = pm_train %>% select(-2, -4, -6) %>% as.matrix(),
                        label = pm_train %>% select(6) %>% as.matrix() %>% as.numeric() - 1,
                        eta = 0.001,
                        nrounds = 1000,
                        objective = 'binary:logistic',
                        eval_metric = 'auc')
confusionMatrix(pm_test$inner_pm_grade,
                model_rf %>% predict(pm_test %>% select(-2, -4, -6), type = 'class'))



## catboost
model_catboost = catboost.train(learn_pool = catboost.load_pool(data = pm_train %>% select(-6), 
                                                                label = pm_train %>% select(6) %>% unlist() %>% as.numeric()),
                                params = list(loss_function = 'MultiClass',
                                              iterations = 1000,
                                              learning_rate = 0.001)) 
confusionMatrix(pm_test$inner_pm_grade,
                factor(model_catboost %>% catboost.predict(catboost.load_pool(pm_test %>% select(-6)), prediction_type = 'Class') + 1 ))
model_catboost %>% catboost.get_feature_importance()

model_catboost = catboost.train(learn_pool = catboost.load_pool(data = pm_train %>% select(-2, -6), 
                                                                label = pm_train %>% select(6) %>% unlist() %>% as.numeric()),
                                params = list(loss_function = 'MultiClass',
                                              iterations = 1000,
                                              learning_rate = 0.001)) 
confusionMatrix(pm_test$inner_pm_grade,
                factor(model_catboost %>% catboost.predict(catboost.load_pool(pm_test %>% select(-2, -6)), prediction_type = 'Class') + 1 ))
model_catboost %>% catboost.get_feature_importance()

model_catboost = catboost.train(learn_pool = catboost.load_pool(data = pm_train %>% select(-2, -4, -6), 
                                                                label = pm_train %>% select(6) %>% unlist() %>% as.numeric()),
                                params = list(loss_function = 'MultiClass',
                                              iterations = 1000,
                                              learning_rate = 0.001)) 
confusionMatrix(pm_test$inner_pm_grade,
                factor(model_catboost %>% catboost.predict(catboost.load_pool(pm_test %>% select(-2, -4, -6)), prediction_type = 'Class') + 1 ))
model_catboost %>% catboost.get_feature_importance()