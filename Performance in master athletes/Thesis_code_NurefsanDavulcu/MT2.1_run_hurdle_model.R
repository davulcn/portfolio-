# MT2.1 Running and Hurdle Model - Train/Test Split 
# Nurefsan Davulcu 
library(dplyr)
library(car)
library(leaps)
library(glmnet)

run_data<-model_data %>% filter(event_type=='short track' | event_type=='long track'
                                | event_type=='hurdles') 
#run_data<- run_data %>% select(-c('event_type'))
nrow(run_data)
run_data<-na.omit(run_data) #367
nrow(run_data)
x <- run_data %>% select(-c('record_performance'))
y <- run_data$record_performance
hist(y)
num<- run_data %>% select(-c('Event','Gender','record_performance'))
table(run_data$Event)
table(run_data$Gender)
hist(run_data$Age)

## TRAIN/TEST SPLIT 
# Will use stratified sampling by event - 80/20 split 
set.seed(123)
train_size<-ceiling(0.8*nrow(run_data))
train_size
test_size<-floor(0.2*nrow(run_data))
test_size
train_size + test_size == nrow(run_data)
stratum<-table(run_data$Event)
# will eliminate 200m hurdles- only 1 obs 
nrow(run_data)
run_data<-run_data %>% filter(Event!='200m Hurdles')
nrow(run_data)

stratum<-table(run_data$Event)
stratum
#run_data$id<-c(1:nrow(run_data))
popn_size<-nrow(run_data)
sample_size<-train_size
prop<-sample_size/popn_size
train_data<-data.frame()
test_data<-data.frame()

for (i in c(1:nrow(stratum))) {
  print(names(stratum[i]))
  tmp<-run_data %>% filter(Event == names(stratum[i]))
  samp<-sample(c(1:nrow(tmp)),prop*stratum[i])
  print(samp)
  train_data<-rbind(train_data,tmp[samp,])
  test_data<-rbind(test_data,tmp[-samp,])
}

nrow(train_data) 
train_size
nrow(test_data)
test_size
nrow(train_data)  + nrow(test_data) == nrow(run_data)

x.train <- train_data %>% select(-c('record_performance'))
y.train <- train_data$record_performance
hist(y.train)
x.test <- test_data %>% select(-c('record_performance'))
y.test <- test_data$record_performance
hist(y.test)
