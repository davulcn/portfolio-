# MT2.2 Running and Hurdle Model - Linear Regression
# Nurefsan Davulcu 
library(dplyr)
library(car)
library(leaps)
library(glmnet)
set.seed(123)

# Cross-validation on training data-set 
k <- 5
set.seed(123)
run_cv<-CV_stratify(data = train_data, k = k)
# checks - good 
nrow(run_cv)==nrow(train_data)
sum(is.na(run_cv$fold))
table(run_cv$fold) 

# full model 
run_model1<-lm(record_performance ~ . -event_type,train_data)
summary(run_model1)
anova(run_model1)
vif(run_model1) 
AIC(run_model1)
# cv error
CV_lm(run_cv,k,x = c('Height','Weight','Age','Gender',
                     'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop',
                     'BFM','PhA'),
      y = 'record_performance')

# remove pha
run_model2<-lm(record_performance ~ Height+Age+Gender+Weight
               +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop+BFM,train_data)
summary(run_model2)
anova(run_model2)
vif(run_model2) 
AIC(run_model2)
# cv error
CV_lm(run_cv,k,x = c('Height','Weight','Age','Gender',
                     'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop',
                     'BFM'),
      y = 'record_performance')

# remove weight - **this was the "best model".. but not by much and
# weight is easier information to collect than BFM
# check the CV errors also - done
run_model3<-lm(record_performance ~ Height+Age+Gender
               +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop+BFM,train_data)
summary(run_model3)
anova(run_model3)
vif(run_model3) 
AIC(run_model3)
# cv error
CV_lm(run_cv,k,x = c('Height','Age','Gender',
                      'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop','BFM'),
      y = 'record_performance')


# remove BFM instead of weight  
run_model35<-lm(record_performance ~ Height+Age+Weight+Gender
                +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,train_data)
summary(run_model35)
anova(run_model35)
vif(run_model35) 
AIC(run_model35)
# cv error
CV_lm(run_cv,k,x = c('Height','Age','Gender',
                     'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop','Weight'),
      y = 'record_performance')
# cv errors almost the same as removing Weight, so go with this one since 
# weight is easier to collect

# REDUCED MODEL - only Height Weight Age and Gender and Event
run_reduced<-lm(record_performance ~ Height+Age+Gender+Weight
                +Event,train_data)
summary(run_reduced)
anova(run_reduced)
vif(run_reduced) 
AIC(run_reduced)
CV_lm(run_cv,k,x = c('Height','Age','Gender',
                     'Event','Weight'),
      y = 'record_performance')

# F test for final model35 with reduced model
anova(run_reduced,run_model35)
# contributing significant information:) <0.0001

# F test for final model35 with full model
anova(run_model35,run_model1)

# FINAL TEST ERROR 
yhat<-predict(run_model35,newdata=x.test,type='response')
errors(y.test,yhat)

