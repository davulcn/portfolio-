#MT3.2 Throw Model
library(dplyr)
library(car)

y <- throw_data$record_performance

throw_model1<-lm(record_performance ~ . ,throw_data)
summary(throw_model1)
anova(throw_model1)
vif(throw_model1) 
plot(throw_model1)
#training error 
yhat <- throw_model1$fitted.values
errors(y,yhat)

# log transform the response
throw_model15<-lm(log(record_performance) ~ . ,throw_data)
summary(throw_model15)
anova(throw_model15)
vif(throw_model15) 
plot(throw_model15)
#training error 
yhat <- exp(throw_model15$fitted.values)
errors(y,yhat)

#remove Pha 
throw_model2<-lm(record_performance ~ . -PhA,throw_data)
summary(throw_model2)
anova(throw_model2)
vif(throw_model2) 
#training error 
yhat <- throw_model2$fitted.values
errors(y,yhat)


#remove BFM - best AIC so far
throw_model3<-lm(record_performance ~ Height+Age+Gender+Weight
                 +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model3)
anova(throw_model3)
vif(throw_model3) 
plot(throw_model3) 
#training error 
yhat <- throw_model3$fitted.values
errors(y,yhat)

#Remove Weight instead of BFM- WORSE Proceed with model 3 
throw_model35<-lm(record_performance ~ Height+Age+Gender+BFM
                 +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model35)
anova(throw_model35)
vif(throw_model35) 
#training error 
yhat <- throw_model35$fitted.values
errors(y,yhat)

# glm - log link - improved the linearity, not as much the hetero, 
# AIC not comparable (different dependent variable)
#training error lower :)
throw_model355<-glm(record_performance ~ Height+Age+Gender+Weight
                    +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,
                    family=gaussian(link = "log"),
                    throw_data)
summary(throw_model355)
throw_model355$aic
anova(throw_model355)
vif(throw_model355) 
plot(throw_model355)
# training error 
yhat <- throw_model355$fitted.values
errors(y,yhat)

# lm- log response - better for both linearity and hetero, can't compare AIC 
# training MSE comparable
throw_model3555<-lm(log(record_performance) ~ Height+Age+Gender+Weight
                    +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,
                    throw_data)
summary(throw_model3555)
anova(throw_model3555) 
vif(throw_model3555) 
plot(throw_model3555)
# training error
yhat <- exp(throw_model3555$fitted.values)
errors(y,yhat)

#remove Weight- hmm significantly worse R2 
throw_model4<-lm(record_performance ~ Height+Age+Gender
                 +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model4)
anova(throw_model4)
vif(throw_model4) 
#training error 
yhat <- throw_model4$fitted.values
errors(y,yhat)

# remove gender instead of Weight 
throw_model45<-lm(log(record_performance) ~ Height+Age+Weight
                 +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model45)
anova(throw_model45)
vif(throw_model45) 
#training error 
yhat <- exp(throw_model45$fitted.values)
errors(y,yhat)

# remove weight- R^2 much worse..
throw_model5<-lm(record_performance ~ Height+Age
                  +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model5)
anova(throw_model5)
vif(throw_model5) 
#training error 
yhat <- throw_model5$fitted.values
errors(y,yhat)

# remove hmin cmj - better than model 5
throw_model6<-lm(record_performance ~ Height+Age
                 +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,throw_data)
summary(throw_model6)
anova(throw_model6)
vif(throw_model6) 
#training error 
yhat <- throw_model6$fitted.values
errors(y,yhat)

# remove ftotmax hop
throw_model7<-lm(record_performance ~ Height+Age
                 +Event+PmaxTotRelCMJ,throw_data)
summary(throw_model7)
anova(throw_model7)
vif(throw_model7) 
AIC(throw_model7)
plot(throw_model7) # some heteroce.. maybe improved by log trans
plot(throw_model7$fitted.values,throw_data$record_performance)
#training error 
yhat <- throw_model7$fitted.values
errors(y,yhat)

# log transform 
throw_model75<-lm(log(record_performance) ~ Height+Age
                 +Event+PmaxTotRelCMJ,throw_data)
summary(throw_model75)
anova(throw_model75)
vif(throw_model75) 
AIC(throw_model75) 
plot(throw_model75) # much better for the heteroce..
plot(throw_model75$fitted.values,log(throw_data$record_performance))
#training error 
yhat <- exp(throw_model75$fitted.values)
errors(y,yhat)

# glm - log link 
throw_model755<-glm(record_performance ~ Height+Age
                  +Event+PmaxTotRelCMJ,
                  family=gaussian(link = "log"),
                  throw_data)
summary(throw_model755)
anova(throw_model755)
vif(throw_model755) 
AIC(throw_model755) 
plot(throw_model755)
#training error 
yhat <- throw_model755$fitted.values
errors(y,yhat)

# add back gender- no difference.. hm
throw_model8<-lm(record_performance ~ Height+Age+ Gender
                 +Event+PmaxTotRelCMJ,throw_data)
summary(throw_model8)
anova(throw_model8)
vif(throw_model8) 
AIC(throw_model8)
#training error 
yhat <- throw_model8$fitted.values
errors(y,yhat)

# add back Weight instead of Gender
throw_model85<-lm(record_performance ~ Height+Age+Weight
                 +Event+PmaxTotRelCMJ,throw_data)
summary(throw_model85)
anova(throw_model85)
vif(throw_model85) 
AIC(throw_model85)
#training error 
yhat <- throw_model85$fitted.values
errors(y,yhat)

# add back Gender- not really making a good difference. okay 
throw_model9<-lm(record_performance ~ Height+Age+Weight+Gender
                  +Event+PmaxTotRelCMJ,throw_data)
summary(throw_model9)
anova(throw_model9)
vif(throw_model9) 
AIC(throw_model9)
#training error 
yhat <- throw_model9$fitted.values
errors(y,yhat)

# REDUCED MODEL - only Height Weight Age and Gender and Event
throw_reduced<-lm(log(record_performance) ~ Height+Age+Gender+Weight
                +Event,throw_data)
summary(throw_reduced)
anova(throw_reduced)
vif(throw_reduced) 
#training error 
yhat <- exp(throw_reduced$fitted.values)
errors(y,yhat)

# F tests 

# compare to reduced model
anova(throw_reduced,throw_model3555) # significantly better
anova(throw_reduced,throw_model45) # significantly better
# compare model with gender and without 
anova(throw_model45,throw_model3555) 
# compare to full model
anova(throw_model3555,throw_model15) # final model better :) 

# Conclusion: stay with model 3 :), and decide between 
# the glm and log transform.. and also look at the CV's

# CV Error (est of test error) for Final Model throw_model3555 and throw_model355

k <- 5
mse<-c()
mae<-c()
for (i in c(1:k)) {
  #print(i)
  train_test <- CV_train_test(cv_throw, i, 
                x = c('Height','Age','Gender','Weight',
                    'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop'),
                y = 'record_performance')
  model<-glm(record_performance ~ Height+Age+Gender+Weight
                      +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,
                      family=gaussian(link = "log"),
                      train_test$train)
  yhat<-predict(model,newdata=train_test$x.test, type='response')
  print(length(yhat))
  print(length(unlist(train_test$y.test)))
  errs<-errors(unlist(train_test$y.test), yhat)
  mse[i]<-errs$mse
  mae[i]<-errs$mae
}
print(paste(k,'-fold CV mae error is',mean(mae))) #0.0593443060650768
print(paste(k,'-fold CV mse error is',mean(mse))) #0.00541680133926625

mse<-c()
mae<-c()
for (i in c(1:k)) {
  print(i)
  train_test <- CV_train_test(cv_throw, i, 
                              x = c('Height','Age','Gender','Weight',
                                    'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop'),
                              y = 'record_performance')
  model<-lm(log(record_performance) ~ Height+Age+Gender+Weight
            +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,
            train_test$train)
  yhat<-predict(model,newdata=train_test$x.test, type='response')
  print(length(yhat))
  print(length(unlist(train_test$y.test)))
  # make sure to transform predictions back to original scale for errors
  errs<-errors(unlist(train_test$y.test), exp(yhat))
  mse[i]<-errs$mse
  mae[i]<-errs$mae
  #mse[i]<-rmse(train_test$y.test, yhat)
}
print(paste(k,'-fold CV mae error is',mean(mae))) #0.0595236018018336
print(paste(k,'-fold CV mse error is',mean(mse))) #0.0055397543440208

