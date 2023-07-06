#MT4.2 Jump Model
# Nurefsan Davulcu 
library(dplyr)
library(car)
library(MASS)

jump_data <- jump_data %>% select(-c('event_type'))
y <- jump_data$record_performance

jump_model1<-lm(record_performance ~ .,jump_data)
summary(jump_model1)
anova(jump_model1)
vif(jump_model1) 
AIC(jump_model1)
plot(jump_model1)
#training error 
yhat <- jump_model1$fitted.values
errors(y,yhat)

# remove BFM
jump_model2<-lm(record_performance ~ .-BFM,jump_data)
summary(jump_model2)
anova(jump_model2)
vif(jump_model2) 
AIC(jump_model2)
plot(jump_model2)
#training error 
yhat <- jump_model2$fitted.values
errors(y,yhat)

# remove PHA 
jump_model3<-lm(record_performance ~ Height+Age+Gender+Weight
                +Event+PmaxTotRelCMJ+hminCMJ+Ftotmaxg_Hop,jump_data)
summary(jump_model3)
anova(jump_model3)
vif(jump_model3) 
AIC(jump_model3)
plot(jump_model3)
#training error 
yhat <- jump_model3$fitted.values
errors(y,yhat)

# remove hminCMJ 
jump_model4<-lm(record_performance ~ Height+Age+Gender+Weight
                +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,jump_data)
summary(jump_model4)
anova(jump_model4)
vif(jump_model4) 
AIC(jump_model4)
plot(jump_model4)
#training error 
yhat <- jump_model4$fitted.values
errors(y,yhat)

# remove Gender 
jump_model5<-lm(record_performance ~ Height+Age+Weight
                +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,jump_data)
summary(jump_model5)
anova(jump_model5)
vif(jump_model5) 
AIC(jump_model5)
plot(jump_model5)
yhat <- jump_model5$fitted.values
errors(y,yhat)

# test with f test 
anova(jump_model5,jump_model4) # not significantly better with gender

# REDUCED MODEL - only Height Weight Age and Gender and Event
jump_reduced<-lm(record_performance ~ Height+Age+Gender+Weight
                  +Event,jump_data)
summary(jump_reduced)
anova(jump_reduced)
vif(jump_reduced) 
AIC(jump_reduced)
#training error 
yhat <- jump_reduced$fitted.values
errors(y,yhat)

# chosen model vs reduced f test 
anova(jump_reduced,jump_model5) # final model significantly better 0.0001
# full model vs chosen model f test 
anova(jump_model5,jump_model1) # not significant compared to full model hm

# improve the model assumptions for jump_model5 
# some pattern in the residuals
res <- resid(jump_model5)
#Height,Age,Weight
#Event+PmaxTotRelCMJ+Ftotmaxg_Hop
par(mfrow = c(2, 3))
plot(jump_data$Height,res)
plot(jump_data$Age,res)
plot(jump_data$Weight,res)
plot(jump_data$Event,res)
plot(jump_data$PmaxTotRelCMJ,res)
plot(jump_data$Ftotmaxg_Hop,res)

# age seems to be the problematic one hm. 
par(mfrow = c(1, 1))
plot(log(jump_data$Age),res)

# transformations - squared of response worked the best 
# out of log, inverse, and sqrt for
# residuals but messed up the normality and lowered other things
jump_model55<-lm(record_performance^2 ~ Height+Age+Weight
                +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,jump_data)
summary(jump_model55)
anova(jump_model55)
vif(jump_model55) 
AIC(jump_model55)
plot(jump_model55)

res <- resid(jump_model55)
plot(jump_data$Age,res)

# transforming age not really doing anything 
jump_model555<-lm(record_performance ~ Height+Age^2+Weight
                 +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,jump_data)
summary(jump_model555)
anova(jump_model555)
vif(jump_model555) 
AIC(jump_model555)
plot(jump_model555)

# BOX COX 
bc <- boxcox(jump_model5)
# close to 1 so not a transformation

# BEST PROCEED WITH JUMP MODEL 5- CV ERROR 
#jump_model5<-lm(record_performance ~ Height+Age+Weight
#                +Event+PmaxTotRelCMJ+Ftotmaxg_Hop,jump_data)
k <- 5
CV_lm(jump_cv,k,x = c('Height','Age','Weight',
      'Event','PmaxTotRelCMJ','Ftotmaxg_Hop'),
      y = 'record_performance')
# [1] "5 -fold CV mae error is 0.030235804212059"
# [1] "5 -fold CV mse error is 0.00705887869112635"
