# MT4.1 Jump Model - Setup 
# Nurefsan Davulcu 
library(dplyr)

jump_data<-model_data %>% filter(event_type=='jumps')
jump_data$Event<-factor(jump_data$Event)
nrow(jump_data)
jump_data<-na.omit(jump_data) # eliminated 10 obs
nrow(jump_data) 
y <- jump_data$record_performance
summary(y)
var(y)
hist(y) # good!
table(jump_data$Event)
table(jump_data$Gender)
hist(jump_data$Age)
hist(log(jump_data$Age))

plot(jump_data$Age,jump_data$record_performance)

# Will not use seperate train and test for this since there 
# is not enough data - CV instead at the end 
set.seed(123)
jump_cv<-CV_stratify(data = jump_data, k = 5)
# checks - good 
nrow(jump_cv)==nrow(jump_data)
sum(is.na(jump_cv$fold))
table(jump_cv$fold) 
