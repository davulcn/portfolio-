# MT1.6 Exploratory Analysis 
# Nurefsan Davulcu 
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)

### Response Variable ###

# raw marks .. remember this one is not all in the same units
y<-age_grade_data$Mark_num
hist(y)
# QQ plot by Event Type 
qplot(sample = Mark_num, data = age_grade_data, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance by Event Type') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# QQ Plot for just the running events
temp<-age_grade_data %>% filter(event_type=='short track' | event_type=='long track') 
qplot(sample = Mark_num, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance for Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = Mark_num, data = temp, color=Event)+ 
  labs(title='QQplot of Performance by Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Hurdles 
temp<-age_grade_data %>% filter(event_type=='hurdles') 
qplot(sample = Mark_num, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance for Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = Mark_num, data = temp, color=Event)+ 
  labs(title='QQplot of Performance by Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Jumps 
temp<-age_grade_data %>% filter(event_type=='jumps') 
qplot(sample = Mark_num, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance for Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = Mark_num, data = temp, color=Event)+ 
  labs(title='QQplot of Performance by Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Throws
temp<-age_grade_data %>% filter(event_type=='throws') 
qplot(sample = Mark_num, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance for Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = Mark_num, data = temp, color=Event)+ 
  labs(title='QQplot of Performance by Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Mark as percent of world record
hist(age_grade_data$record_performance)
# QQ plot by Event Type 
qplot(sample = record_performance, data = age_grade_data, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record by Event Type') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())
# other could be eliminated, it seems throws are a very different distribution 
# compared to the other events 

# Remove the other category (Decathlon, heptathlon, pentathlon)

temp<-age_grade_data %>% filter(event_type!='other') 
attach(temp)
hist(record_performance)
qqnorm(record_performance)
detach(temp)

hist(temp$record_performance)
qqnorm(temp$record_performance)
qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record by Event Type') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# QQ Plot for just the running events
temp<-age_grade_data %>% filter(event_type=='short track' | event_type=='long track') 
hist(temp$record_performance)
qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record for Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = record_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Percent of World Record by Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Hurdles 
temp<-age_grade_data %>% filter(event_type=='hurdles') 
hist(temp$record_performance)
qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record for Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = record_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Percent of World Record by Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Jumps 
temp<-age_grade_data %>% filter(event_type=='jumps') 
hist(temp$record_performance)
qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record for Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = record_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Percent of World Record by Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Throws
temp<-age_grade_data %>% filter(event_type=='throws') 
hist(temp$record_performance)
hist(log(temp$record_performance)) #pretty good
hist(1/(temp$record_performance)) #bad
hist(sqrt(temp$record_performance)) #no difference
# maybe try box cox

qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record for Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = record_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Percent of World Record by Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Mark as age grade performance

hist(age_grade_data$age_grade_performance)
qqnorm(age_grade_data$age_grade_performance)

# QQ plot by Event Type 
qplot(sample = age_grade_performance, data = age_grade_data, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance by Event Type') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# QQ Plot for just the running events
temp<-age_grade_data %>% filter(event_type=='short track' | event_type=='long track') 
qplot(sample = age_grade_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance for Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = age_grade_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Age Grade Performance of World Record by Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Hurdles 
temp<-age_grade_data %>% filter(event_type=='hurdles') 
qplot(sample = age_grade_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance for Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = age_grade_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Age Grade Performance by Hurdles') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Jumps 
temp<-age_grade_data %>% filter(event_type=='jumps') 
qplot(sample = age_grade_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance for Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = age_grade_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Age Grade Performance by Jumps') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Throws
temp<-age_grade_data %>% filter(event_type=='throws') 
qplot(sample = age_grade_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance for Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = age_grade_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Age Grade Performance by Throws') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())


