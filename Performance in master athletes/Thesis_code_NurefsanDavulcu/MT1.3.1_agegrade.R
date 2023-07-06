# MT1.3: Constructing the Response Variable - Age Grade Performance
# Nurefsan Davulcu
library(dplyr)

#import the factors 
wav_women<-read.csv("WMA/wavacalc_women.csv",header = TRUE)
wav_men<-read.csv("WMA/wavacalc_men.csv",header = TRUE)
wav_women$Gender<-'F'
wav_women <- wav_women %>% select(-c('isRoad'))
wav_men$Gender<-'M'
wav<-rbind(wav_women,wav_men)
wav$Gender<-as.factor(wav$Gender)

#import the world records
records<-read.csv("WMA/records.csv", header=TRUE)
records$Gender<-as.factor(records$Gender)

# Convert marks in world records to minutes 

# indices
lindex<-which(records$Event %in% long_time_events)
sindex<-which(records$Event %in% short_time_events)
# the rest - not a time event
rindex<-which(!(records$Event %in% time_events))
length(rindex) + length(lindex) + length(sindex) #good 

ltime<-records[lindex,]$Mark
stime<-records[sindex,]$Mark
rest<-records[rindex,]$Mark

stime_mins<-lapply(strsplit(stime,split=':'),as.numeric)
stime_mins<-lapply(stime_mins,stime_to_minutes)
stime_mins<-as.numeric(stime_mins)
head(stime_mins)

# convert long time to minutes 
ltime_mins<-lapply(strsplit(ltime,split=':'),as.numeric)
ltime_mins<-lapply(ltime_mins,ltime_to_minutes)
ltime_mins<-as.numeric(ltime_mins)
head(ltime_mins)

# join them
mark<-c(as.numeric(ltime_mins),as.numeric(stime_mins),as.numeric(rest))
index<-c(lindex,sindex,rindex)
new_mark<-data.frame(mark,index)
new_mark <- new_mark[order(index),]
sum(sort(index)==new_mark$index)

# Add Mark_num column - the times here are in minutes  
records$Mark_num<-new_mark$mark

#import file to match the event names from data 
#to age grading factors (wav)
event_match<-read.csv("WMA/events.csv",na.strings="")
head(event_match)
event_match<-apply(event_match,2,trimws) #remove the trailing spaces 
event_match

# add event_factor to the main data file 
data_final<-merge(event_match,data_final,by='Event')

# Age Grade
age_grade_data <- data_final %>% select(c('MafsID','Event','Event_factor','Gender',
                                          'Mark','Mark_num','Age')) 
str(age_grade_data)
sum(is.na(age_grade_data))

factor<-c()
record<-c()
record_num<-c()

for (i in 1:nrow(age_grade_data)) {
  age<-age_grade_data$Age[i]
  event_factor<-age_grade_data$Event_factor[i]
  event<-age_grade_data$Event[i]
  gender<-age_grade_data$Gender[i]
  
  if (event_factor %in% wav$Event & gender %in% wav$Gender) {
    factor[i]<-wav[which(wav$Event==event_factor & wav$Gender==gender),age]
  } else {
    factor[i]<-NA
  }
  
  record[i]<-records[which(records$Event==event & records$Gender==gender),]$Mark
  record_num[i]<-records[which(records$Event==event & records$Gender==gender),]$Mark_num
}
length(factor)
length(record)
length(record_num)
nrow(age_grade_data)
summary(factor) #The NA'S are for Decathlon and Heptathlon
sum(is.na(record)) #The NA's are for throwing pentathlon and 6km cross country

#remove(age_grade_data)
age_grade_data<-cbind(age_grade_data,factor,record,record_num)

## NEED TO FIX HERE 
# Calculate percent of world record- Different for time and distance events
attach(age_grade_data)
record_performance <-ifelse(Event %in% time_events,
                      record_num/Mark_num,
                      Mark_num/record_num)
detach(age_grade_data)
# check percent of world record 
sum(is.na(record_performance)) # this is for throwing pentathlon and 6 km cross country
sum(na.omit(record_performance>1))
sum(na.omit(record_performance<0))
hist(record_performance)

# Add percent of world record to age grade df
age_grade_data$record_performance<-record_performance

# Calculate age grade performance - Different for time and distance events
attach(age_grade_data)
age_grade_performance <-ifelse(Event %in% time_events,
                               record_num/(Mark_num*factor),
                               (Mark_num*factor)/record_num)
detach(age_grade_data)

# check age grade performance
sum(is.na(age_grade_performance)) 
# this is for missing factors in decathlon, hept, 
# and missing world record forthrowing pentathlon and 6 km cross country
sum(na.omit(age_grade_performance>1))
sum(na.omit(age_grade_performance<0))
hist(age_grade_performance) # higher compared to % record, makes sense since we have accounted for the older ages

# Add it
age_grade_data$age_grade_performance<-age_grade_performance

# add event type 
event<-age_grade_data$Event
age_grade_data$event_type<-ifelse(event %in% short_track, 'short track',
                             ifelse(event %in% long_track, 'long track',
                                    ifelse(event %in% hurdles, 'hurdles',
                                           ifelse(event %in% throws, 'throws',
                                                  ifelse(event %in% jumps, 'jumps',
                                                         ifelse(event %in% other, 'other',NA))))))
age_grade_data$event_type<-factor(age_grade_data$event_type)

head(age_grade_data) 

# Add record_performance and age_grade_performance to main data 
add <- age_grade_data %>% select(c('MafsID','Event','record_performance','age_grade_performance'))
nrow(data_final) #666
nrow(add) #666
nrow(merge(data_final,add,by=c('MafsID','Event'))) # 666- good
data_final<-merge(data_final,add,by=c('MafsID','Event'))


