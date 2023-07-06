# This was to generate a clean records file, and fill in missing 
# world records 

wav_women<-read.csv("WMA/wavacalc_women.csv",header = TRUE)
wav_men<-read.csv("WMA/wavacalc_men.csv",header = TRUE)
wav_women$Gender<-'F'
wav_men$Gender<-'M'
wav<-rbind(wav_women,wav_men)
wav$Gender<-as.factor(wav$Gender)

#import the world records
records_women<-read.csv("WMA/Weltrekorde_Frauen_iaaf.csv", header=TRUE)
records_women$Gender<-'F'
records_women <- records_women %>% select(c('Event','Gender','Mark'))
for (e in events) {
  if (!(e %in% records_women$Event)) {
    temp<-c(e,'F',NA)
    records_women<-rbind(records_women,temp)
  }
}
head(records_women)
nrow(records_women)

records_men<-read.csv("WMA/Weltrekorde_Maenner_iaaf.csv", header=TRUE)
records_men$Gender<-'M'
records_men <- records_men %>% select(c('Event','Gender','Mark'))
for (e in events) {
  if (!(e %in% records_men$Event)) {
    temp<-c(e,'M',NA)
    records_men<-rbind(records_men,temp)
  }
}
head(records_men)
nrow(records_men)

# Join the records together
records<-rbind(records_women,records_men)
records$Gender<-as.factor(records$Gender)

head(records)
str(records)

# Add time units to make sure it will match
units<-ifelse(records$Event %in% long_time_events,'hh:mm:ss',
              ifelse(records$Event %in% short_time_events,'mm:ss:ms','non-time'))
records$units<-units

out_file<-paste0("data/records_incomplete.csv")
write.csv(records,out_file)

# fixed the records in Excel and saved as 'records.csv' to be 
# used in MT1.3_age.grade.R