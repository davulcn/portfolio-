# MT1.2: Formatting the Performance Marks for Analysis
# Nurefsan Davulcu
library(dplyr)

# Look at events
events <- as.character(unique(join_data$Event)) #32 events
events<-trimws(events)
length(events)
# how many records of each event
table(join_data$Event)

# Group the Events
short_track<-c("100m","200m","400m","800m",
        "1500m","5000m",
        "5000m Race Walk")
long_track<-c("Half Marathon",
              "6 km Cross Country",
              "8 km Cross Country",
              "10 km Race Walk",
              "20 km Race Walk",
              "10 km Road Race (run)")
hurdles<-c("100m Hurdles","110m Hurdles","200m Hurdles","300m Hurdles",
           "400m Hurdles","80m Hurdles","2000m Steeplechase",
           "3000m Steeplechase")
throws<-c("Shot Put","Javelin Throw","Weight Throw","Discus Throw",
          "Hammer Throw","Throwing Pentathlon")
jumps<-c("Triple Jump","High Jump","Long Jump","Pole Vault")
other<-c("Decathlon","Heptathlon")

#ALL TIME EVENTS 

time_events <- c("300m Hurdles",  
"100m",
"400m",     
"80m Hurdles",                             
"1500m",
"800m",                                    
"Half Marathon",                         
"2000m Steeplechase",
"3000m Steeplechase",
"200m",                                 
"110m Hurdles",                         
"400m Hurdles",                            
"10 km Road Race (run)",                   
"5000m",                                 
"6 km Cross Country","8 km Cross Country",
"10 km Race Walk",                         
"5000m Race Walk",                       
"20 km Race Walk",                        
"100m Hurdles",                            
"200m Hurdles")

#Print all the events so see their units 
for (e in time_events)
{
  temp<-join_data[which(join_data$Event==e),]
  print(e)
  print(head(temp$Mark))
}

# these are in hours:minutes:seconds
long_time_events<-c("Half Marathon","10 km Road Race (run)",
                    "6 km Cross Country","8 km Cross Country",
                    "10 km Race Walk",
                    "20 km Race Walk")

# these are in minutes:seconds.milliseconds
short_time_events <- c("300m Hurdles", 
                 "100m",
                 "400m",     
                 "80m Hurdles",                             
                 "1500m",
                 "800m",                                    
                 "2000m Steeplechase",                      
                 "200m",                                 
                 "110m Hurdles",                         
                 "400m Hurdles",                            
                 "5000m",                                 
                 "5000m Race Walk",                       
                 "100m Hurdles",                            
                 "200m Hurdles",
                 "3000m Steeplechase")

# Make Event type column 
event<-join_data$Event
join_data$event_type<-ifelse(event %in% short_track, 'short track',
                   ifelse(event %in% long_track, 'long track',
                          ifelse(event %in% hurdles, 'hurdles',
                                 ifelse(event %in% throws, 'throws',
                                        ifelse(event %in% jumps, 'jumps',
                                               ifelse(event %in% other, 'other',NA))))))
join_data$event_type<-factor(join_data$event_type)
summary(join_data$event_type)

# FORMATTING TIME EVENT MARKS FOR CONSISTENCY AND PROCESSING 
# indices
lindex<-which(join_data$Event %in% long_time_events)
sindex<-which(join_data$Event %in% short_time_events)
# the rest - not a time event
rindex<-which(!(join_data$Event %in% time_events))
length(rindex) + length(lindex) + length(sindex) #good 

# long event times - these are in hours:minutes:seconds
ltime<-join_data[lindex,]$Mark
ltime
# add zero in the beginning for hours for times that are displaying
#in minutes:seconds, see this by looking at the first digit 
ltime_split<-lapply(strsplit(ltime,split=':'),as.numeric)

ltime_is_mins <- function(time) {
  if (time[1]>=4) {
    return(TRUE)
  }
  return(FALSE)
}

is_mins<-as.logical(lapply(ltime_split, ltime_is_mins))
ltime<-ifelse(is_mins,paste0('0:',ltime),ltime) 
ltime

# most times that were displaying in minutes:seconds have a :00 at the end 
# but not all, remove this 
# need to run below if we want to deal with the times in Excel ever
ltime<-ifelse(is_mins,gsub(":00$","",ltime),ltime)
ltime
# run below if we want the times in the R strptime format (so far not useful)
#ltime<-strptime(ltime, "%H:%M:%S")
#ltime # yay it works
#sum(is.na(ltime))
names(ltime)<-lindex
ltime

# short event times - these are in minutes:seconds.milliseconds
#my_options <- options(digits.secs = 2) #options to display milliseconds
stime<-join_data[sindex,]$Mark
stime
stime<-ifelse(grepl(pattern=":",stime),stime, paste0('0:',stime))
# run below if we want the times in the R strptime format (so far not useful)
#stime<-strptime(stime, "%M:%OS")
#stime # yay it works
#sum(is.na(stime))
#options(my_options) # set back the global options
names(stime)<-sindex
stime

# the rest - non time marks
rest<-join_data[rindex,]$Mark
names(rest)<-rindex

# Join all the marks back
mark<-c(ltime,stime,rest)
index<-c(lindex,sindex,rindex)
new_mark<-data.frame(mark,index)
new_mark <- new_mark[order(index),]
sum(sort(index)==new_mark$index)

# Add Formatted Mark column 
join_data$Mark<-new_mark$mark

# Mark Column in minutes 
stime_to_minutes <- function(time) {
  mins<-time[1]+(time[2]/60)
  return(mins)
}

ltime_to_minutes <- function(time) {
  mins<-(time[1]*60)+(time[2])+(time[3]/60)
  return(mins)
}

# convert short time to minutes
stime_mins<-lapply(strsplit(stime,split=':'),as.numeric)
head(stime_mins)
stime_mins<-lapply(stime_mins,stime_to_minutes)
stime_mins<-as.numeric(stime_mins)
head(stime_mins)
  
# convert long time to minutes 
ltime_mins<-lapply(strsplit(ltime,split=':'),as.numeric)
head(ltime_mins)
ltime_mins<-lapply(ltime_mins,ltime_to_minutes)
ltime_mins<-as.numeric(ltime_mins)
head(ltime_mins)

# join all the marks back
mark<-c(as.numeric(ltime_mins),as.numeric(stime_mins),as.numeric(rest))
index<-c(lindex,sindex,rindex)
new_mark<-data.frame(mark,index)
new_mark <- new_mark[order(index),]
sum(sort(index)==new_mark$index)

# Add Mark_num column - the times here are in minutes
join_data$Mark_num<-new_mark$mark
