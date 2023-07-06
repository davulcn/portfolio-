# MT1.3_clustering: Declustering the data

x<-join_data %>% select(c('MafsID', 'Event'))
nrow(unique(x,by='MafsID')) 
#Declustered data should have 666 obs, 144 repeated events by the same subject to eliminate

# choose best performance per person per event  
# for time events - minimum
# for distance events - maximum

y <- join_data %>%
  group_by(MafsID, Event) %>%
  summarise(
    Min = min(Mark_num), #.groups='drop'
    Max = max(Mark_num)
  ) 

y$Mark_num<-ifelse(y$Event %in% time_events, y$Min, y$Max)
sum(is.na(y$Mark_num)) # check - good 

data_final<-merge(y, join_data, by=c('Mark_num','MafsID','Event'))

