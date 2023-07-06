library(dplyr)

# Part 1: Cleaning 
# Nurefsan Davulcu

#file locations - change if different 
file1<-paste0("data/Mafs18_Demo_Sport_CMJ_BioImp_Marks_4Neffy_v0.11.csv")
file2<-paste0("data/Mafs18_Demo_Sport_Leo_BioImp_Marks_4Neffy_v1.00.csv")
#read data 
data1<-read.csv(file1,na.strings = '#N/A',stringsAsFactors = FALSE)
data2<-read.csv(file2,na.strings = '#N/A', stringsAsFactors = FALSE)
str(data1)
str(data2)

# number of subjects
subjects<-unique(data1$MafsID)
subjects2<-unique(data2$MafsID)
length(subjects) #232 subjects 
length(subjects2) #233 subjects 

# see which subjects mismatch
summary(subjects %in% subjects2)
summary(subjects2 %in% subjects)
subjects2[!(subjects2 %in% subjects)]
# 050mafs18 not in data1
# remove it from data2
data2<-data2[-which(data2$MafsID=='050mafs18'),]
  
# REMOVE MISSING SUBJECT ROWS - 4 
sum(is.na(data1$MafsID))
sum(is.na(data2$MafsID))
data1<-data1[!is.na(data1$MafsID),]
data2<-data2[!is.na(data2$MafsID),]

#datasets with only columns in common
# merge data1 and data2 
dupcol=c('MafsID',
         'Height',
         'Weight',
         'Gender',
         'BestEventSubj',
         'ASMM',
         'Event',
         'EventParent',
         'Mark'
)

keep1 <- data1 %>% select(dupcol)
keep2 <- data2 %>% select(dupcol)

# checks - consistency between the data - all seem to be good 
summary(sort(keep1$MafsID)==sort(keep2$MafsID))
summary(sort(keep1$Height)==sort(keep2$Height))
summary(sort(keep1$Weight)==sort(keep2$Weight))
summary(sort(keep1$Gender)==sort(keep2$Gender))
summary(sort(keep1$BestEventSubj)==sort(keep2$BestEventSubj)) 
summary(sort(keep1$ASMM)==sort(keep2$ASMM)) 
summary(sort(keep1$Event)==sort(keep2$Event))
summary(sort(keep1$EventParent)==sort(keep2$EventParent)) 
summary(sort(keep1$Mark)==sort(keep2$Mark)) 

#ASM_per_Height2 and ASMM_per_Height2 are the same
summary(sort(data1$ASM_per_Height2)==sort(data2$ASMM_per_Height2))

join_data<-merge(data1, data2, by=dupcol)
#there's duplicate rows (true duplicates - same for every variable)
join_data<-unique(join_data)
nrow(join_data)
#check - good, there was 75 duplicate rows 
nrow(unique(data1))
nrow(unique(data2))

# add obs_ID column (useful later)
join_data$ID<-c(1:nrow(join_data))
str(join_data)

# "6 km Cross Country or 8 km Cross Country" is ambigious - change 
# only one of the observations was for 6km
index <- which(join_data$Event=='6 km Cross Country or 8 km Cross Country')
join_data[index,]$Event<-'8 km Cross Country'
index <- which(join_data$MafsID=='071mafs18' & join_data$Mark=='42:32:00')
join_data[index,]$Event<-'6 km Cross Country'

# Change some of the variable types 
str(join_data)

join_data$Gender<-as.factor(join_data$IDGENDER)
join_data$BestEventSubj<-as.factor(join_data$BestEventSubj)
join_data$Event<-as.factor(join_data$Event)
join_data$EventParent<-as.factor(join_data$EventParent)
join_data <- join_data %>% select(-c('IDGENDER','AgeExact','ASM_per_Height2'))

# output file location - change if different 
out_file<-paste0("data/Mafs18_Demo_Sport_V1.0.csv")
# write to csv
write.csv(join_data,out_file)

#final 810 rows
rm(list=setdiff(ls(), "join_data"))
