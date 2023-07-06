# MT3.1 Throw Model - Setup 
# Nurefsan Davulcu 

throw_data<-model_data %>% filter(event_type=='throws')
throw_data <- throw_data %>% select(-c('event_type'))
throw_data$Event<-factor(throw_data$Event)
nrow(throw_data)
throw_data<-na.omit(throw_data) #29 missing , 97 obs 
nrow(throw_data) #126-97
y <- throw_data$record_performance
hist(y)
hist(log(temp$record_performance)) # seems it will be the best transformation
table(throw_data$Event)
table(throw_data$Gender)
hist(throw_data$Age)

# Look at correlations for the throw data 
#Correlations
num <- throw_data %>% select(-c('Event','Gender'))
cormat <- round(cor(num),2)
head(cormat)
corplot<-cor_plot(cormat)
corplot<-cor_plot_features(corplot)
print(corplot)

# Will not use seperate train and test for this since there 
# is not enough data - CV instead at the end 
set.seed(123)
cv_throw<-CV_stratify(data = throw_data, k = 5)

