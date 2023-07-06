# MT 2.0 Modelling Setup 
# Nurefsan Davulcu 

model_data<- data_final %>% select(c(
  'record_performance',
  'Height',
  'Weight',
  'Age',
  'Gender',
  'Event',
  'event_type',
  
  #From jump test:
  'PmaxTotRelCMJ',
  'hminCMJ',
  
  #From hopping test:
  'Ftotmaxg_Hop',
  
  #Bioimpedance: 
  'BFM',
  'PhA'
))

# event to factor 
model_data$Event <- factor(model_data$Event)
  
# remove missing data 
nrow(model_data)
nrow(na.omit(model_data)) # 99 observations will be removed 
model_data <- na.omit(model_data) 
nrow(model_data)

