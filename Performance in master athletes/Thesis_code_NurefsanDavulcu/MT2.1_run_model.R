# MT2.1: Running Model - Setup 
# Nurefsan Davulcu 
library(mice)
library(dplyr)
library(ggplot2)
library(reshape2)

run_data<-data_final %>% filter(event_type=='short track' | event_type=='long track') 
nrow(run_data)
run_data$Event<-factor(run_data$Event)

# check the record performance
temp<-age_grade_data %>% filter(event_type=='short track' | event_type=='long track')
y<- temp$record_performance
nrow(temp)

# Response- Raw
qplot(sample = Mark_num, data = run_data, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Performance for Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = Mark_num, data = run_data, color=Event)+ 
  labs(title='QQplot of Performance by Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Response - Percent of World Record 
hist(y)
qqnorm(y)
qqline(y, col = "steelblue", lwd = 2)

qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Percent of World Record for Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

qplot(sample = record_performance, data = temp, color=Event)+ 
  labs(title='QQplot of Percent of World Record by Running Events') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

# Variable selection
run_data<- run_data %>% select(c(
  
  'Mark_num',
  'Height',
  'Weight',
  'Age',
  'Gender',
  'Event',
  
  #From jump test:
  'Ptotmaxrel_CMJ',
  'PmaxTotRelCMJ',
  'hmaxCMJ',
  'hminCMJ',
  
  #From hopping test:
  'Ftotmaxg_Hop',
  'tContact_Hop',
  
  #Bioimpedance: 
  'BFM',
  'PhA',
  'ASMM' 
))


#missing data 
nrow(na.omit(run_data))
# will have to delete 33 observations
mispattern<-md.pattern(run_data)
missing.cols<-colnames(run_data)[colSums(is.na(run_data)) > 0]
missing.values<-colSums(is.na(run_data[,missing.cols]))
colSums(is.na(run_data[,missing.cols]))

run_data<-na.omit(run_data) #328

#Correlations
num <- run_data %>% select(-c('Event','Gender'))
cormat <- round(cor(num),2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
melted_cormat$value <- abs(melted_cormat$value)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  ggtitle('Correlation Plot (Absolute values)')

# Check the Event distribution - may perform stratified sampling for train/test split
table(run_data$Event)
