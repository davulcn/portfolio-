# MT1.7_exploratory_x
# Nurefsan Davulcu 

library(dplyr)
library(tidyr)
library(mice)
library(ggplot2)
library(reshape2)

# All predictors x 
x<- data_final %>% select(-c(
  'age_grade_performance',
  'record_performance',
  'Mark',
  'Mark_num',
  'EventParent',
  'ID',
  'Min',
  'Max',
  'MafsID',
  'BestEventSubj',
  'Event_factor'
))
nrow(x)
nrow(na.omit(x))
x<-na.omit(x)

num <- x %>% select(-c('Event','event_type','Gender'))
cor_mat <- cor(num)
# look at the high correlations
cor_mat[!lower.tri(cor_mat)] <- NA # remove diagonal and redundant values
filter_cor <- data.frame( cor_mat) %>%
  tibble::rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.7)

# some of the individual correlations
tmp<- data_final %>% select(c(hminCMJ,hmaxCMJ))
tmp <- na.omit(tmp)
cor(data_final$hminCMJ,data_final$hmaxCMJ)
tmp<- data_final %>% select(c(PmaxTotRelCMJ,hmaxCMJ))
tmp <- na.omit(tmp)
cor(data_final$PmaxTotRelCMJ,data_final$hmaxCMJ)

write.table(cormat,"tables/full_cor.csv")

# Subsetted predictors x - for the linear model
x<- data_final %>% select(c(
  
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

#missing data 
nrow(na.omit(x))
mispattern<-md.pattern(x)
missing.cols<-colnames(x)[colSums(is.na(x)) > 0]
missing.values<-colSums(is.na(x[,missing.cols]))
colSums(is.na(x[,missing.cols]))

# body impedance variables are missing together, hopping test is 
# missing together

x<-na.omit(x) #580, 86 all missing 

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

# FUNCTION FOR CORRELATION PLOT 
cor_plot<-function(cor_mat) {

  # Reorder the correlation matrix
  #cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  melted_cormat$value <- melted_cormat$value
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
}

cor_plot_features<-function(ggheatmap) {
  ggheatmap<-ggheatmap + 
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
                                 title.position = "top", title.hjust = 0.5)) 
    #ggtitle('Correlation Plot (Absolute values)')
    
  return(ggheatmap)
}

#Correlations
num <- x %>% select(-c('Event','event_type','Gender'))
cormat <- round(cor(num),2)
head(cormat)
corplot<-cor_plot(cormat)
corplot<-cor_plot_features(corplot)
print(corplot)
