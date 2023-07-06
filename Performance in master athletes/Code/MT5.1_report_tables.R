# Report Tables and plots
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

tmp <- model_data
tmp$event_type<-as.character(tmp$event_type)
tmp$model_type <- ifelse(tmp$event_type %in% c('short track',
                                                  'long track',
                                                  'hurdles'),
                                'run',
                                tmp$event_type)
tmp$model_type <- factor(tmp$model_type)

# Event Table
event_summary<- tmp %>% count(model_type,Event)
write.csv(event_summary,'tables/event_summary.csv')

# descriptive stats for reporting 
tmp <- tmp %>% filter(model_type != 'other') %>%
  select(-c(Gender,Event,event_type))

mean_table<-t(aggregate(. ~ model_type, data = tmp, mean))
sd_table<-t(aggregate(. ~ model_type, data = tmp, sd))
write.csv(mean_table,'tables/mean_table.csv')
write.csv(sd_table,'tables/sd_table.csv')

#paste0(mean_throw,' \u00b1 ',sd(hi))

# 1. Age grade perf and record perf hist 
png("plots/report_plots/figure_1.png",width=1000)
par(mfrow=c(1,2))
hist(age_grade_data$record_performance,main='% of World Record Performance',
     xlab='',col='light blue')
hist(age_grade_data$age_grade_performance,main='Age Grade Performance',
     xlab='',col='light blue')
dev.off()

# 2. Age grade perf and record perf by event 

# to make common legend 
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

temp<-age_grade_data %>% filter(event_type!='other') 
# QQ plot by Event Type 
p1<-qplot(sample = age_grade_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of Age Grade Performance\nby Event Type') +
  theme(legend.position = "none")
  #theme(legend.position=c(0.9,0.2),legend.title=element_blank())
# QQ plot by Event Type 
p2<-qplot(sample = record_performance, data = temp, color=event_type, shape=event_type)+ 
  labs(title='QQplot of % of World Record Performance\nby Event Type') +
  theme(legend.position=c(0.9,0.2),legend.title=element_blank())

#legend <- get_legend(p2)
#p2 <- p2 + theme(legend.position = "none")

png("plots/report_plots/figure_2.png",width=1300,600,res=150)
grid.arrange(p1, p2, ncol=2)
#grid.arrange(p1, p2, legend,ncol=2, layout_matrix = rbind(c(1,2), c(3,3)))
dev.off()

# 4.	Correlation plots per model 
num <- run_data %>% select(-c('Event','Gender','event_type'))
cormat <- round(cor(num),2)
head(cormat)
corplot1<-cor_plot(cormat)
corplot1<-cor_plot_features(corplot1)
corplot1<-corplot1 + theme(#axis.text.x = element_blank(),
                           legend.position = "none",
                           plot.title = element_text(hjust = 0.5)) +
          ggtitle('Running-Hurdle')
print(corplot1)

num <- jump_data %>% select(-c('Event','Gender','event_type'))
cormat <- round(cor(num),2)
head(cormat)
corplot2<-cor_plot(cormat)
corplot2<-cor_plot_features(corplot2)
corplot2<-corplot2 + theme(#axis.text.x = element_blank(), 
                           legend.position = "none", 
                           plot.title = element_text(hjust = 0.5)) +
          ggtitle('Jumping')
print(corplot2)

num <- throw_data %>% select(-c('Event','Gender'))
cormat <- round(cor(num),2)
head(cormat)
corplot3<-cor_plot(cormat)
corplot3<-cor_plot_features(corplot3) +ggtitle('Throwing') +
          theme(plot.title = element_text(hjust = 0.5))
print(corplot3)

# two by two grid - need to add all axis titles back 
png("plots/report_plots/figure_4.png",width=1500, height=1500, res=150)
grid.arrange(corplot1,corplot2,corplot3, nrow=2,ncol=2)
dev.off()

# seperate plots 
png("plots/report_plots/figure_4a.png",width=1000,height=816,res=150)
print(corplot1)
dev.off() 
png("plots/report_plots/figure_4b.png",width=1000,height=816,res=150)
print(corplot2)
dev.off() 
png("plots/report_plots/figure_4c.png",width=1000,height=816,res=150)
print(corplot3)
dev.off()  


# 3 rows, 1 column 
png("plots/report_plots/figure_4.5.png",width=1000, height=1400, res=200)
grid.arrange(corplot1,corplot2,corplot3, nrow=3,ncol=1)
dev.off()

# 5.	6 PLOTS: Full data and by event for running, throw and jump
# could also do this with facets.. 
# Running-Hurdle
p1 <- ggplot(run_data, aes(x=record_performance)) + geom_histogram(bins=10,
                                                               color='black',
                                                               fill='white') +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())
p2 <- qplot(sample = record_performance, data = run_data, color=Event)+ 
      theme(legend.position = "none")

# Jumps 
p3 <- ggplot(jump_data, aes(x=record_performance)) + geom_histogram(bins=10,
                                                               color='black',
                                                               fill='white') +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())
p4 <- qplot(sample = record_performance, data = jump_data, color=Event)+ 
      theme(legend.position = "none")

# Throws
p5 <- ggplot(throw_data, aes(x=record_performance)) + geom_histogram(bins=10,
                                                               color='black',
                                                               fill='white') +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())
p6 <- qplot(sample = record_performance, data = throw_data, color=Event)+ 
      theme(legend.position = "none")

png("plots/report_plots/figure_5.png",width=1000, height=1400, res=200)
grid.arrange(arrangeGrob(p1, p2, top = 'Running-Hurdle',nrow=1,ncol=2),
             arrangeGrob(p3, p4, top = 'Jumping',nrow=1,ncol=2),
             arrangeGrob(p5, p6, top = 'Throwing',nrow=1,ncol=2),
             nrow=3, ncol=1,
             top = textGrob("% of World Record Performance",gp=gpar(fontsize=16)))
dev.off()
