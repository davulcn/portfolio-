# Model Summaries for Reporting 

# quick model summary functions 
rsq <- function(model) {
  print(summary(model)$adj.r.squared*100)
}

se <- function(model) {
  round(print(summary(model)$sigma),3)
}

pvalues <- function(model) {
  p <- summary(model)$coefficients[,4]
  #p_indic<-cut(p, breaks = c(0.001,0.01,0.05,0.1),right=FALSE)
      #labels=c('****','***','**','*',''),right=FALSE)
  p_indic<- ifelse(p < 0.001, '****', 
         ifelse( p < 0.01, '***', 
                 ifelse(p < 0.05, '**',
                        ifelse(p<0.1, '*', ''))))
  return(p_indic)
}

coef_table <- function(model) {
  coef<-round(coefficients(model),3)
  names<-names(coef)
  # add on significance indicators 
  coef<-paste0(coef,pvalues(model))
  cint<-round(confint(model),3)
  cint2<-paste0("[",cint[,1],", ",cint[,2],"]")
  table<-cbind(names,coef,cint2)
  colnames(table)<-c('Variable','Coefficient','95% CI')
  return(table)
}

anova(run_model35)
anova(jump_model5)
anova(throw_model3555)

rsq(run_model35)
rsq(jump_model5)
rsq(throw_model3555)

se(run_model35)
se(jump_model5)
se(throw_model3555)

run_coef<-coef_table(run_model35)
jump_coef<-coef_table(jump_model5)
throw_coef<-coef_table(throw_model3555)
# also add on the original scales for throw model
#orig_coef<-round(exp(coefficients(throw_model3555)),3)
#orig_cint<-confint(throw_model3555)
#orig_cint2<-paste0("[",round(exp(orig_cint[,1]),3),", "
#                   ,round(exp(orig_cint[,2]),3),"]")
#throw_coef<-cbind(throw_coef,orig_coef,orig_cint2)

write.csv(run_coef,"tables/run_coef.csv")
write.csv(jump_coef,"tables/jump_coef.csv")
write.csv(throw_coef,"tables/throw_coef.csv"

round(AIC(run_model35))
round(AIC(jump_model5))
round(AIC(throw_model3555))

# training errors 
yhat <- run_model35$fitted.values
errors(y.train,yhat)
y<- jump_data$record_performance
yhat <- jump_model5$fitted.values
errors(y,yhat)
y<- throw_data$record_performance
yhat <- exp(throw_model3555$fitted.values)
errors(y,yhat)

# diagnostic plots 
png("plots/report_plots/figure_10a.png",width=700)
par(mfrow=c(2,2))
plot(run_model35)
dev.off()
png("plots/report_plots/figure_10b.png",width=700)
par(mfrow=c(2,2))
plot(jump_model5)
dev.off()
png("plots/report_plots/figure_10c.png",width=700)
par(mfrow=c(2,2))
plot(throw_model3555)
dev.off()



