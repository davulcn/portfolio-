# CV_stratify function
# try to add stratum var and stratum name later to make it more flexible
CV_stratify <- function(data, k) {

  #split_data<-split(data, data$Event)
  stratum<-table(data$Event)
  cv_data<-data.frame()
  for (i in c(1:nrow(stratum))) {
    #print(names(stratum[i]))
    tmp<-data %>% filter(Event == names(stratum[i]))
    folds<-rep(c(1:k),floor(nrow(tmp)/k))
    #print(folds)
    # folds might not be an even number.. 
    # complete - randomly assign the remaining
    folds<-c(folds, sample(c(1:5),nrow(tmp)-length(folds)))
    #print(folds)
    #print(length(folds) == nrow(tmp))
    tmp$fold<-sample(folds,length(folds))
    cv_data<-bind_rows(cv_data,tmp)
    #folds<-c(folds,fold)
  }
  return(cv_data)
}

CV_train_test <- function(cv_data, i, x_vars, y_var) {

  index<-which(cv_data$fold==i)
    
  test<-cv_data[index,]
  x.test<- test %>% select(all_of(x_vars))
  y.test<- test %>% select(all_of(y_var))
    
  train<-cv_data[-index,]
  x.train<- train %>% select(all_of(x_vars))
  y.train<- train %>% select(all_of(y_var))
  
  train_test<- list(train, test, x.test, y.test, x.train, y.train)
  names(train_test) <- c('train','test','x.test','y.test','x.train','y.train')
  return(train_test)
}

# test run 
train_test <- CV_train_test(cv_throw, 1, 
                            c('Height','Age','Gender','Weight',
                                  'Event','PmaxTotRelCMJ','hminCMJ','Ftotmaxg_Hop'),
                            'record_performance')

errors <- function(y, yhat) {
    n <- length(yhat)
    n2 <- length(y)
    print(n == n2)
    #print(y-yhat)
    #MAE 
    mae <- (1/n) * (sum(abs(y-yhat)))
    #print(mae)
    #mse 
    mse <- (1/n) * (sum((y-yhat)^2))
    #print(mse)
    errs<-list(mae,mse)
    names(errs) <- c('mae','mse')
    return(errs)
}

rmse <- function(x1,x2) {
  return(sqrt(mean((x1-x2)^2)))
}

#Note: Using an external vector in selections is ambiguous.
#Use `all_of(x)` instead of `x` to silence this message.

#Note: Using an external vector in selections is ambiguous.
#ℹ Use `all_of(y)` instead of `y` to silence this message.

CV_lm <- function(cv_data,k,x,y) {
  n<-nrow(cv_data)
  mae<-c()
  mse<-c()
  
  f <- as.formula(
    paste(y, 
          paste(x, collapse = " + "), 
          sep = " ~ "))
  
  for (i in c(1:k)) {

    index<-which(cv_data$fold==i)
    
    test<-cv_data[index,]
    x.test<- test %>% select(x)
    y.test<- test %>% select(y)
    
    train<-cv_data[-index,]
    x.train<- train %>% select(x)
    y.train<- train %>% select(y)
    
    model<-lm(f, data=cv_data)
    yhat<-predict(model,newx=x.test,type='response')
    
    #MAE 
    mae[i] <- (1/n) * (sum(abs(y.test-yhat)))
    #mse 
    mse[i] <- (1/n) * (sum((y.test-yhat)^2))
  }
  print(paste(k,'-fold CV mae error is',mean(mae)))
  print(paste(k,'-fold CV mse error is',mean(mse)))
}

# specifications of how to model,
# coming from somewhere else
outcome <- "mpg"
variables <- c("cyl", "disp", "hp", "carb")

# our modeling effort, 
# fully parameterized!
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
print(f)
# mpg ~ cyl + disp + hp + carb

model <- lm(f, data = mtcars)
print(model)

