
library(caret)

Learning_curve <- function(X,y, stepsize) {
  
  sampleSize <- dim(X)[1]
  sampling <- sample(c(1,0), sampleSize, replace=TRUE,prob=c(0.7,0.3))
  
  X_train <- X[sampling == 1,]
  y_train <- y[sampling == 1]
    
  X_test <- X[sampling == 0,]
  y_test <- y[sampling == 0]
  
  
  
  
 for (step in seq(stepsize, dim(X_train)[1], stepsize)) {
   
   X_train_sub <- X_train[1:step,]
   y_train_sub <- y_train[1:step]
   
   theta <- solve(t(X_train_sub) %*% X_train_sub) %*% t(X_train_sub) %*% y_train_sub
   
   h_train <- X_train_sub %*% theta
   
   RMSE <- sqrt(sum((h_train - y_train_sub)^2) / step)  # step = sample size
   
   
   
 }
  
}