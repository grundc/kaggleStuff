# LinReg_GradientDesc

### Debugging (with theta = 0)
# X_debug <- matrix(c(1,1,1,1,5,2,4,5), nrow=4, ncol=2)
# y_debug <- matrix(c(1,6,4,2))
# LinReg_GradientDesc(X_debug,y_debug,0.01,0,1000,0.0001)
### Result
#### [,1]
#### [1,]  5.2147549
#### [2,] -0.5733459

LinReg_GradientDesc <- function(X, y, adjust,lambda, maxRepeats, minCostDiff)  {
      
     
      
      # Setting variable
      m <- dim(X)[1]
      
      # Setting the initial thetas all to ones
      theta <- as.matrix(rep(0,dim(X)[2]))
      
      h0 <- X %*% theta
      cost_old <- 1/ (2*m) * sum((h0 - y)^2)
      cost_hist <- as.matrix(cost_old)
      
      for (i in 1:maxRepeats) {
            
            
            grad <- adjust * 1/m * (t(X) %*% (h0 - y))
            
            # print(paste("grad = ",grad))
            
            theta <- theta - grad
            
            h0 <- X %*% theta
            
            cost_new <- 1/ (2*m) * sum((h0 - y)^2)
            cost_hist <- rbind(cost_hist, cost_new)
            
            # print(paste("Cost old = ", cost_old))
            # print(paste("Cost new = ", cost_new))
            
            if ( (cost_old - cost_new) < minCostDiff ) {
                  print(paste("Reached minCostDiff after iterations = ",i))
                  break
            }
            
            cost_old <- cost_new
           
      }
      
      return(theta)
      # setClass(Class="GradDesc",
      #          representation(
      #                theta="matrix",
      #                costs="matrix"
      #          )
      # )
      # return(new("GradDesc",
      #       theta = theta,
      #       costs = cost_hist
      # ))
      
}




