# LinReg_GradientDesc

LinReg_GradientDesc <- function(X, y, adjust,lambda, maxRepeats, minCostDiff)  {
      
     
      
      # Setting variable
      m <- dim(X)[1]
      
      # Setting the initial thetas all to ones
      theta <- as.matrix(rep(1,dim(X)[2]))
      
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




