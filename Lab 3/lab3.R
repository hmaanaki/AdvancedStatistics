rm(list = ls())
# (1A) 
x <- seq(0,1,0.01)
dexp(x,rate=5)/0.9932621

i = 1
dexp_vals <-vector()
for(val in x){
  dexp_vals[i] <- dexp(val,rate=5)/0.9932621
  i = i + 1
}
plot(x,dexp_vals)

# (1B)
pi_old <- 0.5

posterior_dist <- vector()
for(i in 1:100000){
  p_old <- (dexp(pi_old,rate=5)/0.993261)*dbinom(10,14, pi_old)
  pi_new <- pi_old + rnorm(1,0,sd=0.01)
  
  if(pi_new > 1)
      pi_new = 1;
  
  if(pi_new < 0)
      pi_new = 0;
  
  p_new <- (dexp(pi_new, rate=5)/0.993261) * dbinom(10, 14, pi_new)
  
  ratio <- p_new / p_old
  
  if( ratio > 1 || ratio >= runif(1))
      pi_old = pi_new
  
  posterior_dist[i] = pi_old
  
  if(i %% 100 == 0){
      my_hist <- hist(posterior_dist, breaks = 200, plot = FALSE)
      plot(my_hist$mids, my_hist$counts/i, main = paste("iteration", i), ylim = c(0, 0.02))
      dbetasum = sum(dbeta(my_hist$mids, 15,11))
      lines(my_hist$mids, dbeta(my_hist$mids, 15,11) / dbetasum, col = "blue")
      Sys.sleep(0.1)
      
      if(i == 20000){
        break
      }
  }
}

posterior_dist_grid <- vector()
x <- seq(0,1,0.01)

i<-1
sum <- 0
for( x_val in my_hist$mids){
  posterior_dist_grid[i] <- (dexp(x_val,rate = 5)/0.9932621) * dbinom(10, 14, x_val)
  sum = sum + posterior_dist_grid[i]
  i = i + 1
}

lines(my_hist$mids,posterior_dist_grid / sum, col = "red")

# (1C)

pi_old <- 0.5

posterior_dist <- vector()
for(i in 1:100000){
  p_old <- (dexp(pi_old,rate=5)/0.993261)*dbinom(417, 583, pi_old)
  pi_new <- pi_old + rnorm(1,0,sd=0.01)
  
  if(pi_new > 1)
    pi_new = 1;
  
  if(pi_new < 0)
    pi_new = 0;
  
  p_new <- (dexp(pi_new, rate=5)/0.993261) * dbinom(417,583, pi_new)
  
  ratio <- p_new / p_old
  
  if( ratio > 1 || ratio >= runif(1))
    pi_old = pi_new
  
  posterior_dist[i] = pi_old
  
  if(i %% 100 == 0){
    my_hist <- hist(posterior_dist, breaks = 200, plot = FALSE)
    plot(my_hist$mids, my_hist$counts/i, main = paste("iteration", i), ylim = c(0, 0.06),xlim = c(0.4,0.9))
    dbetasum = sum(dbeta(my_hist$mids, 584,417))
    lines(my_hist$mids, dbeta(my_hist$mids, 584, 417) / dbetasum, col = "blue")
    Sys.sleep(0.1)
    
    if(i == 20000){
      break
    }
  }
}

posterior_dist_grid <- vector()
x <- seq(0,1,0.01)

i<-1
sum <- 0
for( x_val in my_hist$mids){
  posterior_dist_grid[i] <- (dexp(x_val,rate = 5)/0.9932621) * dbinom(417,583, x_val)
  sum = sum + posterior_dist_grid[i]
  i = i + 1
}

lines(my_hist$mids,posterior_dist_grid / sum, col = "red")


