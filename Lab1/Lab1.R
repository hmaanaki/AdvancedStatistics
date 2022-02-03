
trials <- c(10, 100, 500, 1000, 5000, 10000, 25000, 50000, 75000, 100000)
means <- vector(mode ="double", length = length(trials))
variances <- vector(mode = "double", length = length(trials))

## Function for getting result from rolls
rollLoadedDie <- function(rolls){
  rolls_result <- vector(mode ="double", length = rolls)
  
  for(i in 1:rolls){
    # Stores the rolls with i
    rolls_result[i] <- sample(1:6,1,replace = TRUE,prob= c(rep(0.1,5), 0.5))
  }
  ## Shows the mean of the loaded die for a certain number of trials
  print(mean(rolls_result))
  ## shows the variance of the loaded die for a certain number of trials
  print(var(rolls_result))

  return(rolls_result)
}

## Generate histogram for a large number of rolls
## It does not seem to follow a uniform distribution since we can
## See that the frequency at 6 is much higher
rolls_result_large <- rollLoadedDie(100000)
hist(rolls_result_large)

## the mean and variance after a large number of rolls
for(i in 1:length(trials)){
  rolls_result <- rollLoadedDie(trials[i])
  # Contains all the means for all the trials
  means[i] <- mean(rolls_result)
  # Contrains all the variances for all the trials
  variances[i] <- var(rolls_result)
}

## Plots the trials versus the means and variance
## Based on the selected points it takes ~10,000 (sometimes up to 25000) trials to converge
windows()
plot(log10(trials), means)
lines(log10(trials), rep(4.498,length(trials)))
windows()
plot(log10(trials), variances)
lines(log10(trials), rep(3.264, length(trials)))
