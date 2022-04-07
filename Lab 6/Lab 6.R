
setwd("C:\\Users\\hmaanaki\\Desktop\\Advanced Statistics\\Lab 6")
myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)

myT <- myT[ apply(myT, 1, median) > 5,]

myTNorm <- myT
for( i in 1:ncol(myT)){
  colSum = sum(myT[,i])
  myTNorm[,i] = myTNorm[,i]/colSum
}

# Question A

factors <- c(rep("Day 2",3), 
              rep("Week 12", 3), 
              rep("Week 18", 5))
factors <-factor(factors)

pValsA <- vector()
for(i in 1:nrow(myTNorm)){
  myData <- as.numeric(myTNorm[i,])
  myChangingLm <- lm(myData ~ factors, x=TRUE)
  pValsA[i] <- anova(myChangingLm)$"Pr(>F)"[1]
}

hist(pValsA)
pValsAAdjusted <- p.adjust(pValsA, method = "BH")
hist(pValsAAdjusted)
pValsASortedAdjusted <- sort(pValsAAdjusted, decreasing = FALSE)

count = 0
for(j in 1:length(pValsASortedAdjusted)){
  if(pValsASortedAdjusted[j] < 0.05){
    count = count + 1
  }
  else{
    break
  }
}

# There are 612 significant genes with a BH FDR-corrected 0.05 threshold
count

# Question B

factorsNumeric <- c(rep(2,3), rep(86, 3), rep(128, 5))
factorsNumeric <- as.numeric(factor(factorsNumeric))

pValsRegression <- vector()
for(i in 1: nrow(myTNorm)){
  myData <- as.numeric(myTNorm[i,])
  myChangingLm <- lm(myData ~ factorsNumeric, x=TRUE)
  pValsRegression[i] <- anova(myChangingLm)$"Pr(>F)"[1]
}

hist(pValsRegression)
pValsRegressionAdjusted <- p.adjust(pValsRegression, method = "BH")
hist(pValsRegressionAdjusted)
pValsRegressionF <- sort(pValsRegressionAdjusted, decreasing = FALSE)

count = 0
for(k in 1:length(pValsASortedAdjusted)){
  if(pValsRegressionF[k] < 0.05){
    count = count + 1
  }
  else{
    break
  }
}

# There are 737 significant genes with a BH FDR-corrected 0.05 threshold
count

# Question C
pfVals <- vector()
for(i in 1: nrow(myTNorm)){
  myData <- as.numeric(myTNorm[i,])
  myChangingLmFull <- lm(myData ~ factors, x=TRUE)
  myChangingLmReduced <- lm(myData ~ factorsNumeric, x=TRUE)
  
  fullResiduals <- sum(residuals(myChangingLmFull) ^ 2)
  reducedResiduals <-sum(residuals(myChangingLmReduced) ^ 2)
  
  f = ((reducedResiduals - fullResiduals) /(9-8) ) / (fullResiduals/8)
  pfVals[i] <- pf(f, 1, 8, lower.tail = FALSE)
}

hist(pfVals)

pfValsAdjusted <- p.adjust(pfVals, method = "BH")
pfValsSorted <- sort(pfValsAdjusted, decreasing = FALSE)

count = 0
for(l in 1:length(pfValsSorted)){
  if(pfValsSorted[l] < 0.05){
    count = count + 1
  }
  else{
    break
  }
}

# At a threshold of 0.05, ther is a significant difference between 30 genes for
# the two models

count

# Question D

index <- 1:3983

myFrame <- data.frame(index, pValsAAdjusted,
                     pValsRegressionAdjusted, pfValsAdjusted)


  # Plot for A
myFrame <- myFrame[order(myFrame$pValsAAdjusted),]
boxplot(as.numeric( myTNorm[myFrame$index[1],]) ~ factors)

  # Plot for B
myFrame <- myFrame[order(myFrame$pValsRegressionAdjusted),]
boxplot(as.numeric( myTNorm[myFrame$index[1],]) ~ factorsNumeric)
abline(lm(as.numeric(myTNorm[myFrame$index[1],]) ~ factorsNumeric))       

  # Plot for C
myFrame <- myFrame[order(myFrame$pfValsAdjusted),]
boxplot(as.numeric( myTNorm[myFrame$index[1],]) ~ factors)

# Question E

# Looking at the box plots, it is clear that the two-parameter
# model is ideal for this data since it shows a proper relationship for the
# most significant gene. Additionally, since we have time-series data, it 
# would be more logical to run the linear model using the days as numeric leading
# to a two-parameter model.Using three-parameter model's seems like it would be more ideal
# for data that doesn't contain time-series (or numeric chaging) data (or if you are not looking at changes
# over time).
