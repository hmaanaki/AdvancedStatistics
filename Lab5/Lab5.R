
# Question 1A

setwd("C:\\Users\\hmaan\\Desktop\\Advanced statistics\\Lab5")
myT <- read.table("Lab5_data.txt",header=TRUE,row.names=1)

plot(log(myT$CumulativeCellDivisions), log(myT$Lifetime_cancer_risk), main = 'Log-Log Plot')

# Question 1B

linearModel = lm(log(myT$Lifetime_cancer_risk)~log(myT$CumulativeCellDivisions))
abline(linearModel)

#Question 1C

summary(linearModel)

  ## The p-value for the null hypothesis that the slope of the regression between
  ## these two variables is zero is 5.12e-8

  ## The R-squared value is 0.6463

# Question 1D
  
plot(linearModel)
  ## based on the plot of fitted values vs residuals, it appears that the
  ## variance is somewhat equal throughout (visually). We can also see visually
  ## from the previous plot in 1B

qqnorm(residuals(linearModel))
  ## Based on the qqnorm plot, the data overlaps the expected fit meaning
  ## that the data does follow a normal distribution

caseControlData <- read.table("caseControlData.txt",header=TRUE,sep = "\t")
bmiData <- read.table("bmiData.txt",header=TRUE,sep = "\t")

colnames(caseControlData)
# Adding in BMI to each row 
caseControlData$bmi <- rep(NA,nrow(caseControlData))
for(row in 1:nrow(caseControlData)){
  key <- sub("case", "", caseControlData$sample[row])
  key <- sub("control", "", key)
  key <- strsplit( key, "_")[[1]][1]
  caseControlData$bmi[row] = bmiData$bmi[match(key,bmiData$studyid)]
}

# Determining the p-value for each column
pValLinearReg <- vector()
caseControlDataValidBMI <- subset(caseControlData,bmi!="NA")
for(col in 3:ncol(caseControlDataValidBMI)-1){
  tempLinearModel <- lm(unlist(caseControlDataValidBMI[,col])~caseControlDataValidBMI$bmi)
  pValLinearReg[col] <- anova(tempLinearModel)$"Pr(>F)"[1]
}

hist(pValLinearReg)
plot(pValLinearReg)
  ## Looking at the histogram, they appear to be uniformly distributed
  ## Since the p-values are normally distributed, we can say that the null hypothesis
  ## that they are not related is true

# Adjusting the p-values with a 10% false discovery rate
pValLinearRegAdjusted <- p.adjust(pValLinearReg, method = "BH")

hist(pValLinearRegAdjusted, xlim = c(0,1))
plot(pValLinearRegAdjusted, xlim = c(0,1))
  
  ## Looking at the histogram of the adjusted p-values, it is clear that none
  ## of the associations are significant with a 10% false discovery rate

