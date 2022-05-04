
library(nlme)
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\hmaanaki\\Desktop\\Advanced Statistics\\Lab 7")

myT <- read.table("prePostPhylum.txt", header = TRUE, sep="\t")
numCols <- ncol(myT)
myColClasses <- c(rep("character",4), rep("numeric", numCols-4))
myT <-read.table("prePostPhylum.txt",header=TRUE,sep="\t",colClasses=myColClasses)

myTData<-myT[,5:10]

myPCOA <- princomp(myTData)

pc1 <- myPCOA$scores[,1]
pc2 <- myPCOA$scores[,2]

## Question 2
uniqueGenotype = unique(myT$genotype)
uniqueCage = unique(myT$cage)
uniqueTimepoint = unique(myT$time)

# Genotype
plot(pc1, pc2, col=ifelse(myT$genotype == uniqueGenotype[1], "red", "black"))
# Cage
plot(pc1, pc2, col= case_when(myT$cage == uniqueCage[1] ~ 1,
                              myT$cage == uniqueCage[2] ~ 2,
                              myT$cage == uniqueCage[3] ~ 3,
                              myT$cage == uniqueCage[4] ~ 4,
                              myT$cage == uniqueCage[5] ~ 5,
                              myT$cage == uniqueCage[6] ~ 6,
                              myT$cage == uniqueCage[7] ~ 7,
                              myT$cage == uniqueCage[8] ~ 8,
                              myT$cage == uniqueCage[9] ~ 9,
                              myT$cage == uniqueCage[10] ~ 10,
                              myT$cage == uniqueCage[11] ~ 11))
# Time
plot(pc1, pc2, col=ifelse(myT$time == uniqueTimepoint[1], "red", "black"))

## Question 3

# Anova
cageFactors = factor(myT$cage)
cagePc1_lm <- lm(pc1 ~ myT$cage, x = "true") 
cagePc2_lm <- lm(pc2 ~ myT$cage, x = "true")

pval_cage_pc1 <- anova(cagePc1_lm)$"Pr(>F)"[1]
pval_cage_pc2 <- anova(cagePc2_lm)$"Pr(>F)"[1]

# t-test genotype
myT["pc1"] <- pc1
myT["pc2"] <- pc2
wt_genotype_pc1 = myT[(myT$genotype == "WT"),]$pc1
mutant_genotype_pc1 = myT[(myT$genotype == "10-/-"),]$pc1

wt_genotype_pc2 = myT[(myT$genotype == "WT"),]$pc2
mutant_genotype_pc2 = myT[(myT$genotype == "10-/-"),]$pc2

pval_genotype_pc1 <- t.test(wt_genotype_pc1, mutant_genotype_pc1, paired = FALSE)$p.value
pval_genotype_pc2 <- t.test(wt_genotype_pc2, mutant_genotype_pc2, paired = FALSE)$p.value

# t-test time
pre_time_pc1 = myT[(myT$time == "PRE"),]$pc1
post_time_pc1 = myT[(myT$time == "POST"),]$pc1

pre_time_pc2 = myT[(myT$time == "PRE"),]$pc2
post_time_pc2 = myT[(myT$time == "POST"),]$pc2

pval_time_pc1 <- t.test(pre_time_pc1, post_time_pc1, paired = FALSE)$p.value
pval_time_pc2 <- t.test(pre_time_pc2, post_time_pc2, paired = FALSE)$p.value


# Question 4
par(mfrow=c(3,2)) 
myT4 <- myT[myT$time == "POST",]
# boxplot of each phylo against the cage
cage <- c(t(myT4[2]))
cageFactors <- factor(cage)
for(i in 7:ncol(myT4)-2){
  boxplot(myT4[,i] ~ cageFactors, title(colnames(myT4)[i-1]))
}

# Question 4 (B)
par(mfrow=c(1,1))
cage <- myT$cage
genotype <- myT$genotype

pValsMixed <- vector()
corrCoeffMixed <-vector()
index = 1
for(j in 5:10){
  bug <- myT[,j]
  tempFrame <- data.frame(bug, cage, genotype)
  
  tempMixedModel <- lme(bug ~ genotype, method = "REML", random = ~1 | cage, data = tempFrame)
  pValsMixed[index] <- anova(tempMixedModel)$"p-value"[2]
  tempGLSModel <- gls(bug~genotype, method ="REML", correlation = corCompSymm(form = ~1 | cage), data = tempFrame)
  corrCoeffMixed[index] <- coef(tempGLSModel$modelStruct[1]$corStruct,unconstrained=FALSE)[[1]]
  index = index + 1
}

barplot(pValsMixed ~ colnames(myT4)[5:10])
y = c(rep(0.1,10))
lines(0:9,y, lty = 2, col = 2)

# creating a dataframe with the phylas, p-values and correlation coefficients

mixedModelResults <- data.frame(pValsMixed, corrCoeffMixed)
row.names(mixedModelResults) <- colnames(myT4)[5:10]
