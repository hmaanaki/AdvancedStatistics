## Lab 4 ## Hussian Maanaki ## Hmaanaki@uncc.edu

# Question 1
setwd("C:\\Users\\hmaanaki\\Desktop\\Advanced Statistics\\Lab 4")
myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)

# Question 2
plot(log(myT$D2_01), log(myT$D2_02), main = 'Log-Log Plot')

# It appears that the two biological replicates have similar 
# patterns of gene expression as seen by the plot

# Question 3
geneMeans <- rowMeans(myT)
geneVariances <- apply(myT,1,var)

plot(log(geneMeans),log(geneVariances), main = 'Log-Log Plot')

# Based on the plot it appears that the mean does not equal
# the variance. It appears that the variance is larger than the mean

# Question 4

geneCountD201 <- myT['NC101_00003', 'D2_01']
geneTotalCountD201 <- sum(myT$D2_01)
geneCountD202 <- myT['NC101_00003', 'D2_02']
geneTotalCountD202 <- sum(myT$D2_02)

contTable <- matrix(c(geneCountD201,geneTotalCountD201-geneCountD201
                      ,geneCountD202,geneTotalCountD202-geneCountD202), ncol = 2)
colnames(contTable) <- c('Seq in D2_01','Seq in D2_02')
rownames(contTable) <- c('Assigned to NC101_00003', ' Not assigned to NC101_00003')

contTable

fisher.test(contTable, alternative = "two.sided")$p.value

# Question 5

allPValues <- vector()

for(row in 1:nrow(myT)){
  gene <- myT[row,1:2]
  contTableVariable <- matrix(c(gene$D2_01, geneTotalCountD201-gene$D2_01,
                                gene$D2_02, geneTotalCountD202-gene$D202), ncol = 2)
  
  allPValues[row] <- fisher.test(contTableVariable, alternative = "two.sided")$p.value
  rm(contTableVariable)
}

hist(allPValues,breaks = 20)
# Appears that the p-values are more significant than
# we would expect under the uniform distribution

# Removing low abundance genes
allPValuesHighAbundance <- vector()
myTHighAbundanceGenes <- myT[(myT$D2_01 + myT$D2_02 > 50),]

geneCountD201 <- myTHighAbundanceGenes['NC101_00003', 'D2_01']
geneTotalCountD201 <- sum(myTHighAbundanceGenes$D2_01)
geneCountD202 <- myTHighAbundanceGenes['NC101_00003', 'D2_02']
geneTotalCountD202 <- sum(myTHighAbundanceGenes$D2_02)

for(row in 1:nrow(myTHighAbundanceGenes)){
  gene <- myTHighAbundanceGenes[row,1:2]
  contTableVariable <- matrix(c(gene$D2_01, geneTotalCountD201-gene$D2_01,
                                gene$D2_02, geneTotalCountD202-gene$D202), ncol = 2)
  contTableVariable
  allPValuesHighAbundance[row] <- fisher.test(contTableVariable, alternative = "two.sided")$p.value
  rm(contTableVariable)
}

hist(allPValuesHighAbundance,breaks = 20)

# Appears that all of the genes were more significant than expected under the unifrm distribution still 

# Question 6
myT_add_1 <- myT + 1

geneCountD201 <- myT_add_1['NC101_00003', 'D2_01']
geneTotalCountD201 <- sum(myT_add_1$D2_01)
geneCountD202 <- myT_add_1['NC101_00003', 'D2_02']
geneTotalCountD202 <- sum(myT_add_1$D2_02)

expected_freq_NC101_00003D202 <- geneCountD201/geneTotalCountD201
poisson.test(geneCountD202, expected_freq_NC101_00003D202,alternative = "two.sided")

# Question 7

results_poisson <- vector()
myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)

for(row in 1:nrow(myT)){
  gene <- myT_add_1[row,1:2]
  expected_freq_D201 <- gene[1,1] / geneTotalCountD201
  counts_D202 <- gene[1,2]
  
  results_poisson[row] <- poisson.test(counts_D202, expected_freq_D201, alternative = "two.sided")$p.value
}

plot(log(results_poisson), log(allPValues), main = 'Log-Log Plot')

# They appear to correlate very well but are not exactly equal
