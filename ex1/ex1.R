library(moments)
library(lsr)

#Question 1
#A
N = 3
MEAN = 70
SD = 10

NormalDistributionByN = function(N, MEAN, SD, showHist=FALSE){
  # Create a sample of 50 numbers which are normally distributed.
  sampleVector <- rnorm(N, MEAN, SD)
  
  # Plot the histogram for this sample.
  if(showHist) {
    hist(sampleVector, main = "Normal DIstribution")
  }
  return (sampleVector)
}

samples = NormalDistributionByN(N, MEAN, SD, TRUE)
print(mean(samples))
print(sd(samples))

#B
means = c()
sds = c()
allSamples = c()
N_Values = c(3, 10, 50, 100, 250, 500)
for (n in N_Values) {
  current = NormalDistributionByN(n, MEAN, SD,TRUE)
  allSamples = c(allSamples, current)
  means = c(means, mean(current))
  sds = c(sds, sd(current))
}
print(means)
print(sds)
#C
plot(N_Values, means)
abline(h = mean(allSamples), col = "darkgreen")


#D
plot(N_Values, sds)
abline(h = sd(allSamples), col = "darkgreen")

#E


#Question 2

#A
sigmaPartMinusMean = function(X, powerValue) {
  returnValue = 0
  for (variable in X) {
    returnValue = (returnValue + (variable - mean(X)) ^ 2)
  }
  return(returnValue)
}
statistics = function(X) {
  upValue = ((1/length(X)) * sigmaPartMinusMean(X, 3))
  downValue = (sqrt(((1/length(X)) * sigmaPartMinusMean(X, 2))))^3
  return((upValue / downValue))
}

#B
getStatisticTenK = function(n, m, sd) {
  allValues = c()
  for (i in 1:10000) {
    allValues = c(allValues, statistics(NormalDistributionByN(n, m, sd)))
  }
  return(allValues)
}

N = 50
oneValue = getStatisticTenK(N, MEAN, SD)
print(mean(oneValue))
print(var(oneValue))

means = c()
vars = c()

#C
allValues = c()
for (n in seq(50, 500, by=50)) {
  currentValues = getStatisticTenK(n, MEAN, SD)
  allValues = c(allValues, currentValues)
  means = c(means, mean(currentValues))
  vars = c(vars, var(currentValues))
}
print(means)
print(vars)

getSkew = function(X, u, q){
  return(mean(((X - u) / q) ^ 3))
}

#D
plot(seq(50, 500, by=50), means, main = "Mean vs N value")
abline(h = getSkew(X=0, u=MEAN, q=SD), col = "darkgreen")
print(getSkew(X=0, u=MEAN, q=SD))

#E
plot(seq(50, 500, by=50), vars, main = "Var vs N value")

#Question 3

getNonBiasEstimator <- function(X) {
  firstPart = (1/(length(X)-1))
  secondPart = sigmaPartMinusMean(X, 2)
  return(sqrt(firstPart * secondPart))
}

getTDist = function(X, MEAN) {
  return((mean(X)- MEAN) / (getNonBiasEstimator(X) / sqrt(length(X))))
}

#A
N = 70
MEAN = 120
SD = 8

#B
allValues = c()
for (i in 1:10000) {
  samples = rnorm(N, MEAN, SD)
  allValues = c(allValues, getTDist(samples, MEAN))
}

#C
hist(allValues, main = "Histograms of all t values")

#D
x = seq(-6, 6, length = 10000)
curve(dt(x, df=N-1), from=-6, to=6)

#Question 4

#A
qqnorm(allValues)

#B
abline(0, 1, col = 'red')


#Question 5

#B
N = 3
MEAN = 70
SD = 2

samples = rnorm(N, MEAN, SD)
quantile(samples, probs = c(0.05 / 2, 1 - (0.05 / 2)))

#C
ic_sub = c()
N_Values = c(3, 10, 50, 100, 250, 500)
for (variable in N_Values) {
  ic = unname(quantile(rnorm(variable, MEAN, SD), probs = c(0.05 / 2, 1 - (0.05 / 2))))
  ic_sub = c(ic_sub, ic[2] - ic[1])
}
plot(N_Values, ic_sub)

N = 50
ic_sub = c()
Mean_Values = c(70, 100, 250, 500, 760)
for (variable in Mean_Values) {
  ic = unname(quantile(rnorm(N, variable, SD), probs = c(0.05 / 2, 1 - (0.05 / 2))))
  ic_sub = c(ic_sub, ic[2] - ic[1])
}
plot(Mean_Values, ic_sub)


MEAN = 70
SD_Values = c(2, 4, 6, 8, 10)
allValues = c()
for (variable in SD_Values) {
  ic = unname(quantile(rnorm(N, MEAN, variable), probs = c(0.05 / 2, 1 - (0.05 / 2))))
  allValues = c(allValues, ic[2] - ic[1])}
plot(SD_Values, allValues)


#Question 6

#Aנ
setwd('C:/Repos/Study/ResearchMethodsInCognition/ex1')
df = read.csv('neglect_data.csv')
df = na.omit(df)
colnames(df) = c("I", "Subject", "TrialNumber", "Length", "Location")

#B
df$LeftPart = df$Location / df$Length

#C 
ics = c()
for(subject in c(5, 21, 13, 7)){
  currentSubjectDf = df[df$Subject == subject,]
  ics = c(ics, quantile(currentSubjectDf$LeftPart, probs = c(0.05 / 2, 1 - (0.05 / 2))))
}
print(ics)
#D
leftPartMean = c()
for(subject in unique(df$Subject)){
  currentSubjectDf = df[df$Subject == subject,]
  leftPartMean = c(leftPartMean, mean(currentSubjectDf$LeftPart))
}
print(leftPartMean)

#E
quantile(leftPartMean, probs = c(0.05 / 2, 1 - (0.05 / 2)))


#F
tScore = getTDist(df$LeftPart, 0.55)

#G
pt(tScore, nrow(df) - 1, lower.tail = FALSE) 

#H
t.test(df$LeftPart, mu = 0.55)

#I
cohensD(df$LeftPart, mu = 0.55)





  
