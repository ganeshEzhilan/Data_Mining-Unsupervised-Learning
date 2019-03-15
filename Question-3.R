#############################################################################
## The probelm is find the binary incidence matrix and apply apriori algorithm
## 
## Ganesh Ezhilan
## Created: February 15,2019
#############################################################################

install.packages("ggplot2")
install.packages("gridExtra")
install.packages("RColorBrewer")
install.packages("arules")


#############################################################################
## Installing packages
#############################################################################
library(MASS)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library("arules")
data("Boston")
head(Boston)
summary(Boston)
pairs(Boston)
#############################################################################
## Histograms for variables in dataset
#############################################################################
p1 <- ggplot(data = Boston, aes(x = medv)) + geom_histogram(stat = "bin")
p2 <- ggplot(data = Boston, aes(x = crim)) + geom_histogram()
p3 <- ggplot(data = Boston, aes(x = zn)) + geom_histogram()
p4 <- ggplot(data = Boston, aes(x = indus)) + geom_histogram()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(data = Boston, aes(x = chas)) + geom_histogram()
p6 <- ggplot(data = Boston, aes(x = nox)) + geom_histogram()
p7 <- ggplot(data = Boston, aes(x = rm)) + geom_histogram()
p8 <- ggplot(data = Boston, aes(x = age)) + geom_histogram()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(data = Boston, aes(x = dis)) + geom_histogram()
p10 <- ggplot(data = Boston, aes(x = rad)) + geom_histogram()
p11 <- ggplot(data = Boston, aes(x = tax)) + geom_histogram()
p12 <- ggplot(data = Boston, aes(x = ptratio)) + geom_histogram()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(data = Boston, aes(x = black)) + geom_histogram()
p14 <- ggplot(data = Boston, aes(x = lstat)) + geom_histogram()
grid.arrange(p13, p14, ncol=2)

bostonData = Boston
hist(Boston$crim)
bostonData$crim <-  ordered(cut(Boston$crim, c(0,5,90), labels = c("low",  "high")))
hist(Boston$indus)
bostonData$indus <- ordered(cut(Boston$indus, c(0,10,110), labels = c("low",  "high")))
hist(Boston$nox)
bostonData$nox <- ordered(cut(Boston$nox, c(0,0.6,2), labels = c("low",  "high")))
hist(Boston$rm)
bostonData$rm <-ordered(cut(Boston$rm, c(0,6,110), labels = c("low",  "high")))
hist(Boston$age)
bostonData$age <- ordered(cut(Boston$age, c(0,70,150), labels = c("low",  "high")))
hist(Boston$dis)
bostonData$dis <- ordered(cut(Boston$dis, c(0,4,20), labels = c("low",  "high")))
hist(Boston$rad)
bostonData$rad <- ordered(cut(Boston$rad, c(-1,6,40), labels = c("low",  "high")))
hist(Boston$tax)
bostonData$tax <- ordered(cut(Boston$tax, c(0,500,1110), labels = c("low",  "high")))
hist(Boston$ptratio)
bostonData$ptratio <-  ordered(cut(Boston$ptratio, c(0,19,40), labels = c("low",  "high")))
hist(Boston$lstat)
bostonData$lstat <- ordered(cut(Boston$lstat, c(0,15,60), labels = c("low",  "high")))
hist(Boston$medv)
bostonData$medv <- ordered(cut(Boston$medv, c(0,25,70), labels = c("low",  "high")))

hist(Boston$zn)
bostonData$zn <- NULL
hist(Boston$black)
bostonData$black <- NULL
hist(Boston$chas)
bostonData$chas <- NULL

#Convert to a binary incidence matrix
bostonData <- as(bostonData, "transactions")

#Creating the item frequncy plot
itemFrequencyPlot(bostonData,
                          col=brewer.pal(8,'Pastel2'),
                          support = 0.1, cex.names = 0.8,
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

# Apply the apriori algorithm
rules  <- apriori(bostonData,parameter =list(supp=0.01,conf=0.8,minlen=4))

lowcrim_rules<-subset(rules,subset=rhs %in% "crim=low" & lift>1.2)

inspect(head(sort(subset(lowcrim_rules,subset=lhs %in% "dis=low" & lift>1.1),by='confidence')))

rulesLowPTratio <- subset(rules, subset = rhs %in% "ptratio=low")
rulesLowPTratio

#inspect(lowpupil-teacher ratio)
inspect(head(sort(rulesLowPTratio, by = "lift"),n=3))
inspect(head(sort(rulesLowPTratio, by = "confidence"),n=3))
inspect(head(sort(rulesLowPTratio, by = "support"),n=3))

#############################################
## Multiple Regression Model
#############################################

#regression model. 
large_model <- lm(ptratio ~ ., data = Boston)
my_summary <- summary(large_model)

my_summary$coefficients
my_summary$adj.r.squared
