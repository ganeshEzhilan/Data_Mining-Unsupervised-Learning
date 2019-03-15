#############################################################################
## The probelm is to cluster demographic data using a classification tree
## Ganesh Ezhilan
## Created: February 15,2019
#############################################################################
install.packages("ElemStatLearn")
install.packages("rattle")
install.packages("ggplot2")

library("ElemStatLearn")
library(rpart)
library(rattle)
library(ggplot2)
library(plyr)
library(ggplot2)
library(tidyr)


marketingData = marketing
marketingData['Target'] = 1 

apply(marketingData, 2, function(x) any(is.na(x)))

summary(marketingData)
marketingData$Marital[is.na(marketingData$Marital)] <- median(marketingData$Marital, na.rm=TRUE)
marketingData$Edu[is.na(marketingData$Edu)] <- median(marketingData$Edu, na.rm=TRUE)
marketingData$Occupation[is.na(marketingData$Occupation)] <- median(marketingData$Occupation, na.rm=TRUE)
marketingData$Lived[is.na(marketingData$Lived)] <- median(marketingData$Lived, na.rm=TRUE)
marketingData$Household[is.na(marketingData$Household)] <- median(marketingData$Household, na.rm=TRUE)
marketingData$Status[is.na(marketingData$Status)] <- median(marketingData$Status, na.rm=TRUE)
marketingData$Home_Type[is.na(marketingData$Home_Type)] <- median(marketingData$Home_Type, na.rm=TRUE)
marketingData$Ethnic[is.na(marketingData$Ethnic)] <- median(marketingData$Ethnic, na.rm=TRUE)
marketingData$Language[is.na(marketingData$Language)] <- median(marketingData$Language, na.rm=TRUE)

apply(marketingData, 2, function(x) any(is.na(x)))
sapply(marketingData, class)

data.frame(min=sapply(marketingData,min),max=sapply(marketingData,max))
dim(marketingData)


marketingData1 <- data.frame(matrix(ncol = 15, nrow = 8993))
marketingData1['Income'] <- sample(1:9, 8993, replace = TRUE)
marketingData1['Sex'] <- sample(1:2, 8993, replace = TRUE)
marketingData1['Marital'] <-  sample(1:5, 8993, replace = TRUE)
marketingData1['Age'] <-sample(1:7, 8993, replace = TRUE)
marketingData1['Edu'] <- sample(1:6, 8993, replace = TRUE)
marketingData1['Occupation'] <- sample(1:9, 8993, replace = TRUE)
marketingData1['Lived'] <- sample(1:5, 8993, replace = TRUE)
marketingData1['Dual_Income'] <-  sample(1:3, 8993, replace = TRUE)
marketingData1['Household'] <- sample(1:9, 8993, replace = TRUE)
marketingData1['Householdu18'] <- sample(0:9, 8993, replace = TRUE)
marketingData1['Status'] <- sample(1:3, 8993, replace = TRUE)
marketingData1['Home_Type'] <- sample(1:5, 8993, replace = TRUE)
marketingData1['Ethnic'] <- sample(1:8, 8993, replace = TRUE)
marketingData1['Language'] <- sample(1:3, 8993, replace = TRUE)
marketingData1['Target'] <- 0
marketingData2 <- marketingData1[, -c(1:15)]

final <- rbind(marketingData, marketingData2)

#for (i in 1:15){
 # final[,i]<-as.factor(final[,i])
#}

final$Target = as.factor(as.character(final$Target))

model <- rpart(Target~., final)
windows()

summary(model)
pred = predict(model, final[,-c(15)])
plot(model)
text(model,use.n = T,all =T,cex =1)
pred

path.rpart(model, 15)

ggplot(gather(marketing), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
    