getwd()
library(ggplot2)
library(cowplot)
setwd("F:/STUDY MATERIAL/R PROGRAMMING/Bank Loan Prediction/Task 3")
getwd()
attach(Book1)
str(Book1)
data<-Book1
str(data)
## Now add factors for variables that are factors
data$ID <- as.factor(data$ID)
data$ID
data$Age<-as.factor(data$Age)
data$Experience<-as.factor(data$Experience)
data$Income<-as.factor(data$Income)
data$`ZIP Code`<-as.factor(data$`ZIP Code`)
data$Family<-as.factor(data$Family)
data$CCAvg<-as.factor(data$CCAvg)
data$Education<-as.factor(data$Education)
data$Mortgage<-as.factor(data$Mortgage)
data$`Personal Loan`<-as.factor(data$`Personal Loan`)
data$`Securities Account`<-as.factor(data$`Securities Account`)
data$`CD Account`<-as.factor(data$`CD Account`)
data$Online<-as.factor(data$Online)
data$CreditCard<-as.factor(data$CreditCard)
#####################################
##
## Now we can do some quality control by making sure all of the factor
## levels are represented by people with and without personal loan
xtabs(~ `Personal Loan` + Age, data=data)
xtabs(~ `Personal Loan` + Experience, data=data)
xtabs(~ `Personal Loan` + Income, data=data)
xtabs(~ `Personal Loan` + Family, data=data)
xtabs(~ `Personal Loan` + CCAvg, data=data)
xtabs(~ `Personal Loan` + Education, data=data)
xtabs(~ `Personal Loan` + Mortgage, data=data)
xtabs(~ `Personal Loan` + `Securities Account`, data=data)
xtabs(~ `Personal Loan` + data$`CD Account` , data=data)
xtabs(~ `Personal Loan` + CreditCard, data=data)
## Now do the actual logistic regression
##
###########
logistic <- glm(`Personal Loan` ~ Age, data=data, family="binomial")
summary(logistic)
## Now we will use all of the data available to predict loans
##
#####################################
logistic1 <- glm(`Personal Loan` ~ Experience, data=data, family="binomial")
summary(logistic1)
logistic2 <- glm(`Personal Loan` ~ Income, data=data, family="binomial")
summary(logistic2)
logistic3 <- glm(`Personal Loan` ~ Family, data=data, family="binomial")
summary(logistic3)
logistic4 <- glm(`Personal Loan` ~ CCAvg, data=data, family="binomial")
summary(logistic4)
logistic5 <- glm(`Personal Loan` ~ Education, data=data, family="binomial")
summary(logistic5)
logistic6 <- glm(`Personal Loan` ~ Mortgage, data=data, family="binomial")
summary(logistic6)
logistic7 <- glm(`Personal Loan` ~ `Securities Account`, data=data, family="binomial")
summary(logistic7)
logistic8 <- glm(`Personal Loan` ~ data$`CD Account`, data=data, family="binomial")
summary(logistic8)
logistic9 <- glm(`Personal Loan` ~ Online, data=data, family="binomial")
summary(logistic9)
logistic10 <- glm(`Personal Loan` ~ CreditCard, data=data, family="binomial")
summary(logistic10)

## now we can plot the data
predicted.data <- data.frame(
  probability.of.pel=logistic10$fitted.values,
  pel=data$`Personal Loan`)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.pel, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## prob of loan and color by whether or not they actually want loan
window()
ggplot(data=predicted.data, aes(x=rank, y=probability.of.pel)) +
  geom_point(aes(color=pel), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of loan by credit card")

library(nnet)
model<-multinom(`Personal Loan`~ Income+ Family+CreditCard,data=data)
summary(model)
##############################
##decision tree
library(party)
set.seed(1234)
pd<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train<-data[pd==1,]
validate<-data[pd==2,]
tree<-ctree(`Personal Loan`~Income+Family,data=data)
tree
plot(tree)