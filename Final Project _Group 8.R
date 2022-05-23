
install.packages('reticulate')
install.packages('pre')
install.packages('akima')
install.packages('beeswarm')
install.packages("psych")
install.packages('caTools')
library('dplyr')
library('ggplot2')
library('MASS')
library('Hmisc')
library("pre")
library('graphics')
library(reticulate)
library(CatterPlots)
require(lattice)
require(ggplot2)
library(beeswarm)
library(psych)
df <- read.csv("Bank_Personal_Loan_Modelling.csv")
df
head(df)
summary(df)

#data Pre-Processing
print("Total Number of rows :")
nrow(df)
print("Total Number of colunms :")
ncol(df)

#reordering the Personal loan column
df <- df[c(1,2,3,4,5,6,7,8,9,11,12,13,14,10)]
df

# information of datatypes
str(df)

#checking for null values
sapply(df,function(x)sum(is.na(x)))

#removing the rows with negative experience
df <- df[df$Experience >= 0,]
nrow(df)

#summary of the data
summary(df)


##Data distribution in each attribute 

#For ID
x<-df$ID
h1<-hist(x, breaks=10, col="red", xlab="ID",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Age
x<-df$Age
h2<-hist(x, breaks=10, col="red", xlab="Age", main="Histogram with Normal Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Experience
x<-df$Experience
h3<-hist(x, breaks=10, col="red", xlab="Experience",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Income
x<-df$Income
h4<-hist(x, breaks=10, col="red", xlab="Income",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For ZIP.Code
x<-df$ZIP.Code
h5<-hist(x, breaks=10, col="red", xlab="ZIP Code",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Family
x<-df$Family 
h6<-hist(x, breaks=10, col="red", xlab="Family",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For CCAvg
x<-df$CCAvg
h7<-hist(x, breaks=10, col="red", xlab="CCAvg",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Education
x<-df$Education
h8<-hist(x, breaks=10, col="red", xlab="Education",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Mortgage
x<-df$Mortgage 
h9<-hist(x, breaks=10, col="red", xlab="Mortgage",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Personal.Loan
x<-df$Personal.Loan
h0<-hist(x, breaks=10, col="red", xlab="Personal Loan",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Securities Account
x<-df$Securities.Account
h11<-hist(x, breaks=10, col="red", xlab="Securities Account",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For CD.Account
x<-df$CD.Account
h12<-hist(x, breaks=10, col="red", xlab="CD Account",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Online
x<-df$Online
h13<-hist(x, breaks=10, col="red", xlab="Online",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#For Credit Card
x<-df$CreditCard
h14<-hist(x, breaks=10, col="red", xlab="Credit Card",main="Histogram with Density Curve",prob= TRUE)
lines(density(x), col="blue", lwd=2)
lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)


#split the data

library(caTools)

split <- sample.split(df$Personal.Loan, SplitRatio = 0.70)

#get training and test data
bank.train <- subset(df, split == TRUE)
bank.train
bank.test <- subset(df, split == FALSE)
bank.test

# factoring of categorical variables
x<-c('Family', 'Education','Securities.Account', 'CD.Account', 'Online','CreditCard','Personal.Loan')
df[x] <- lapply(df[x], factor)
summary(df)

bank.train[x] <- lapply(bank.train[x], factor)
bank.test[x] <- lapply(bank.test[x], factor)


#Building models

#logistic regression model

set.seed(1000)
model =glm(Personal.Loan~.,data=bank.train,family="binomial")
summary(model)

model2=glm(Personal.Loan~ Income+Family+CCAvg+Education+Securities.Account+CD.Account+Online+CreditCard,data=bank.train,family="binomial")
summary(model2)

model3=glm(Personal.Loan~Income+Family+Education+CD.Account+CreditCard+CCAvg,data=bank.train,family="binomial")
summary(model3)

#better AIC value is for model3 with less factors affecting it.

exp(coef(model3))

#prediction on training data
glm.probs <- predict(model3,newdata=bank.train,type = 'response')

#confusion matrix
table(bank.train$Personal.Loan,glm.probs > 0.5)

#accuracy on training data
Accuracy <- (3092 + 228)/nrow(bank.train)*100

print(Accuracy)

#prediction on test data
glm.probs2 <- predict(model3,newdata = bank.test, type = 'response')

#confusion matrix
table(bank.test$Personal.Loan,glm.probs2 > 0.5)

#accuracy on test set
Accuracy <- (1324 + 93)/nrow(bank.test)*100

print(Accuracy)

#plotting ROCR curves.

install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(glm.probs2, bank.test$Personal.Loan)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

## End of Logistic Regression



### Random Forest
set.seed(1000)

#install required libraries
library(randomForest)
library(caret)

#build the model using training dataset
rf <- randomForest(Personal.Loan ~., data=bank.train, ntree= 500, mtry=6, importance= TRUE)
rf

#plot the curve 
plot(rf)
#accuracy of the training data
Accuracy <- (3121 + 302)/nrow(bank.train)*100
print(Accuracy)

#predict on the test data using the model built above
p1<-predict(rf,bank.test)
p1

# get confusion matrix to calculate the accuracy of the model on the test data.

confusionMatrix(p1,bank.test$Personal.Loan)

#calculate the accuracy of the model on the test data.
Accuracy <- (1337 + 126)/nrow(bank.test)*100
print(Accuracy)

# variable importance plot

varImpPlot(rf)

#End of Random forest.








