#data<-read.csv("C:/Users/aman/Downloads/datasets-master/datasets-master/titanic.csv")
#head(data)
install.packages("ROCR")
training.data.raw <- read.csv('C:/Users/aman/Downloads/datasets-master/datasets-master/titanic.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

#Selecting the columns that we need
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
data

#replacing missing values in age attribute with mean
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

is.factor(data$Sex)
is.factor(data$Embarked)

contrasts(data$Sex)

#Discarded rows which have null in embarked
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#Splitting datasets
train <- data[1:800,]
test <- data[801:889,]

#Fitting into the model
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

#Predicting the model
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

#ROC Curve and AUC 
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

