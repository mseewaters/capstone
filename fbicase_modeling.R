library(DMwR)
library(performanceEstimation)
library(rpart)
library(e1071)
library(randomForest)
library(ada)
library(ipred)

data.final <- read.csv("FBIcase_cleaned_notfilled.csv", na.strings=c("NA","","Unknown", "None"))

data.m1 <- data.final[,c('public','rural.city','RSO','victim.age','victim.sex',
                         'victim.race','offender.age','offender.sex',
                         'offender.race','multiple.victims','multiple.offenders', 'type',
                         'relationship.group','division.name','same.race','target.harm')]

data.m1$target.harm <- as.factor(data.m1$target.harm)
data.m1$same.race <- as.factor(data.m1$same.race)

for (i in c(1:4,6,7,9,10,13:16))
{
  data.m1[,i] <- as.character(data.m1[,i])
  data.m1[,i][is.na(data.m1[,i])] <- 'unknown'
  data.m1[,i] <- as.factor(data.m1[,i])
}
data.m1 <- centralImputation(data.m1)

mt <- table(data.m1$type)
prop.table(mt)

table(data.m1$target.harm)

library(ROCR)

trPerc = .80
data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f),as.integer(trPerc*nrow(data.smote.f)))
train <- data.smote.f[idx2,]
test <- data.smote.f[-idx2,]

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=100, gamma=0.1)
pr.svm <- predict(model.svm,test[,-ncol(test)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], test$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

plot(svm.perf,main="ROC Curves",col=3,lwd=2)
plot(svm.perf,col=3,lwd=2, add=TRUE)

### test individual basic models before running performance estimation
trPerc = .8
test.num <- 80

idx <- sample(1:nrow(data.m1),as.integer(trPerc*nrow(data.m1)))
holdout <- data.m1[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m1[idx,], perc.over = 100)
table(data.smote$target.harm)
train <- data.m1[idx,]

# ROC curve

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=100, gamma=0.1)
pr.svm <- predict(model.svm,test[,-ncol(test)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], test$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for svm",col=2,lwd=2)

# holdout analysis (non-sampled data)

pred.rf <- predict(model.svm, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='Yes')
table(holdout[,ncol(holdout)], pred.rf)

fbi.imp <- as.data.frame(importance(model.rf))
fbi.imp <- fbi.imp[order(-fbi.imp$MeanDecreaseGini),,drop=FALSE]


data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
table(data.smote.f$target.harm)

res <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m1),PredTask(target.harm ~ ., data.smote.f)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,100), gamma=c(0.1,0.01)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='Yes')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,200)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='Yes')),
    
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='Yes')),
    workflowVariants("standardWF", learner = "bagging", evaluator.pars=list(stats=c("rec","prec","F"), posClass='Yes')),
    workflowVariants("standardWF", learner = "ada", evaluator.pars=list(stats=c("rec","prec","F"), posClass='Yes'))
  ),
  BootSettings(type=".632", nReps=100))
plot(res)

getWorkflow("svm.v2",res)
