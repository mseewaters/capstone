library(DMwR)
library(performanceEstimation)
library(rpart)
library(e1071)
library(randomForest)
library(ada)
library(ipred)

data.final <- read.csv("NIBRS_cleaned_notfilled.csv")
data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")

data.m1 <- data.final[,c('current.population','hour.group','victim.age','victim.sex',
                         'victim.race','victim.residency','offenderage.group','offender.sex',
                         'offender.race','multiple.victims','multiple.offenders',
                         'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m3 <- data.final3[,c('current.population','hour.group','victim.age','victim.sex',
                         'victim.race','victim.residency','offenderage.group','offender.sex',
                         'offender.race','multiple.victims','multiple.offenders',
                         'relationship.group','division.name','loc.group','same.race','target.harm')]


# Fill others
data.m1 <- centralImputation(data.m1)
data.m1$target.harm <- as.factor(data.m1$target.harm)

data.m3 <- centralImputation(data.m3)
data.m3$target.harm <- as.factor(data.m3$target.harm)

data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
model.rf <- randomForest(target.harm ~ ., data=data.smote.f, ntree=500, nodesize=2)
opt1.imp <- as.data.frame(importance(model.rf))
opt1.imp <- opt1.imp[order(-opt1.imp$MeanDecreaseGini),,drop=FALSE]

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
model.rf <- randomForest(target.harm ~ ., data=data.smote.f3, ntree=500, nodesize=2)
opt3.imp <- as.data.frame(importance(model.rf))
opt3.imp <- opt3.imp[order(-opt3.imp$MeanDecreaseGini),,drop=FALSE]


#ROC curve for RF
library(ROCR)

trPerc = .8
data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f),as.integer(trPerc*nrow(data.smote.f)))
train <- data.smote.f[idx2,]
test <- data.smote.f[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curves using upsampled data",col=2,lwd=2)

data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f),as.integer(trPerc*nrow(data.smote.f)))
train <- data.smote.f[idx2,]
test <- data.smote.f[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=2,lwd=2, add=TRUE)

data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f),as.integer(trPerc*nrow(data.smote.f)))
train <- data.smote.f[idx2,]
test <- data.smote.f[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=2,lwd=2, add=TRUE)

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f3),as.integer(trPerc*nrow(data.smote.f3)))
train <- data.smote.f3[idx2,]
test <- data.smote.f3[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=3,lwd=2, add=TRUE)

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f3),as.integer(trPerc*nrow(data.smote.f3)))
train <- data.smote.f3[idx2,]
test <- data.smote.f3[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=3,lwd=2, add=TRUE)

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
idx2 <- sample(1:nrow(data.smote.f3),as.integer(trPerc*nrow(data.smote.f3)))
train <- data.smote.f3[idx2,]
test <- data.smote.f3[-idx2,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=3,lwd=2, add=TRUE)

abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(title="Approach to data preparation", 0.55, 0.3, c('1: Incident based (n=4757)','3: victim/offender based (n=6426)'), 2:3)





res1 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.smote.f), PredTask(target.harm ~ ., data.smote.f3)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,100), gamma=c(0.1,0.001)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(25,500)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "bagging", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "ada", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=5, nReps=3))
plot(res1)