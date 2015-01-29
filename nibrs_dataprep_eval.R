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
                         'offender.race','multiple.victims','multiple.offenders', 'type',
                         'relationship.group','division.name','loc.group','same.race','target.harm')]


# Fill others

data.m1$target.harm <- as.factor(data.m1$target.harm)
data.m1$same.race <- as.factor(data.m1$same.race)

for (i in c(2,4:14))
{
  data.m1[,i] <- as.character(data.m1[,i])
  data.m1[,i][is.na(data.m1[,i])] <- 'unknown'
  data.m1[,i] <- as.factor(data.m1[,i])
}
data.m1 <- centralImputation(data.m1)


data.m3$target.harm <- as.factor(data.m3$target.harm)
data.m3$same.race <- as.factor(data.m3$same.race)

for (i in c(2,4:14))
{
  data.m3[,i] <- as.character(data.m3[,i])
  data.m3[,i][is.na(data.m3[,i])] <- 'unknown'
  data.m3[,i] <- as.factor(data.m3[,i])
}
data.m3 <- centralImputation(data.m3)

data.smote.f <- SMOTE(target.harm ~ ., data.m1, perc.over = 100)
model.rf <- randomForest(target.harm ~ ., data=data.smote.f, ntree=500, nodesize=2)
opt1.imp <- as.data.frame(importance(model.rf))
opt1.imp <- opt1.imp[order(-opt1.imp$MeanDecreaseGini),,drop=FALSE]

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
model.rf <- randomForest(target.harm ~ ., data=data.m3, ntree=500, nodesize=2)
opt3.imp <- as.data.frame(importance(model.rf))
opt3.imp <- opt3.imp[order(-opt3.imp$MeanDecreaseGini),,drop=FALSE]


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

plot(rf.perf,main="ROC Curve for randomForest",col=2,lwd=2)

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

test.num <- 2000
model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.725*test.num, MultVic=.15*test.num, MultOff=.1*test.num, MultOV=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for randomForest (3 reps)",col=2,lwd=2)
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
  c(PredTask(target.harm ~ ., data.m1), PredTask(target.harm ~ ., data.m3)),
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


### test individual basic models before running performance estimation

trPerc = .90
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))

holdout <- data.m3[-idx,]

data.smote <- data.m3[idx,]

trPerc = .9
idx2 <- sample(1:nrow(data.smote),as.integer(trPerc*nrow(data.smote)))

train <- data.smote[idx2,]
test <- data.smote[-idx2,]

# RF model evaluation and holdout analysis (non-sampled data)
model.rf <- randomForest(target.harm ~ ., data=train)
pred.rf <- predict(model.rf, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.rf,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.rf)

pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.rf)


trPerc = .90
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))

holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)

trPerc = .9
idx2 <- sample(1:nrow(data.smote),as.integer(trPerc*nrow(data.smote)))

train <- data.smote[idx2,]
test <- data.smote[-idx2,]

# RF model evaluation and holdout analysis (non-sampled data)
model.rf <- randomForest(target.harm ~ ., data=train)
pred.rf <- predict(model.rf, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.rf,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.rf)

pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.rf)