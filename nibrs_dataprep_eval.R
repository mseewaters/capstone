library(DMwR)
library(performanceEstimation)
library(rpart)
library(e1071)
library(randomForest)
library(ada)
library(ipred)


data.final1 <- read.csv("NIBRS_cleaned_notfilled.csv")
data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")

data.m1 <- data.final1[,c('current.population','victim.age','multiple.victims','multiple.offenders',
                          'hour.group','victim.sex', 'victim.race','victim.residency','offenderage.group','offender.sex',
                          'offender.race','type','relationship.group','division.name','loc.group','same.race','target.harm')]


data.m3 <- data.final3[,c('current.population','victim.age','multiple.victims','multiple.offenders',
                          'hour.group','victim.sex', 'victim.race','victim.residency','offenderage.group','offender.sex',
                          'offender.race','type','relationship.group','division.name','loc.group','same.race','target.harm')]


# Fill others

data.m1$target.harm <- as.factor(data.m1$target.harm)
data.m1$same.race <- as.factor(data.m1$same.race)

for (i in c(5:14))
{
  data.m1[,i] <- as.character(data.m1[,i])
  data.m1[,i][is.na(data.m1[,i])] <- 'unknown'
  data.m1[,i] <- as.factor(data.m1[,i])
}
data.m1 <- centralImputation(data.m1)


data.m3$target.harm <- as.factor(data.m3$target.harm)
data.m3$same.race <- as.factor(data.m3$same.race)

for (i in c(5:14))
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

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
train <- data.smote

test.num <- 3500
model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve:  Test of data treatments",col=2,lwd=2)
#plot(rf.perf,col=3,lwd=2, add=TRUE)

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
train <- data.smote

test.num <- 3500
model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
train <- data.smote

test.num <- 3500
model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

trPerc = .80
idx <- sample(1:nrow(data.m1),as.integer(trPerc*nrow(data.m1)))
holdout <- data.m1[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m1[idx,], perc.over = 100)
train <- data.smote

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

#plot(rf.perf,main="ROC Curve:  Test of data treatments",col=2,lwd=2)
plot(rf.perf,col=3,lwd=2, add=TRUE)

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

trPerc = .80
idx <- sample(1:nrow(data.m1),as.integer(trPerc*nrow(data.m1)))
holdout <- data.m1[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m1[idx,], perc.over = 100)
train <- data.smote

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

trPerc = .80
idx <- sample(1:nrow(data.m1),as.integer(trPerc*nrow(data.m1)))
holdout <- data.m1[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m1[idx,], perc.over = 100)
train <- data.smote

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc

abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(title="Approach to data preparation", 0.4, 0.3, c('1: victim/offender based (AUC = 0.88)','2: Incident based (AUC = 0.86)'), 2:3)


