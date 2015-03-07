library(DMwR)
library(performanceEstimation)
library(rpart)
library(e1071)
library(randomForest)
library(ada)
library(ipred)
library(ROCR)
library(ggplot2)



#Load population demographic data
fips.data <- read.csv("fips_high_cor.csv")
fips.data$FIPS <- sprintf("%05d",fips.data$FIPS)
fips.data <- centralImputation(fips.data)

#Load NIBRS data and filter for 2010-12
data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")
data.final3$year <- as.numeric(substr(as.character(data.final3$incident.date),0,4))
data.final3.3yrs <- subset(data.final3,as.numeric(substr(as.character(data.final3$incident.date),0,4)) >=2010)

#Merge data sets and isolate fields for modeling
data.m3.raw <- data.final3.3yrs[,c('FIPS','current.population','hour.group','victim.age','victim.sex',
                              'victim.race','victim.residency','offenderage.group','offender.sex',
                              'offender.race','multiple.victims','multiple.offenders', 'type',
                              'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m3merge <- merge(data.m3.raw,fips.data)


data.m3 <- data.m3merge[,c('current.population','victim.age','offender.age',
                           'multiple.victims','multiple.offenders', 
                           'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                           'offender.race','type',
                           'relationship.group','division.name','loc.group','same.race',
                           'nonrelatives','relover65','rent','college','samehouse','employed','income',
                           'target.harm')]

data.m3 <- data.m3merge[,c('current.population','victim.age',
                           'multiple.victims','multiple.offenders', 
                           'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                           'offender.race','type', 'offenderage.group',
                           'relationship.group','division.name','loc.group','same.race',
                           'nonrelatives','relover65','rent','college','samehouse','employed','income',
                           'target.harm')]

# Increased variables for feature selection
data.m3.raw <- data.final3.3yrs[,c('FIPS','current.population','hour.group','incident.hour','victim.age','victim.sex',
                                   'victim.race','victim.residency','offenderage.group','offender.age','offender.sex',
                                   'offender.race','multiple.victims','multiple.offenders', 'type',
                                   'Victim.in.relation','relationship.group','state.name','region.name',
                                   'division.name','location.name','loc.group','same.race','target.harm')]

data.m3merge <- merge(data.m3.raw,fips.data)


data.m3 <- data.m3merge[,c('current.population','victim.age','offender.age', 'incident.hour',
                           'multiple.victims','multiple.offenders', 
                           'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                           'offender.race','offenderage.group','type',
                           'Victim.in.relation', 'relationship.group','division.name','state.name','region.name',
                           'loc.group','location.name','same.race',
                           'nonrelatives','relover65','rent','college','samehouse','employed','income',
                           'target.harm')]

# Final data for model
data.m3 <- data.m3merge[,c('victim.age','hour.group',
                           'offenderage.group','type',
                           'relationship.group','division.name',
                           'loc.group',
                           'nonrelatives','relover65','rent','college','samehouse','employed','income','pop',
                           'target.harm')]

# Fill missing data, factors as 'unknown', numbers as mean

data.m3$target.harm <- as.factor(data.m3$target.harm)
data.m3$same.race <- as.factor(data.m3$same.race)
#6:14

for (i in c(2:7))
{
  data.m3[,i] <- as.character(data.m3[,i])
  data.m3[,i][is.na(data.m3[,i])] <- 'Unknown'
  data.m3[,i] <- as.factor(data.m3[,i])
}
data.m3 <- centralImputation(data.m3)

# Create balanced sample data set
data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)

# Screen the base rate and sampled data on default models
res1 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3), PredTask(target.harm ~ ., data.smote.f3)),
  c(workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "svm", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=5, nReps=3))
plot(res1)

library(caret)
mod0 <- train(target.harm ~ ., data = data.m3,
              method = "rf",
             metric = "ROC",
              tuneGrid = data.frame(mtry = 3),
              trControl = trainControl(method = "cv",
                                      classProbs = TRUE,
                                      summaryFunction = twoClassSummary))
getTrainPerf(mod0)

# Evaluate comparative ROC to look across thresholds
trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]
train <- data.m3[idx,]

#data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
#table(data.smote$target.harm)
#train2 <- data.smote


model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for different models",col=2,lwd=2)
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")

#plot(rf.perf2,,col=3,lwd=2, add=TRUE)
auc2 <- performance(rf.pred,"auc")
auc2 <- unlist(slot(auc2, "y.values"))

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=10, gamma=0.1)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

#plot(svm.perf,main="ROC Curves",col=2,lwd=2)
auc3 <- performance(svm.pred,"auc")
auc3 <- unlist(slot(auc3, "y.values"))

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=10, gamma=0.1)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf2 <- performance(svm.pred,"tpr","fpr")

plot(svm.perf2,,col=3,lwd=2, add=TRUE)
auc4 <- performance(svm.pred,"auc")
auc4 <- unlist(slot(auc4, "y.values"))

model.bag <- bagging(target.harm ~ ., data=train)
pr.bag <- predict(model.bag,type="prob",holdout[,-ncol(holdout)])[,2]
bag.pred <- prediction(pr.bag, holdout$target.harm)
bag.perf <- performance(bag.pred,"tpr","fpr")

#plot(bag.perf,main="ROC Curves",col=2,lwd=2)
auc5 <- performance(bag.pred,"auc")
auc5 <- unlist(slot(auc5, "y.values"))

model.bag <- bagging(target.harm ~ ., data=train)
pr.bag <- predict(model.bag,type="prob",holdout[,-ncol(holdout)])[,2]
bag.pred <- prediction(pr.bag, holdout$target.harm)
bag.perf2 <- performance(bag.pred,"tpr","fpr")

plot(bag.perf2,,col=4,lwd=2, add=TRUE)
auc6 <- performance(bag.pred,"auc")
auc6 <- unlist(slot(auc6, "y.values"))


# Screen the base rate and sampled data at different thresholds
res2 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3), PredTask(target.harm ~ ., data.smote.f3)),
  c(workflowVariants("standardWF", learner = "randomForest", 
                     learner.pars=list(cutoff=c(0.3,0.7)),
                     evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=5, nReps=1))
plot(res1)


# prescreen for SVM
svm.tune.res <- data.frame(rec=NA, prec=NA, Fstat=NA)
for (n in 1:2)
{
  trPerc = .80
  idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
  holdout <- data.m3[-idx,]
  train <- data.m3[idx,]
  
  for (i in 0:4)
  {
    for (j in -2:2)
    {
      
      model.svm <- svm(target.harm ~ ., data=train, cost=10^i, gamma=10^j)
      pred.svm <- predict(model.svm, holdout[,-ncol(holdout)])  
      cmeas <- classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
      print(i)
      print(j)
      print(cmeas)
      svm.tune.res <- rbind(svm.tune.res,cmeas)
    }
  }
}


# More expansive screen of models and parameters

res <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,10,100), gamma=c(0.1)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(25,500), mtry=c(3,5,10)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')), 
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "bagging", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "ada", 
                     learner.pars=list(loss=c("e","l"), iter=c(50,200)),
                     evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=5, nReps=2))
plot(res)
getWorkflow("svm.v2",res)
estimationSummary(res, "randomForest.v6",'data.m3')
estimationSummary(res, "bagging",'data.m3')
estimationSummary(res, "svm.v2",'data.m3')



# Evaluaiton of 3 best ----------------------------------------------------
# Evaluate comparative ROC to look across thresholds for three models above
trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]
train <- data.m3[idx,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for different models",col=2,lwd=2)
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=10, gamma=0.1)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

#plot(svm.perf,main="ROC Curves",col=2,lwd=2)
auc3 <- performance(svm.pred,"auc")
auc3 <- unlist(slot(auc3, "y.values"))

model.bag <- bagging(target.harm ~ ., data=train)
pr.bag <- predict(model.bag,type="prob",holdout[,-ncol(holdout)])[,2]
bag.pred <- prediction(pr.bag, holdout$target.harm)
bag.perf <- performance(bag.pred,"tpr","fpr")

#plot(bag.perf,main="ROC Curves",col=2,lwd=2)
auc5 <- performance(bag.pred,"auc")
auc5 <- unlist(slot(auc5, "y.values"))

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]
train <- data.m3[idx,]

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")

#plot(rf.perf2,,col=3,lwd=2, add=TRUE)
auc2 <- performance(rf.pred,"auc")
auc2 <- unlist(slot(auc2, "y.values"))

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=10, gamma=0.1)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf2 <- performance(svm.pred,"tpr","fpr")

plot(svm.perf2,,col=3,lwd=2, add=TRUE)
auc4 <- performance(svm.pred,"auc")
auc4 <- unlist(slot(auc4, "y.values"))

model.bag <- bagging(target.harm ~ ., data=train)
pr.bag <- predict(model.bag,type="prob",holdout[,-ncol(holdout)])[,2]
bag.pred <- prediction(pr.bag, holdout$target.harm)
bag.perf2 <- performance(bag.pred,"tpr","fpr")

plot(bag.perf2,,col=4,lwd=2, add=TRUE)
auc6 <- performance(bag.pred,"auc")
auc6 <- unlist(slot(auc6, "y.values"))

abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(title="Data treatments", 0.4, 0.4, c('randomForest (AUC = 0.94)','svm (AUC = 0.91)','CART bagging (AUC = 0.93)'), 2:4)


# Data treament impact ----------------------------------------------------

# Evaluate comparative ROC to look across thresholds for three models above
trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]
train <- data.m3[idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
table(data.smote$target.harm)
train2 <- data.smote

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for different models",col=2,lwd=2)
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))


model.rf <- randomForest(target.harm ~ ., data=train2, ntree=500, mtry=10)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")

plot(rf.perf2,,col=3,lwd=2, add=TRUE)
auc2 <- performance(rf.pred,"auc")
auc2 <- unlist(slot(auc2, "y.values"))

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, mtry=10, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")
rf.perf3 <- performance(rf.pred,"lift")

plot(rf.perf2,col=4,lwd=2, add=TRUE)
auc3 <- performance(rf.pred,"auc")
auc3 <- unlist(slot(auc3, "y.values"))

plot(rf.perf3,col=4,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(title="Data treatments", 0.3, 0.4, c('No treatment (AUC = 0.93)','balanced target class (AUC = 0.92)','stratified type (AUC = 0.90)'), 2:4)


test.num <- 3000
# RF evaluation of data treatments - balancing and bias
res.thresh <- data.frame(rec1=NA, prec1=NA, Fstat1=NA)
for (i in 1:10)
{
  
  trPerc = .80
  idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
  holdout <- data.m3[-idx,]
  train <- data.m3[idx,]
  
  data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
  table(data.smote$target.harm)
  train2 <- data.smote
  
  model.rf <- randomForest(target.harm ~ ., data=train, ntree=500)
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])  
  cmeas <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
  print(cmeas)
  res.thresh <- rbind(res.thresh,cmeas)
  model.rf <- randomForest(target.harm ~ ., data=train2, ntree=500)
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])  
  cmeas <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
  print(cmeas)
  res.thresh <- rbind(res.thresh,cmeas)
  model.rf <- randomForest(target.harm ~ ., data=train2, ntree=500, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])  
  cmeas <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
  print(cmeas)
  res.thresh <- rbind(res.thresh,cmeas)
}
res.thresh <- res.thresh[-1,]

res.names <- data.frame(thresh=rep(1:9, each=2), data=rep(1:2, 9))

res.thresh.final <- cbind(res.names, res.thresh)
res.thresh.final$threshperc <- res.thresh.final$thresh/10*100
res.thresh.final$data <- as.factor(res.thresh.final$data)
ggplot(data=res.thresh.final, aes(x=threshperc, y=Fstat1, group=data, color=data)) + geom_line()



model.rf <- randomForest(target.harm ~ ., data=train, ntree=500)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for randomForest",col=2,lwd=2)
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))

model.rf <- randomForest(target.harm ~ ., data=train2, ntree=500)
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")

plot(rf.perf2,,col=3,lwd=2, add=TRUE)
auc2 <- performance(rf.pred,"auc")
auc2 <- unlist(slot(auc2, "y.values"))

model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, strata=train$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf2 <- performance(rf.pred,"tpr","fpr")

plot(rf.perf2,,col=4,lwd=2, add=TRUE)
auc3 <- performance(rf.pred,"auc")
auc3 <- unlist(slot(auc3, "y.values"))

abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(title="Data treatments", 0.4, 0.3, c('None (AUC = 0.95)','Balanced target (AUC = 0.94)','Sampled for type (AUC = 0.93)'), 2:4)

# Parameter optimization

res2 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3)),
  c(workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(100,500,1000), mtry=c(3,5,10,15), 
                                       nodesize=c(1,10,50)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1'))
    ),
  CvSettings(nFolds=5, nReps=2))
plot(res2)
getWorkflow("randomForest.v2",res2)



# First feature selection -------------------------------------------------


# Evaluation of multiple category variables

data.base <- data.m3[,c('current.population','victim.age','offender.age', 
                        'multiple.victims','multiple.offenders', 
                        'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                        'offender.race','type',
                        'relationship.group','division.name',
                        'loc.group','same.race',
                        'nonrelatives','relover65','rent','college','samehouse','employed','income',
                        'target.harm')]

data.geo1 <- data.m3[,c('current.population','victim.age','offender.age', 
                        'multiple.victims','multiple.offenders', 
                        'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                        'offender.race','type',
                        'relationship.group','state.name',
                        'loc.group','same.race',
                        'nonrelatives','relover65','rent','college','samehouse','employed','income',
                        'target.harm')]

data.geo2 <- data.m3[,c('current.population','victim.age','offender.age', 
                        'multiple.victims','multiple.offenders', 
                        'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                        'offender.race','type',
                        'relationship.group','region.name',
                        'loc.group','same.race',
                        'nonrelatives','relover65','rent','college','samehouse','employed','income',
                        'target.harm')]

data.loc <- data.m3[,c('current.population','victim.age','offender.age', 
                       'multiple.victims','multiple.offenders', 
                       'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                       'offender.race','type',
                       'relationship.group','division.name',
                       'location.name','same.race',
                       'nonrelatives','relover65','rent','college','samehouse','employed','income',
                       'target.harm')]

data.hour <- data.m3[,c('current.population','victim.age','offender.age', 
                        'multiple.victims','multiple.offenders', 
                        'victim.sex','victim.race','victim.residency','offender.sex', 'incident.hour',
                        'offender.race','type',
                        'relationship.group','division.name',
                        'loc.group','same.race',
                        'nonrelatives','relover65','rent','college','samehouse','employed','income',
                        'target.harm')]

data.age <- data.m3[,c('current.population','victim.age','offenderage.group', 
                       'multiple.victims','multiple.offenders', 
                       'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                       'offender.race','type',
                       'relationship.group','division.name',
                       'loc.group','same.race',
                       'nonrelatives','relover65','rent','college','samehouse','employed','income',
                       'target.harm')]

data.relate <- data.m3[,c('current.population','victim.age','offender.age', 
                          'multiple.victims','multiple.offenders', 
                          'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                          'offender.race','type',
                          'Victim.in.relation','division.name',
                          'loc.group','same.race',
                          'nonrelatives','relover65','rent','college','samehouse','employed','income',
                          'target.harm')]

data.nodemo <- data.m3[,c('current.population','victim.age','offender.age', 
                          'multiple.victims','multiple.offenders', 
                          'victim.sex','victim.race','victim.residency','offender.sex', 'hour.group',
                          'offender.race','type',
                          'relationship.group','division.name',
                          'loc.group','same.race',
                          'target.harm')]


res3 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.base),PredTask(target.harm ~ ., data.geo1),PredTask(target.harm ~ ., data.geo2),
    PredTask(target.harm ~ ., data.loc), PredTask(target.harm ~ ., data.relate)),
  c(workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(500), mtry=c(10)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')) 
  ),
  CvSettings(nFolds=5, nReps=2))
plot(res3)

res4 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.base), PredTask(target.harm ~ ., data.hour), 
    PredTask(target.harm ~ ., data.age), PredTask(target.harm ~ ., data.nodemo)),
  c(workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(500), mtry=c(10)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')) 
  ),
  CvSettings(nFolds=5, nReps=2))
plot(res4)

res5 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.base), PredTask(target.harm ~ ., data.geo2), PredTask(target.harm ~ ., data.relate),
    PredTask(target.harm ~ ., data.hour), PredTask(target.harm ~ ., data.age), PredTask(target.harm ~ ., data.nodemo)),
  c(workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(500), mtry=c(10)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')) 
  ),
  CvSettings(nFolds=5, nReps=2))
plot(res5)

getWorkflow("svm.v2",res)
estimationSummary(res3, "randomForest",'data.base')
estimationSummary(res3, "randomForest",'data.relate')
estimationSummary(res3, "randomForest",'data.geo2')


# Second feature selection ------------------------------------------------

library(AUCRF)
test.num = 3000
fit <- AUCRF(target.harm ~ ., data=data.m3, ntree=500, strata=data.m3$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
summary(fit)
plot(fit)
OptimalSet(fit)

fitCV <- AUCRFcv(fit)
summary(fitCV)
plot(fitCV)
OptimalSet(fitCV)


# Final model -------------------------------------------------------------


model.rf <- randomForest(target.harm ~ ., data=data.m3, mtry=10, ntree=200, strata=data.m3$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))
holdout <- data.m3[-idx,]
train <- data.m3[idx,]

res.thresh <- data.frame(rec=NA, prec=NA, Fstat=NA)
for (i in 1:19)
{
  model.rf <- randomForest(target.harm ~ ., data=train, cutoff=c(i/20,1-i/20), mtry=10, ntree=200, strata=data.m3$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])  
  cmeas <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
  print(cmeas)
  res.thresh <- rbind(res.thresh,cmeas)
}
res.thresh <- res.thresh[-1,]

res.names <- data.frame(thresh=rep(1:19))

res.thresh.final <- cbind(res.names, res.thresh)
res.thresh.final$threshperc <- res.thresh.final$thresh/20*100
ggplot(data=res.thresh.final, aes(x=threshperc, y=rec)) + geom_line()

library(reshape)
thresh.melt <- melt(res.thresh.final, id=c("thresh","threshperc"))
ggplot(data=thresh.melt, aes(x=threshperc, y=value, group=variable, color=variable)) + geom_line(size=1.5)


model.rf <- randomForest(target.harm ~ ., data=train, ntree=200, mtry=10, strata=data.m3$type, sampsize=c(SingleVO=.74*test.num, MultVic=.12*test.num, MultOff=.11*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for final model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))

data.m3[1:4,]
