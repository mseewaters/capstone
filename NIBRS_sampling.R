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
                              'victim.race','victim.residency','offender.age','offender.sex',
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


# Fill missing data, factors as 'unknown', numbers as mean

data.m3$target.harm <- as.factor(data.m3$target.harm)
data.m3$same.race <- as.factor(data.m3$same.race)

for (i in c(6:14))
{
  data.m3[,i] <- as.character(data.m3[,i])
  data.m3[,i][is.na(data.m3[,i])] <- 'unknown'
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

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
table(data.smote$target.harm)
train <- data.smote
train2 <- data.m3[idx,]


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

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

plot(svm.perf,main="ROC Curves",col=2,lwd=2)
auc3 <- performance(svm.pred,"auc")
auc3 <- unlist(slot(auc3, "y.values"))

model.svm <- svm(target.harm ~ ., data=train2,probability=TRUE)
pr.svm <- predict(model.svm,holdout[,-ncol(holdout)], probability=TRUE)
svm.pred <- prediction(attr(pr.svm,"probabilities")[,2], holdout$target.harm)
svm.perf2 <- performance(svm.pred,"tpr","fpr")

plot(svm.perf2,,col=3,lwd=2, add=TRUE)
auc4 <- performance(svm.pred,"auc")
auc4 <- unlist(slot(auc4, "y.values"))

# Screen the base rate and sampled data at different thresholds
res2 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3), PredTask(target.harm ~ ., data.smote.f3)),
  c(workflowVariants("standardWF", learner = "randomForest", 
                     learner.pars=list(cutoff=c(0.3,0.7)),
                     evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=5, nReps=1))
plot(res1)

res.thresh <- data.frame(rec1=NA, prec1=NA, Fstat1=NA)
for (i in 1:9)
{
  model.rf <- randomForest(target.harm ~ ., data=train, cutoff=c(i/10,1-i/10))
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])  
  cmeas <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
  print(cmeas)
  res.thresh <- rbind(res.thresh,cmeas)
  model.rf <- randomForest(target.harm ~ ., data=train2, cutoff=c(i/10,1-i/10))
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

# More expansive creen of models and parameters

res <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m3)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,100), gamma=c(0.1,0.001)),
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


svm.tune.res <- data.frame(rec=NA, prec=NA, Fstat=NA)
i=1
j=1

for (i in -2:2)
{
  for (j in -2:2)
  {
    

    model.svm <- svm(target.harm ~ ., data=data.m3, cost=10^i, gamma=10^j)
    pred.svm <- predict(model.svm, holdout[,-ncol(holdout)])  
    cmeas <- classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
    print(cmeas)
    res.thresh <- rbind(svm.tune.res,cmeas)
  }
}
model.svm <- svm(target.harm ~ ., data=data.m3,probability=TRUE, cost=1, gamma=0.1)


# Parameter tuning
library(caret)

fitControl <- trainControl(method = "repeatedCV", number=5, repeats=2, 
                           classProbs = TRUE,summaryFunction = twoClassSummary)

rfFit <- train(target.harm~., data=data.m3,
               method = "rf", trControl=fitControl, metric="ROC")
