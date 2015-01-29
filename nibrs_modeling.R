library(DMwR)
library(performanceEstimation)
library(rpart)

data.final <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")



# Clustering --------------------------------------------------------------

values <- data.final[,c('population.group','incident.hour','completed','victim.age','victim.sex',
                        'victim.race','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','loc.group','same.race','target.harm')]

current.na.action <- options('na.action')
options(na.action='na.pass')
x1 <- as.data.frame(model.matrix(~population.group - 1, data.final))
x2 <- as.data.frame(model.matrix(~completed - 1, data.final))
x3 <- as.data.frame(model.matrix(~victim.sex - 1, data.final))
x4 <- as.data.frame(model.matrix(~victim.race - 1, data.final))
x5 <- as.data.frame(model.matrix(~offender.sex - 1, data.final))
x6 <- as.data.frame(model.matrix(~offender.race - 1, data.final))
x7 <- as.data.frame(model.matrix(~relationship.group - 1, data.final))
x8 <- as.data.frame(model.matrix(~division.name - 1, data.final))
x9 <- as.data.frame(model.matrix(~loc.group - 1, data.final))
x10 <- as.data.frame(model.matrix(~offender.sex - 1, data.final))
# add others here using froamework above
data.clustering <- cbind(values[,c(2,3,4,7,10,11,15,16)],x1,x3,x4,x5,x6,x7,x9)
options(na.action='na.omit')

for (i in 1:length(data.clustering))
{
  data.clustering[,i] <- as.numeric(data.clustering[,i])
}
values.s <- na.omit(data.clustering)

mydata <- values.s
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


cluster <- kmeans(values.s, 20, nstart=25)

cluster$size
cluster$centers

nibrs.cluster <- cbind(na.omit(values), cluster = cluster$cluster)
write.csv(nibrs.cluster, file = "nibrs_cluster.csv")


# Modeling ----------------------------------------------------------------

library(e1071)
library(randomForest)
library(ada)
library(ipred)

data.m <- data.final[,c('current.population','incident.hour','victim.age','victim.sex',
                        'victim.race','victim.residency','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m2 <- data.final[,c('current.population','hour.group','victim.age','victim.sex',
                        'victim.race','victim.residency','offenderage.group','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m3 <- data.final[,c('current.population','hour.group','victim.age','offenderage.group','multiple.offenders',
                         'relationship.group','division.name','loc.group','target.harm')]
# Fill others
data.m <- centralImputation(data.m)
data.m$target.harm <- as.factor(data.m$target.harm)

data.m2 <- centralImputation(data.m2)
data.m2$target.harm <- as.factor(data.m2$target.harm)

data.m3 <- centralImputation(data.m3)
data.m3$target.harm <- as.factor(data.m3$target.harm)

data.smote.f <- SMOTE(target.harm ~ ., data.m, perc.over = 100)
model.rf <- randomForest(target.harm ~ ., data=data.smote.f, ntree=500, nodesize=2)
opt1.imp <- as.data.frame(importance(model.rf))
opt1.imp <- opt1.imp[order(-opt1.imp$MeanDecreaseGini),,drop=FALSE]

#ROC curve for RF
library(ROCR)
data.smote.f <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)

trPerc = .8
idx2 <- sample(1:nrow(data.smote.f),as.integer(trPerc*nrow(data.smote.f)))
train <- data.smote.f[idx2,]
test <- data.smote.f[-idx2,]

model.svm <- svm(target.harm ~ ., data=train,probability=TRUE, cost=1, gamma=0.1)
pr.svm <- predict(model.svm,test[,-ncol(test)], probability=TRUE)
svm.pred <- prediction(1-attr(pr.svm,"probabilities")[,2], test$target.harm)
svm.perf <- performance(svm.pred,"tpr","fpr")

plot(svm.perf,main="ROC Curves",col=3,lwd=2)


model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
pr.rf <- predict(model.rf,type="prob",test[,-ncol(test)])[,2]
rf.pred <- prediction(pr.rf, test$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,col=2,lwd=2, add=TRUE)

model.ada <- ada(target.harm ~ ., data=train)
pr.ada <- predict(model.ada,type="prob",test[,-ncol(test)])[,2]
ada.pred <- prediction(pr.ada, test$target.harm)
ada.perf <- performance(ada.pred,"tpr","fpr")

plot(ada.perf,col=4,lwd=2, add=TRUE)

model.nb <- naiveBayes(target.harm ~ ., data=train)
pr.nb <- predict(model.nb,type="raw",test[,-ncol(test)])[,2]
nb.pred <- prediction(pr.nb, test$target.harm)
nb.perf <- performance(nb.pred,"tpr","fpr")

plot(nb.perf,col=5,lwd=2, add=TRUE)



model.bag <- bagging(target.harm ~ ., data=train)
pr.bag <- predict(model.bag,type="prob",test[,-ncol(test)])[,2]
bag.pred <- prediction(pr.bag, test$target.harm)
bag.perf <- performance(bag.pred,"tpr","fpr")

plot(bag.perf,col=6,lwd=2, add=TRUE)

model.ct <- rpart(target.harm ~ ., data=train)
pr.ct <- predict(model.ct,type="prob", test[,-ncol(test)])[,2]
ct.pred <- prediction(pr.ct, test$target.harm)
ct.perf <- performance(ct.pred,"tpr","fpr")

plot(ct.perf,col=7,lwd=2, add=TRUE)

abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.7, 0.4, c('Random Forest','svm','adaBoost','Naive Bayes','bagging','rpart'), 2:7)

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))


### test individual basic models before running performance estimation
trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))

holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
table(data.smote$target.harm)
train <- data.smote

trPerc = .9
idx2 <- sample(1:nrow(data.smote),as.integer(trPerc*nrow(data.smote)))

train <- data.smote[idx2,]
test <- data.smote[-idx2,]



# svm model evaluation and holdout analysis (non-sampled data)
model.svm <- svm(target.harm ~ ., data=train)
pred.svm <- predict(model.svm, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
table(test[,ncol(test)], pred.svm)

pred.svm <- predict(model.svm, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.svm)

# RF model evaluation and holdout analysis (non-sampled data)
test.num <- 2000
model.rf <-randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, cutoff=c(0.3,0.7), strata=train$type, sampsize=c(SingleVO=.725*test.num, MultVic=.15*test.num, MultOff=.1*test.num, MultOV=.025*test.num))
pred.rf <- predict(model.rf, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.rf,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.rf)

pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.rf)


# NB model evaluation and holdout analysis (non-sampled data)
model.nb <- naiveBayes(target.harm ~ ., data=train)
pred.nb <- predict(model.nb, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.nb,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.nb)

pred.nb <- predict(model.nb, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.nb,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.nb)


# generic model evaluation and holdout analysis (non-sampled data)
model.gen <- ctree(target.harm ~ ., data=train)
pred.gen <- predict(model.gen, test[,-ncol(test)], type='response')
classificationMetrics(test$target.harm,pred.gen,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.gen)

pred.gen <- predict(model.gen, holdout[,-ncol(holdout)], type='response')
classificationMetrics(holdout$target.harm,pred.gen,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.gen)


# Optimized by evaluate upsampling and different model parameters

data.smote.f <- SMOTE(target.harm ~ ., data.m, perc.over = 100)
table(data.smote.f$target.harm)

data.smote.f2 <- SMOTE(target.harm ~ ., data.m2, perc.over = 100)
table(data.smote.f2$target.harm)

data.smote.f3 <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
table(data.smote.f3$target.harm)

res <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.m),PredTask(target.harm ~ ., data.smote.f)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,100), gamma=c(0.1,0.001)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,200)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "ada", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=10, nReps=1))
plot(res)

res1 <- performanceEstimation(
  c(PredTask(target.harm ~ ., data.smote.f),PredTask(target.harm ~ ., data.smote.f2), PredTask(target.harm ~ ., data.smote.f3)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(0.1,1), gamma=c(0.1,0.001)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(10,500)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "ada", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
  ),
  CvSettings(nFolds=10, nReps=1))
plot(res1)

res2 <- performanceEstimation(
  PredTask(target.harm ~ ., data.smote.f3),
  c(workflowVariants("standardWF", learner = "ada", learner.pars=list(loss = c('exponential','logistic'), type = c('discrete','real','gentle')),
                     evaluator.pars=list(stats=c("rec","prec","F"), posClass='1')),
    workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(0.1,1,10,100), gamma=c(0.5,0.1,0.01,0.001)),
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(10,200,500), nodesize = c(2,10,50)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"), posClass='1')),
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"), posClass='1'))
    
    ),
  CvSettings(nFolds=10, nReps=2))
plot(res2)
topPerformers(res2)

getWorkflow("svm.v2",res2)
getWorkflow("randomForest.v3",res2)


output <- as.data.frame(rep(NA, 4))

for (i in 1:10)
{
 
  
  trPerc = .90
  idx <- sample(1:nrow(data.m),as.integer(trPerc*nrow(data.m)))
  
  holdout <- data.m[-idx,]
  
  data.smote <- SMOTE(target.harm ~ ., data.m[idx,], perc.over = 100)
  table(data.smote$target.harm)
  
  trPerc = .9
  idx2 <- sample(1:nrow(data.smote),as.integer(trPerc*nrow(data.smote)))
  
  train <- data.smote[idx2,]
  test <- data.smote[-idx2,]
  
  # svm model evaluation and holdout analysis (non-sampled data)
  model.svm <- svm(target.harm ~ ., data=train, cost=1, gamma=0.1)
  pred.svm <- predict(model.svm, test[,-ncol(test)])
  classificationMetrics(test$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
  table(test[,ncol(test)], pred.svm)
  
  model.svm <- svm(target.harm ~ ., data=data.smote, cost=1, gamma=0.1)
  pred.svm <- predict(model.svm, holdout[,-ncol(holdout)])
  print(classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1'))
  output[1,i] <- classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')[1]
  print(table(holdout[,ncol(holdout)], pred.svm))
  
  # RF model evaluation and holdout analysis (non-sampled data)
  model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2)
  pred.rf <- predict(model.rf, test[,-ncol(test)])
  classificationMetrics(test$target.harm,pred.rf,stats=c("rec","prec","F"),posClass='1')
  table(test[,ncol(test)], pred.rf)
  
  model.rf <- randomForest(target.harm ~ ., data=data.smote, ntree=500, nodesize=2)
  pred.rf <- predict(model.rf, holdout[,-ncol(holdout)])
  print(classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1'))
  output[2,i] <- classificationMetrics(holdout$target.harm,pred.rf,stats=c("rec","prec","F"), posClass='1')[1]
  print(table(holdout[,ncol(holdout)], pred.rf))
  
  # generic model evaluation and holdout analysis (non-sampled data)
  model.ada <- bagging(target.harm ~ ., data=train)
  pred.ada <- predict(model.ada, test[,-ncol(test)])
  classificationMetrics(test$target.harm,pred.ada,stats=c("rec","prec","F"),posClass='1')
  table(test[,ncol(test)], pred.ada)
  
  model.ada <- bagging(target.harm ~ ., data=data.smote)
  pred.ada <- predict(model.ada, holdout[,-ncol(holdout)])
  print(classificationMetrics(holdout$target.harm,pred.ada,stats=c("rec","prec","F"), posClass='1'))
  output[3,i] <- classificationMetrics(holdout$target.harm,pred.ada,stats=c("rec","prec","F"), posClass='1')[1]
  print(table(holdout[,ncol(holdout)], pred.ada))
  
  comb <- cbind(pred.svm, pred.rf,pred.ada)
  pred.all <- factor(round(rowMeans(comb),0), labels=c('0','1'))
  print(classificationMetrics(holdout$target.harm,pred.all,stats=c("rec","prec","F"), posClass='1'))
  output[4,i] <- classificationMetrics(holdout$target.harm,pred.all,stats=c("rec","prec","F"), posClass='1')[1]
  print(table(holdout[,ncol(holdout)], pred.all))
}
importance(model.rf)


# Time to arrest ----------------------------------------------------------

data.a <- data.final[,c('current.population','incident.hour','victim.age','victim.sex',
                        'victim.race','victim.residency','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','loc.group','same.race','target.arrest')]
data.a <- data.a[!is.na(data.a$target.arrest),]

data.a <- centralImputation(data.a)


trPerc = .80
idx <- sample(1:nrow(data.a),as.integer(trPerc*nrow(data.a)))

train <- data.a[idx,]
test <- data.a[-idx,]

# svm model evaluation and holdout analysis (non-sampled data)
model.svm <- randomForest(target.arrest ~ ., data=train)
pred.svm <- round(predict(model.svm, test[,-ncol(test)]),1)
regressionMetrics(test$target.arrest,pred.svm,stats=c("mse"))
time.arrest <- cbind(test$target.arrest,pred.svm)
