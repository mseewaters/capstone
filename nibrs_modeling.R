library(DMwR)
library(performanceEstimation)
library(rpart)

data.final <- read.csv("NIBRS_cleaned_notfilled.csv")

data.final$same.race <- ifelse(data.final$victim.race==data.final$offender.race, 0, 1)

data.m <- data.final[,c('current.population','hour.group','victim.age','victim.sex',
                        'victim.race','victim.residency','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','location.group','same.race','target.harm')]



values <- data.final[,c('population.quartile','incident.hour','completed','victim.age','victim.sex',
                        'victim.race','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','location.group','same.race','target.harm')]

current.na.action <- options('na.action')
options(na.action='na.pass')
x1 <- as.data.frame(model.matrix(~population.quartile - 1, data.final))
x2 <- as.data.frame(model.matrix(~completed - 1, data.final))
x3 <- as.data.frame(model.matrix(~victim.sex - 1, data.final))
x4 <- as.data.frame(model.matrix(~victim.race - 1, data.final))
x5 <- as.data.frame(model.matrix(~offender.sex - 1, data.final))
x6 <- as.data.frame(model.matrix(~offender.race - 1, data.final))
x7 <- as.data.frame(model.matrix(~relationship.group - 1, data.final))
x8 <- as.data.frame(model.matrix(~division.name - 1, data.final))
x9 <- as.data.frame(model.matrix(~location.group - 1, data.final))
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


cluster <- kmeans(values.s, 10, nstart=25)

cluster$size

nibrs.cluster <- cbind(na.omit(values), cluster = cluster$cluster)
write.csv(nibrs.cluster, file = "nibrs_cluster.csv")


# Fill others
data.m <- centralImputation(data.m)

#Force to binary target
data.m$target.harm[which(data.m$target.harm==1)] <- 0
data.m$target.harm[which(data.m$target.harm==2)] <- 1
sum(data.m$target.harm)

data.m$target.harm <- as.factor(data.m$target.harm)

data.smote <- SMOTE(target.harm ~ ., data.m, perc.over = 100)
table(data.smote$target.harm)

model.tree <- rpart(target.harm ~ ., data=data.m, method="class")
#pred.tree <- predict(model.tree, data.m, type="class")
prettyTree(model.tree, cex=0.9, margin=0.05, compress=TRUE, fheight=.2, fwidth=.5)

model.tree <- rpart(target.harm ~ ., data=data.smote, method="class")
#pred.tree <- predict(model.tree, data.m, type="class")
prettyTree(model.tree, cex=.85, margin=0.01, compress=TRUE, fheight=.2, fwidth=.3)


printcp(model.tree)
classificationMetrics(data.m$target,pred.tree,stats=c("rec","prec","F"))
table(data.m$target,pred.tree)

write.csv(data.m, file="NIBRS_modeling.csv")

library(e1071)
library(randomForest)

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
model.svm <- svm(target.harm ~ ., data=train, cost=0.1, gamma=.5)
pred.svm <- predict(model.svm, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
table(test[,ncol(test)], pred.svm)

pred.svm <- predict(model.svm, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.svm)

# RF model evaluation and holdout analysis (non-sampled data)
model.rf <- randomForest(target.harm ~ ., data=train)
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

library(ada)

# generic model evaluation and holdout analysis (non-sampled data)
model.gen <- ada(target.harm ~ ., data=train)
pred.gen <- predict(model.gen, test[,-ncol(test)])
classificationMetrics(test$target.harm,pred.gen,stats=c("rec","prec","F"),posClass='1')
table(test[,ncol(test)], pred.gen)

pred.gen <- predict(model.gen, holdout[,-ncol(holdout)])
classificationMetrics(holdout$target.harm,pred.gen,stats=c("rec","prec","F"), posClass='1')
table(holdout[,ncol(holdout)], pred.gen)


####

data.smote.f <- SMOTE(target.harm ~ ., data.m, perc.over = 100)
table(data.smote.f$target.harm)

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

res2 <- performanceEstimation(
  PredTask(target.harm ~ ., data.smote.f),
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
  model.svm <- svm(target.harm ~ ., data=train, cost=1, gamma=0.5)
  pred.svm <- predict(model.svm, test[,-ncol(test)])
  classificationMetrics(test$target.harm,pred.svm,stats=c("rec","prec","F"), posClass='1')
  table(test[,ncol(test)], pred.svm)
  
  model.svm <- svm(target.harm ~ ., data=data.smote, cost=1, gamma=0.5)
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
  model.ada <- ada(target.harm ~ ., data=train)
  pred.ada <- predict(model.ada, test[,-ncol(test)])
  classificationMetrics(test$target.harm,pred.ada,stats=c("rec","prec","F"),posClass='1')
  table(test[,ncol(test)], pred.ada)
  
  model.ada <- ada(target.harm ~ ., data=data.smote)
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
