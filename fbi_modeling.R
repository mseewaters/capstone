library(DMwR)
library(ggplot2)
library(e1071)
library(performanceEstimation)
library(randomForest)
library(rpart)
library(plyr)
library(corrplot)
library(ggmap)
library(gridExtra)


# Load cleaned data -------------------------------------------------------
data <- read.csv("FBI_cleaned_notfilled.csv", stringsAsFactors=TRUE)

cor.data <- data[,c("Victim.Age","Victim.Race","Victim.Gender","Region","Offender.Age","Offender.Race",
                    "Offender.Gender","Rural.City", "Relate.Group", "target")]
for (i in 1:length(cor.data))
{
  cor.data[,i] <- as.numeric(cor.data[,i])
}
c <- cor(na.omit(cor.data))
corrplot(c)

#Density Plots of age of Victim and Offender with Mean line
ggplot(data, aes(x=Victim.Age)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Victim.Age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)


# modelBuildAndEvaluate --------------------------------------------------------------
# Remove extra attributes and those not known at the time of abduction
data.m <- data[,c("Victim.AgeGroup","Victim.Race","Victim.Gender","Region","Offender.AgeGroup","Offender.Race",
                  "Offender.Gender","Rural.City", "Relate.Group", "target")]
data.m$target <- as.factor(data.m$target)
data.smote <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote2 <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote3 <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote4 <- SMOTE(target ~ ., data.m, perc.over = 500, perc.under = 150)
smote.output <- as.data.frame(rbind(table(data.m$target),table(data.smote$target),table(data.smote4$target)))
rownames(smote.output) <- c("original data","upsampling","upsampling/downsampling")
smote.output

# Build and evaluation for SVM, NaiveBayes, RandomForest
library(adabag)
library(ada)
res <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),PredTask(target ~ ., data.smote2),PredTask(target ~ ., data.smote3),
    PredTask(target ~ ., data.smote4)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,10,100), gamma=c(0.1,0.01)),
                     evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,50,200), nodesize = c(2,5)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"))),
    workflowVariants("standardWF", learner = "ada", learner.pars=list(iter=c(10,50)), 
                     evaluator.pars=list(stats=c("rec","prec", "F")))),
  BootSettings(type=".632", nReps=100))
plot(res)
topPerformers(res)

model.svm <- svm(target ~ ., data=data.smote2, cost=10, gamma=0.1)
pred.svm <- predict(model.svm, data.smote)
classificationMetrics(data.smote$target,pred.svm,stats=c("rec","prec","F"))
table(data.smote$target,pred.svm)

model.svm <- svm(target ~ ., data=data.m, cost=10, gamma=0.1)
pred.svm <- predict(model.svm, data.m)
classificationMetrics(data.m$target,pred.svm,stats=c("rec","prec","F"))
table(data.m$target,pred.svm)

model.nb <- naiveBayes(target ~ ., data.smote2)
pred.nb <- predict(model.nb, data.smote)
table(data.smote$target, pred.nb)

model.tree <- rpart(target ~ ., data=data.smote2, method="class", control = rpart.control(minsplit = 20, minbucket = 10))
pred.tree <- predict(model.tree, data.smote2, type="class")
prettyTree(model.tree, cex=0.75, margin=0.05, compress=TRUE, fheight=1)
printcp(model.tree)
classificationMetrics(data.smote2$target,pred.tree,stats=c("rec","prec","F"))
table(data.smote2$target,pred.tree)

model.tree <- rpart(target ~ ., data=data.m, method="class", control = rpart.control(minsplit = 5, minbucket = 2))
pred.tree <- predict(model.tree, data.m, type="class")
prettyTree(model.tree, cex=0.75, margin=0.05, compress=TRUE, fheight=1)
printcp(model.tree)
classificationMetrics(data.m$target,pred.tree,stats=c("rec","prec","F"))
table(data.m$target,pred.tree)
