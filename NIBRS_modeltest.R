library(DMwR)
library(performanceEstimation)
library(rpart)
library(e1071)
library(randomForest)
library(ada)
library(ipred)
library(ROCR)


data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")
fips.data <- read.csv("fips_high_cor.csv")
fips.data$FIPS <- sprintf("%05d",fips.data$FIPS)
fips.data <- centralImputation(fips.data)


data.m3 <- data.final3[,c('current.population','hour.group','victim.age','victim.sex',
                          'victim.race','victim.residency','offenderage.group','offender.sex',
                          'offender.race','multiple.victims','multiple.offenders', 'type',
                          'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m3.raw <- data.final3[,c('FIPS','current.population','hour.group','victim.age','victim.sex',
                              'victim.race','victim.residency','offenderage.group','offender.sex',
                              'offender.race','multiple.victims','multiple.offenders', 'type',
                              'relationship.group','division.name','loc.group','same.race','target.harm')]

data.m3merge <- merge(data.m3.raw,fips.data)


data.m3 <- data.m3merge[,c('current.population','hour.group','victim.age','victim.sex',
                           'victim.race','victim.residency','offenderage.group','offender.sex',
                           'offender.race','multiple.victims','multiple.offenders', 'type',
                           'relationship.group','division.name','loc.group','same.race',
                           'old1','old2','families','somecollege','house',
                           'rent','older.res','per.white','per.black','bachelors','foodstamps',
                           'target.harm')]

data.m3 <- data.m3merge[,c('current.population','victim.age',
                           'offenderage.group',
                           'type',
                           'relationship.group','division.name',
                           'rent','per.white','target.harm')]

data.m3 <- data.m3merge[,c('current.population','victim.age',
                           'offenderage.group',
                           'type',
                           'relationship.group','division.name',
                           'target.harm')]


# Fill others

data.m3$target.harm <- as.factor(data.m3$target.harm)


for (i in c(2,4:9,12:15))
{
  data.m3[,i] <- as.character(data.m3[,i])
  data.m3[,i][is.na(data.m3[,i])] <- 'unknown'
  data.m3[,i] <- as.factor(data.m3[,i])
}
data.m3 <- knnImputation(data.m3, k=25)

trPerc = .80
idx <- sample(1:nrow(data.m3),as.integer(trPerc*nrow(data.m3)))

holdout <- data.m3[-idx,]

data.smote <- SMOTE(target.harm ~ ., data.m3[idx,], perc.over = 100)
table(data.smote$target.harm)
train <- data.smote



#trPerc = .9
#idx2 <- sample(1:nrow(data.smote),as.integer(trPerc*nrow(data.smote)))

#train <- data.smote[idx2,]
#test <- data.smote[-idx2,]

test.num <- 3500
model.rf <- randomForest(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.725*test.num, MultVic=.15*test.num, MultOff=.1*test.num, MultVO=.025*test.num))
pr.rf <- predict(model.rf,type="prob",holdout[,-ncol(holdout)])[,2]
rf.pred <- prediction(pr.rf, holdout$target.harm)
rf.perf <- performance(rf.pred,"tpr","fpr")

plot(rf.perf,main="ROC Curve for randomForest (3 reps)",col=2,lwd=2)
#plot(rf.perf,col=3,lwd=2, add=TRUE)

#compute area under curve
auc <- performance(rf.pred,"auc")
auc <- unlist(slot(auc, "y.values"))

importance(model.rf)

data.smote <- SMOTE(target.harm ~ ., data.m3, perc.over = 100)
table(data.smote$target.harm)
train <- data.smote

library(caret)

rfProfile <- rfe(data.m3[,-ncol(data.m3)], data.m3$target.harm, sizes = c(2,10),
                  rfeControl = rfeControl(functions = rfFuncs))

library(AUCRF)
fit <- AUCRF(target.harm ~ ., data=train, ntree=500, nodesize=2, strata=train$type, sampsize=c(SingleVO=.725*test.num, MultVic=.15*test.num, MultOff=.1*test.num, MultVO=.025*test.num))
summary(fit)
plot(fit)
OptimalSet(fit)

fitCV <- AUCRFcv(fit)
summary(fitCV)
plot(fitCV)
OptimalSet(fitCV)
