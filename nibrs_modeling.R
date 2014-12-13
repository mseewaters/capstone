library(DMwR)
library(performanceEstimation)
library(rpart)

data.final <- read.csv("NIBRS_cleaned_notfilled.csv")

data.m <- data.final[,c('current.population','hour.group','victim.age','victim.sex',
                        'victim.race','victim.residency','offender.age','offender.sex',
                        'offender.race','multiple.victims','multiple.offenders',
                        'relationship.group','division.name','location.group','target.harm')]

# Fill others
data.m <- centralImputation(data.m)

#Force to binary target
data.m$target.harm[which(data.m$target.harm==1)] <- 0
data.m$target.harm[which(data.m$target.harm==2)] <- 1
sum(data.m$target.harm)

data.m$target.harm <- as.factor(data.m$target.harm)
data.smote <- SMOTE(target.harm ~ ., data.m, perc.over = 150)
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
