library(randomForest)
library(DMwR)

#Load population demographic data
fips.data <- read.csv("fips_high_cor.csv")
fips.data$FIPS <- sprintf("%05d",fips.data$FIPS)
fips.data <- centralImputation(fips.data)

#Load NIBRS data and filter for 2010-12
data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")
data.final3$year <- as.numeric(substr(as.character(data.final3$incident.date),0,4))
data.final3.3yrs <- subset(data.final3,as.numeric(substr(as.character(data.final3$incident.date),0,4)) >=2010)

data.final3.3yrs.primary <- subset(data.final3.3yrs,data.final3.3yrs$primary.incident==1)
prop.table(table(data.final3.3yrs.primary$type))

data.m3.raw <- data.final3.3yrs[,c('FIPS','current.population','hour.group','victim.age','offenderage.group',
                                   'type','relationship.group',
                                   'division.name','loc.group','target.harm')]

data.m3merge <- merge(data.m3.raw,fips.data)


data.m3 <- data.m3merge[,c('current.population','victim.age','hour.group',
                           'offenderage.group','type',
                           'relationship.group','division.name',
                           'loc.group',
                           'nonrelatives','relover65','rent','college','samehouse','employed','income',
                           'target.harm')]


# Fill missing data, factors as 'unknown', numbers as mean

data.m3$target.harm <- as.factor(data.m3$target.harm)

for (i in c(3:8))
{
  data.m3[,i] <- as.character(data.m3[,i])
  data.m3[,i][is.na(data.m3[,i])] <- 'unknown'
  data.m3[,i] <- as.factor(data.m3[,i])
}
data.m3 <- centralImputation(data.m3)

test.num <- 4000
model.rf <- randomForest(target.harm ~ ., data=data.m3, ntree=200, mtry=10, strata=data.m3$type, sampsize=c(SingleVO=.725*test.num, MultVic=.13*test.num, MultOff=.12*test.num, MultVO=.025*test.num))
save(model.rf, file="NIBRS_rf.rda")

levels(data.m3$loc.group)


###


