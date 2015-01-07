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

data.raw <- read.csv("NIBRS_datafull.csv", header=FALSE, na.strings=c("NA"))

# Use codebook names for data processing for ease of reference

names.data <- c('SEGMENT', 'STATE', 'ORI', 'INCNUM', 'INCDATE', 'B1006', 'B1007', 
                'B1009', 'B1010', 'B1011', 'B2005', 'B2006', 'B3024', 'V1006', 
                'V1007', 'V20061', 'V20062', 'V20063', 'V20071', 'V20072', 'V20073', 
                'V20111', 'V20171', 'V20172', 'V40181', 'V40182', 'V40183', 'V40191', 
                'V40192', 'V40193', 'V40201', 'V40202', 'V40203',
                'V40221', 'V40222', 'V40223', 'V40231', 'V40232', 'V40261', 'V40262', 
                'V40263', 'V40311', 'V40312', 'V40313', 'V40321', 'V40322', 'V40323', 
                'V40331', 'V40332', 'V40333', 'V40341', 'V40342', 'V40343', 'V50071', 
                'V50072', 'V50081', 'V50082', 'V50091', 'V50092', 'V60081', 'V60082', 
                'V60091', 'V60092', 'V60111', 'V60112')

colnames(data.raw) <- names.data


# Filter for all victims age <= 15 and Relationship (NIBRS definition)
data1 <- data.raw
m <- data1[,c("V40181","V40182","V40183")]
data1$agemax <- apply(m,1,max,na.rm=TRUE)

data.c <- subset(data1, agemax <= 15 & V50071 >= 16 & V40321 %in% c(4,5,7,10,11,12,14,15,16,17,19,24,25,-9,-8,-7), na.rm=TRUE)

#Change "Force" values other and none to smaller values
data.c$V20171[which(data.c$V20171>=900)] <- 1

#Recode victim is sibling to 9 to help with aggregation later
data.c$V40321[which(data.c$V40321==4)] <- 9
#Recode victim is step-child to 4 to help with aggregation later
data.c$V40321[which(data.c$V40321==10)] <- 4

# Set up target variable
# Definition - any injury, even minor, any weapon even personal
# Additional UCR offense of homicide, sex offense, or assault

data.c$target <- ifelse(((data.c$V40261<=2) & (data.c$V20171==400 | data.c$V20171<=1) & (data.c$V20062==133 | data.c$V20062<0) & !(data.c$V40261<=1 & data.c$V20171<=1 & data.c$V20062<0)),1,
                        ifelse((data.c$V40261>=3 | (data.c$V20171>100 & data.c$V20171<900) | (data.c$V20062>110 & data.c$V20062<133)), 2, 0))
sum(data.c$target)


# Filter abduction incidents
data.100 <- subset(data.c, V20061==100 | V20062==100 | V20063==100)

# Filter for all major crimes
# data.100 <- subset(data.c, V20061<200 & V20062<200 & V20063<200)


# Reclass variety of unknowns to NA
data <- as.data.frame(lapply(data.100, function(x){replace(x ,x < 0, NA)}))                   

# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
for (i in 1:length(data.raw))
{
  data.empty[i] <- length(which(is.na(data[i])))  
}
x <- as.data.frame(cbind(names.data, data.empty)[order(-data.empty),])

x <- x[which(x$data.empty != 0),]
x

# Add in groupings
data$VNumVG <- as.factor(ifelse(!is.na(data$V40182), "Multiple Victims", "Single Victim"))
data$VNumOG <- as.factor(ifelse(!is.na(data$V40331), "Multiple Offenders", "Single Offender"))

data.t <- data[,-which(names(data) %in% x[1:36,1])]

data.t$V20111G <- as.factor(ifelse(data.t$V20111 == 20, "Home", ifelse((data.t$V20111 == 44 | data.t$V20111 == 52 | data.t$V20111 == 53 | data.t$V20111 == 57 | data.t$V20111 == 22), "School/Center","Other")))

data.t$V40181G <- cut(data.t$V40181, 
                      breaks=c(-0.5,5.5,10.5,Inf), 
                      labels=c('0-5','6-10','11-15'))

table(data.t$V40181G)

data.t$V50071G <- cut(data.t$V50071,
                      breaks=c(0,20,30,40,50,Inf), 
                      labels=c('<20','20-30','30-40','40-50','>50'))

table(data.t$V50071G)

data.t$B2005G <- cut(data.t$B2005,
                     breaks=c(0,25000,100000,500000,Inf), 
                     labels=c('Pop <25K','Pop 25-100K','Pop 100-500K','Pop >500K'))

table(data.t$B2005G)

data.t$V40321G <- cut(data.t$V40321,
                      breaks=c(0,6,13,24,Inf), 
                      labels=c('Parent','Family','Known','Stranger'))

table(data.t$V40321G)

data.t$V1007G <- cut(data.t$V1007,
                     breaks=c(-1,1,6,12,18,Inf), 
                     labels=c('Night','Early','Morning','Afternoon','Night2'))
#Recode Night2 to Night
data.t$V1007G[which(data.t$V1007G=='Night2')] <- 'Night'

table(data.t$V1007G)
xtabs(target~V1007G, data.t)

check <- nrow(data[!complete.cases(data.t),])

write.csv(data.t, file="abduction_cleaned_coded.csv")
# write.csv(data.t, file="allcrime_cleaned_coded.csv")

# Break to clear memory ---------------------------------------------------

data.t <- read.csv("abduction_cleaned_coded.csv")
# data.t <- read.csv("allcrime_cleaned_coded.csv")

data.graph <- data.t[,-which(names(data.t) %in% c('X','SEGMENT','V40311','agemax'))]
colnames(data.graph)

fullnames.data <- c('state','originating.agency', 'incident.number', 'incident.date', 'date.agency.went.NIBRS',
                    'city', 'population.group', 'country.division', 'country.region', 'current.population', 
                    'UCR.county', 'FIPS.county', 'report.date', 'incident.hour', 'UCR.code', 
                    'completed', 'location', 'Weapon.force', 'victim.age', 'victim.sex', 
                  'victim.race', 'victim.residency', 'injury', 'relationship',
                  'offender.age', 'offender.sex', 'offender.race','target.harm','multiple.victims',
                  'multiple.offenders','location.group','victimage.group','offenderage.group','population.quartile',
                'relationship.group','hour.group')

colnames(data.graph) <- fullnames.data


statemap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/statemap.csv")
data.merge <- merge(data.graph, statemap, by='state')

popgroupmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/popgroupmap.csv")
data.merge <- merge(data.merge, popgroupmap, by='population.group',all.x=TRUE)

divmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/divmap.csv")
data.merge <- merge(data.merge, divmap, by='country.division',all.x=TRUE)

regionmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/regionmap.csv")
data.merge <- merge(data.merge, regionmap, by='country.region',all.x=TRUE)

locmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/locmap.csv")
data.merge <- merge(data.merge, locmap, by='location',all.x=TRUE)

weaponmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/weaponmap.csv")
data.merge <- merge(data.merge, weaponmap, by='Weapon.force',all.x=TRUE)

injmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/injmap.csv")
data.merge <- merge(data.merge, injmap, by='injury',all.x=TRUE)

relmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/relmap.csv")
data.merge <- merge(data.merge, relmap, by='relationship',all.x=TRUE)


data.merge$victim.sex <- factor(data.merge$victim.sex, labels=c('female','male'))
data.merge$offender.sex <- factor(data.merge$offender.sex, labels=c('female','male'))
data.merge$victim.race <- factor(data.merge$victim.race, labels=c('white','black','amer.indian','asian'))
data.merge$offender.race <- factor(data.merge$offender.race, labels=c('white','black','amer.indian','asian'))
data.merge$victim.residency <- factor(data.merge$victim.residency, labels=c('non-resident','resident'))

data.merge$FIPS <- sprintf("%02d%03d",data.merge$state, data.merge$FIPS.county)
data.final <- data.merge[,-which(names(data.merge) %in% c('relationship','injury','Weapon.force','location',
                                                          'country.region', 'country.division','population.group',
                                                          'state','report.date'))]

write.csv(data.final, file="NIBRS_cleaned_notfilled.csv")
# write.csv(data.final, file="NIBRS_allcrime_notfilled.csv")


xtabs(target.harm~UCR.code, data.final)


# create data and rates by ori and fips --------------------------------------------


library(plyr)
ori.pop <- ddply(data.raw, ~ORI,summarise,mean=mean(B2005))
write.csv(ori.pop, file="ori_pop.csv")

fips.rates <- as.data.frame.matrix(table(data.final$FIPS, data.final$UCR.code))
write.csv(fips.rates, file="crime_by_fips.csv")

