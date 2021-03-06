library(DMwR)
library(plyr)
library(corrplot)
library(ggmap)
library(gridExtra)

data.raw <- read.csv("NIBRS_datafinal.csv", header=FALSE, na.strings=c("NA"))

# Use codebook names for data processing for ease of reference

names.data <- c('SEGMENT', 'STATE', 'ORI', 'INCNUM', 'INCDATE', 'B1006', 'B1007', 
                'B1009', 'B1010', 'B1011', 'B2005', 'B2006', 'B3024', 'V1006', 
                'V1007', 'V20061', 'V20062', 'V20063', 'V20071', 'V20072', 'V20073', 
                'V20111', 'V20171', 'V20172', 'V40181', 'V40182', 'V40183', 'V40191', 
                'V40192', 'V40193', 'V40201', 'V40202', 'V40203', 'V40211', 'V40212', 'V40213',
                'V40221', 'V40222', 'V40223', 'V40231', 'V40232', 'V40261', 'V40262', 
                'V40263', 'V40321', 'V40322', 'V40323', 'V40341', 'V40342', 'V40343', 'V40361', 'V40362', 'V40363',
                'V50071', 'V50072', 'V50073','V50081', 'V50082', 'V50083','V50091', 'V50092','V50093', 'V60081', 'V60082', 
                'V60091', 'V60092', 'V60111', 'V60112')

colnames(data.raw) <- names.data

table(data.raw$V20061)

# Filter for all victims age <= 17 and Relationship (NIBRS definition)
data1 <- data.raw
m <- data1[,c("V40181","V40182","V40183")]
data1$agemax <- apply(m,1,max,na.rm=TRUE)

data.c <- subset(data1, agemax <= 17 & (V50071 >= 18 | V50071 < 0) & V40321 %in% c(4,5,7,10,11,12,14,15,16,17,19,24,25,-9,-8,-7), na.rm=TRUE)

#Change "Force" values other and none to smaller values to help with aggregation later
data.c$V20171[which(data.c$V20171>=900)] <- 1

#Recode victim is sibling to 9 to help with aggregation later
data.c$V40321[which(data.c$V40321==4)] <- 9
#Recode victim is step-child to 4 to help with aggregation later
data.c$V40321[which(data.c$V40321==10)] <- 4

# Set up target variable for harm
# Definition - any injury, any weapon even personal, additional UCR offense of homicide, sex offense, or assault
# Initially created a more granular level target 0=No harm, 1=Very minor harm, 2=Moderate/Serious harm

data.c$targ1 <- ifelse(((data.c$V40261<=2) & (data.c$V20171==400 | data.c$V20171<=1) & (data.c$V20062==133 | data.c$V20062<0) & !(data.c$V40261<=1 & data.c$V20171<=1 & data.c$V20062<0)),1,
                        ifelse((data.c$V40261>=3 | (data.c$V20171>100 & data.c$V20171<900) | (data.c$V20062>110 & data.c$V20062<133)), 2, 0))

# Create final modeling variable for only moderate/serious harm
data.c$target[which(data.c$targ1==0)] <- 0
data.c$target[which(data.c$targ1==1)] <- 0
data.c$target[which(data.c$targ1==2)] <- 1
sum(data.c$target)

#Create target variable for time from incident to arrest

data.c$time.arrest <- as.numeric(round(difftime(strptime(data.c$V60081, format="%Y%m%d"), strptime(data.c$INCDATE, format="%Y%m%d"), units='weeks'),1))

# Add in groupings
data.c$VNumVG <- ifelse(data.c$V40182==-6 | data.c$V40182==-8, 1, ifelse(data.c$V40183==-6 | data.c$V40183==-8,2,3))
data.c$VNumOG <- ifelse(data.c$V50072==-6 | data.c$V50072==-8, 1, ifelse(data.c$V50073==-6 | data.c$V50073==-8,2,3))

data.c$type <- as.factor(ifelse(data.c$VNumVG==1 & data.c$VNumOG==1, 'SingleVO', 
                                ifelse(data.c$VNumVG>1 & data.c$VNumOG==1, 'MultVic',
                                       ifelse(data.c$VNumVG==1 & data.c$VNumOG>1, 'MultOff',
                                              ifelse(data.c$VNumVG>1 & data.c$VNumOG>1, 'MultVO', NA)))))

prop.table(table(data.c$type))


#Identify incidents where limited personal information is provided
row.fill <- data.c[,c('V40191','V40201','V50071','V50081','V50091','V40321')]
numNAs <- apply(row.fill, 1, function(z) sum(ifelse(z<0,1,0)))
table(numNAs)
data.c <- data.c[which(numNAs < 4),]


# Reclass variety of unknowns to NA
data <- as.data.frame(lapply(data.c, function(x){replace(x ,x < 0, NA)}))                   
colnames(data)

# Count NA by variable and isolate fields with at least 1 NA

names.data <- c('SEGMENT', 'STATE', 'ORI', 'INCNUM', 'INCDATE', 'B1006', 'B1007', 
                'B1009', 'B1010', 'B1011', 'B2005', 'B2006', 'B3024', 'V1006', 
                'V1007', 'V20061', 'V20062', 'V20063', 'V20071', 'V20072', 'V20073', 
                'V20111', 'V20171', 'V20172', 'V40181', 'V40182', 'V40183', 'V40191', 
                'V40192', 'V40193', 'V40201', 'V40202', 'V40203', 'V40211', 'V40212', 'V40213',
                'V40221', 'V40222', 'V40223', 'V40231', 'V40232', 'V40261', 'V40262', 
                'V40263', 'V40321', 'V40322', 'V40323', 'V40341', 'V40342', 'V40343', 'V40361', 'V40362', 'V40363',
                'V50071', 'V50072', 'V50073','V50081', 'V50082', 'V50083','V50091', 'V50092','V50093', 'V60081', 'V60082', 
                'V60091', 'V60092', 'V60111', 'V60112', 
                'agemax', 'targ1','target','time.arrest', 'VNumVG', 'VNumOG', 'type')

data.empty = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))
}

x <- as.data.frame(cbind(data.empty, names.data))[order(-data.empty),]
x <- x[which(x$data.empty != 0),]
x



data.t <- data[,-which(names(data) %in% x[c(1:39),2])]

data.t$V40181G <- cut(data.t$V40181, 
                      breaks=c(-0.5,5.5,10.5,Inf), 
                      labels=c('0-5','6-10','11-15'))

table(data.t$V40181G)

data.t$victimtenderage.group <- cut(data.t$V40181, 
                                    breaks=c(-0.5,12.5,Inf), 
                                    labels=c('Yes','No'))

table(data.t$victimtenderage.group)

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

nrow(data[!complete.cases(data.t),])

write.csv(data.t, file="abduction_cleaned_coded.csv")


# Break to clear memory ---------------------------------------------------

data.t <- read.csv("abduction_cleaned_coded.csv")

data.graph <- data.t[,-which(names(data.t) %in% c('X','SEGMENT','B1009','V40311','agemax','targ1'))]
colnames(data.graph)

fullnames.data <- c('state','originating.agency', 'incident.number', 'incident.date', 'date.agency.went.NIBRS',
                    'city', 'country.division', 'country.region', 'current.population', 
                    'UCR.county', 'FIPS.county', 'report.date', 'incident.hour', 'UCR.code', 
                    'completed', 'location', 'force.used', 'victim.age', 'victim.sex', 
                  'victim.race', 'victim.ethnicity','victim.residency', 'injury', 'relationship',
                  'offender.age', 'offender.sex', 'offender.race','target.harm','target.arrest','multiple.victims',
                  'multiple.offenders','type','victimage.group','victim.tenderage','offenderage.group','population.group',
                'relationship.group','hour.group')

colnames(data.graph) <- fullnames.data

data.graph$same.race <- ifelse(data.graph$victim.race==data.graph$offender.race, 0, 1)

statemap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/statemap.csv")
data.merge <- merge(data.graph, statemap, by='state')

divmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/divmap.csv")
data.merge <- merge(data.merge, divmap, by='country.division',all.x=TRUE)

regionmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/regionmap.csv")
data.merge <- merge(data.merge, regionmap, by='country.region',all.x=TRUE)

locmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/locmap.csv")
data.merge <- merge(data.merge, locmap, by='location',all.x=TRUE)

weaponmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/weaponmap.csv")
data.merge <- merge(data.merge, weaponmap, by='force.used',all.x=TRUE)

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
data.final <- data.merge[,-which(names(data.merge) %in% c('relationship','injury','force.used','location',
                                                          'country.region', 'country.division',
                                                          'state','report.date'))]

write.csv(data.final, file="NIBRS_cleaned_notfilled.csv")

table(data.final$target.harm)

