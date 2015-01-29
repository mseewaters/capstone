library(DMwR)
library(plyr)
library(corrplot)


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


# Filter for all victims age <= 15 and Relationship (NIBRS definition)
data1 <- data.raw
m <- data1[,c("V40181","V40182","V40183")]
data1$agemax <- apply(m,1,max,na.rm=TRUE)

data.c <- subset(data1, agemax <= 15 & (V50071 >= 16 | V50071 < 0) & V40321 %in% c(4,5,7,10,11,12,14,15,16,17,19,24,25,-9,-8,-7), na.rm=TRUE)

#Change "Force" values other and none to smaller values to help with aggregation later
data.c$V20171[which(data.c$V20171>=900)] <- 1

#Recode victim is sibling to 9 to help with aggregation later
data.c$V40321[which(data.c$V40321==4)] <- 9
#Recode victim is step-child to 4 to help with aggregation later
data.c$V40321[which(data.c$V40321==10)] <- 4

# Add in groupings
data.c$VNumVG <- ifelse(data.c$V40182==-6 | data.c$V40182==-8, 1, ifelse(data.c$V40183==-6 | data.c$V40183==-8,2,3))
data.c$VNumOG <- ifelse(data.c$V50072==-6 | data.c$V50072==-8, 1, ifelse(data.c$V50073==-6 | data.c$V50073==-8,2,3))

data.c$type <- as.factor(ifelse(data.c$VNumVG==1 & data.c$VNumOG==1, 'SingleVO', 
                      ifelse(data.c$VNumVG>1 & data.c$VNumOG==1, 'MultVic',
                      ifelse(data.c$VNumVG==1 & data.c$VNumOG>1, 'MultOff',
                      ifelse(data.c$VNumVG>1 & data.c$VNumOG>1, 'MultVO', NA)))))

prop.table(table(data.c$type))

row.fill <- data.c[,c('V40191','V40201','V50071','V50081','V50091','V40321')]
numNAs <- apply(row.fill, 1, function(z) sum(ifelse(z<0,1,0)))
table(numNAs)
data.c <- data.c[which(numNAs < 4),]


# disaggregate multiple victims and offenders ----------------------------


data.general <- data.c[,c(1:24,40:41,63:72)]
data.general$primary.incident <- 0
data.general1 <- data.general
data.general1$primary.incident <- 1
data.v1o1 <- data.c[,c(25,28,31,34,37,42,45,54,57,60)]
data.v1o2 <- data.c[,c(25,28,31,34,37,42,48,55,58,61)]
data.v1o3 <- data.c[,c(25,28,31,34,37,42,51,56,59,62)]
data.v2o1 <- data.c[,c(26,29,32,35,38,43,46,54,57,60)]
data.v2o2 <- data.c[,c(26,29,32,35,38,43,49,55,58,61)]
data.v2o3 <- data.c[,c(26,29,32,35,38,43,52,56,59,62)]
data.v3o1 <- data.c[,c(27,30,33,36,39,44,47,54,57,60)]
data.v3o2 <- data.c[,c(27,30,33,36,39,44,50,55,58,61)]
data.v3o3 <- data.c[,c(27,30,33,36,39,44,53,56,59,62)]

data.general.all<- rbind(data.general1,data.general,data.general,data.general,data.general,data.general,data.general,data.general,data.general)
data.vo.all <- as.data.frame(rbind(as.matrix(data.v1o1), as.matrix(data.v1o2), as.matrix(data.v1o3), as.matrix(data.v2o1),
                                   as.matrix(data.v2o2), as.matrix(data.v2o3), as.matrix(data.v3o1),as.matrix(data.v3o2), as.matrix(data.v3o3)))
colnames(data.vo.all) <- c('victim.age','victim.sex','victim.race','victim.ethnicity','victim.resident','victim.injury','relationship','offender.age','offender.sex','offender.race')

data.all <- cbind(data.general.all, data.vo.all)
data.new <- data.all[(data.all$victim.age != -6 & data.all$victim.age != -8 & data.all$offender.age != -6 & data.all$offender.age != -8),]

# Set up target variable for harm
# Definition - any injury, any weapon even personal, additional UCR offense of homicide, sex offense, or assault
# Initially created a more granular level target 0=No harm, 1=Very minor harm, 2=Moderate/Serious harm

data.new$targ1 <- ifelse(((data.new$victim.injury<=2) & (data.new$V20171==400 | data.new$V20171<=1) & (data.new$V20062==133 | data.new$V20062<0) & !(data.new$victim.injury<=1 & data.new$V20171<=1 & data.new$V20062<0)),1,
                        ifelse((data.new$victim.injury>=3 | (data.new$V20171>100 & data.new$V20171<900) | (data.new$V20062>90 & data.new$V20062<133)), 2, 0))

# Create final modeling variable for only moderate/serious harm
data.new$target <- data.new$targ1
data.new$target[which(data.new$targ1==0)] <- 0
data.new$target[which(data.new$targ1==1)] <- 0
data.new$target[which(data.new$targ1==2)] <- 1
sum(data.new$target)


#Create target variable for time from incident to arrest

data.new$time.arrest <- as.numeric(round(difftime(strptime(data.new$V60081, format="%Y%m%d"), strptime(data.new$INCDATE, format="%Y%m%d"), units='weeks'),1))


# Reclass variety of unknowns to NA
data <- as.data.frame(lapply(data.new, function(x){replace(x ,x < 0, NA)}))                   

# Count NA by variable and isolate fields with at least 1 NA
namemap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/fieldnamemap.csv")

names.data <- c('SEGMENT', 'STATE', 'ORI', 'INCNUM', 'INCDATE', 'B1006', 'B1007', 
                'B1009', 'B1010', 'B1011', 'B2005', 'B2006', 'B3024', 'V1006', 
                'V1007', 'V20061', 'V20062', 'V20063', 'V20071', 'V20072', 'V20073', 
                'V20111', 'V20171', 'V20172', 'V40231', 'V40232', 'V60081', 'V60082', 
                'V60091', 'V60092', 'V60111', 'V60112', 'agemax', 'VNumVG','VNumOG', 'type','primary.incident',
                'victim.age','victim.sex','victim.race','victim.ethnicity','victim.resident','victim.injury',
                'relationship','offender.age','offender.sex','offender.race',
                'targ1','target','time.arrest')

data.empty = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))  
}

x <- merge(as.data.frame(cbind(names.data, data.empty)), namemap, by='names.data')
x <- x[order(-data.empty),]
x <- x[which(x$data.empty != 0),]
x



data.t <- data[,-which(names(data) %in% x[c(1:13),1])]

data.t$victimage.group <- cut(data.t$victim.age, 
                      breaks=c(-0.5,5.5,10.5,Inf), 
                      labels=c('0-5','6-10','11-15'))

table(data.t$victimage.group)

data.t$offenderage.group <- cut(data.t$offender.age,
                      breaks=c(0,20,30,40,50,Inf), 
                      labels=c('<20','20-30','30-40','40-50','>50'))

table(data.t$offenderage.group)

data.t$B2005G <- cut(data.t$B2005,
                     breaks=c(0,25000,100000,500000,Inf), 
                     labels=c('Pop <25K','Pop 25-100K','Pop 100-500K','Pop >500K'))

table(data.t$B2005G)

data.t$rel.group <- cut(data.t$relationship,
                      breaks=c(0,6,13,24,Inf), 
                      labels=c('Parent','Family','Known','Stranger'))

table(data.t$rel.group)

data.t$V1007G <- cut(data.t$V1007,
                     breaks=c(-1,1,6,12,18,Inf), 
                     labels=c('Night','Early','Morning','Afternoon','Night2'))
#Recode Night2 to Night
data.t$V1007G[which(data.t$V1007G=='Night2')] <- 'Night'

table(data.t$V1007G)

nrow(data[!complete.cases(data.t),])

write.csv(data.t, file="abduction_cleaned_coded_opt3.csv")


# Break to clear memory ---------------------------------------------------

data.t <- read.csv("abduction_cleaned_coded_opt3.csv")

data.graph <- data.t[,-which(names(data.t) %in% c('X','SEGMENT','B1009','agemax','targ1'))]
colnames(data.graph)

fullnames.data <- c('state','originating.agency', 'incident.number', 'incident.date', 'date.agency.went.NIBRS',
                    'city', 'country.division', 'country.region', 'current.population', 
                    'UCR.county', 'FIPS.county', 'report.date', 'incident.hour', 'UCR.code', 
                    'completed', 'location', 'force.used', 'multiple.victims',
                    'multiple.offenders', 'type', 'primary.incident','victim.age', 'victim.sex', 
                  'victim.race', 'victim.ethnicity', 'victim.residency', 'injury', 'relationship',
                  'offender.age', 'offender.sex', 'offender.race','target.harm','target.arrest','victimage.group',
                  'offenderage.group','population.group',
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

weapmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/weaponmap.csv")
data.merge <- merge(data.merge, weapmap, by='force.used', all.x=TRUE)

injmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/injmap.csv")
data.merge <- merge(data.merge, injmap, by='injury',all.x=TRUE)

relmap <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/relmap.csv")
data.merge <- merge(data.merge, relmap, by='relationship',all.x=TRUE)


data.merge$victim.sex <- factor(data.merge$victim.sex, labels=c('female','male'))
data.merge$offender.sex <- factor(data.merge$offender.sex, labels=c('female','male'))
data.merge$victim.race <- factor(data.merge$victim.race, labels=c('white','black','amer.indian','asian'))
data.merge$offender.race <- factor(data.merge$offender.race, labels=c('white','black','amer.indian','asian'))
data.merge$victim.residency <- factor(data.merge$victim.residency, labels=c('non-resident','resident'))
data.merge$victim.ethnicity <- factor(data.merge$victim.ethnicity, labels=c('not.hispanic','hispanic'))

data.merge$FIPS <- sprintf("%02d%03d",data.merge$state, data.merge$FIPS.county)
data.final <- data.merge[,-which(names(data.merge) %in% c('relationship','injury','force.used','location',
                                                          'country.region', 'country.division',
                                                          'state','report.date'))]

write.csv(data.final, file="NIBRS_cleaned_notfilled_opt3.csv")

table(data.final$target.harm)

