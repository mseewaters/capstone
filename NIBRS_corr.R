library(corrplot)

#Load NIBRS data and filter for 2010-12
data.final3 <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")
data.final3$year <- as.numeric(substr(as.character(data.final3$incident.date),0,4))
data.final3.3yrs <- subset(data.final3,as.numeric(substr(as.character(data.final3$incident.date),0,4)) >=2010)


cor.data <- data.final3.3yrs[,c('current.population','incident.hour','victim.age','victim.sex',
                                'victim.race','victim.residency','offender.age','offender.sex',
                                'offender.race','multiple.victims','multiple.offenders', 'type',
                                'relationship.group','division.name','loc.group','same.race','UCR.code2',
                               'weapon.name','injury.name', 'target.harm')]


for (i in 1:length(cor.data))
{
  cor.data[,i] <- as.numeric(cor.data[,i])
}
c <- cor(na.omit(cor.data))
corrplot(c)
