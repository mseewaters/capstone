library(zoo)
library(reshape)
library(plyr)


data.final <- read.csv("NIBRS_cleaned_notfilled_opt3.csv")

data <- subset(data.final, primary.incident == 1)
data$year <- substr(as.character(data$incident.date),0,4)

st.inc <- as.data.frame(unclass(xtabs(~state.name+year, data=data)))
st.inc <- cbind(state.name = rownames(st.inc), st.inc)
rownames(st.inc) <- NULL
colnames(st.inc) <- c('state.name','inc.2008','inc.2009','inc.2010','inc.2011','inc.2012')
st.inc$inc.3yr <- st.inc$inc.2010+st.inc$inc.2011+st.inc$inc.2012
st.inc$inc.5yr <- st.inc$inc.2008+st.inc$inc.2009+st.inc$inc.3yr

st.map <- ddply(data, c('region.name','division.name','state.name'), summarise, N=length(year))

st.reporting <- read.csv("ori_list_3yr.csv")

st.all <- merge(st.map, st.inc)
st.all <- merge(st.all, st.reporting)
st.all$rate.2010 <- st.all$inc.2010/st.all$report.2010*1000000
st.all$rate.2011 <- st.all$inc.2011/st.all$report.2011*1000000
st.all$rate.2012 <- st.all$inc.2012/st.all$report.2012*1000000
st.all$rate.3yr <- st.all$inc.3yr/st.all$report.3yr*1000000

write.csv(st.all, file='normalized_rates.csv')


# FIPS country ------------------------------------------------------------



fips.harm <- as.data.frame(unclass(xtabs(~FIPS+target.harm, data=data)))
fips.harm$rate <- fips.harm$'1'/(fips.harm$'1'+fips.harm$'0')*100


fips.inc <- as.data.frame(unclass(xtabs(~FIPS+year, data=data)))
fips.inc <- cbind(fipsate.name = rownames(fips.inc), fips.inc)
rownames(fips.inc) <- NULL
colnames(fips.inc) <- c('FIPS','inc.2008','inc.2009','inc.2010','inc.2011','inc.2012')
fips.inc$inc.3yr <- fips.inc$inc.2010+fips.inc$inc.2011+fips.inc$inc.2012
fips.inc$inc.5yr <- fips.inc$inc.2008+fips.inc$inc.2009+fips.inc$inc.3yr


write.csv(fips.inc, file='fips_rates.csv')

fips.inc <- as.data.frame(unclass(xtabs(~FIPS+relationship.group, data=data)))
fips.inc <- cbind(fipsate.name = rownames(fips.inc), fips.inc)
rownames(fips.inc) <- NULL

write.csv(fips.inc, file='fips_rates_relate.csv')

