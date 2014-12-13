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

# loadData ----------------------------------------------------------------
## Load the data into R
data.raw <- read.csv("2013_data_full.csv", stringsAsFactors=TRUE, na.strings = c("NA","N/A","U",""," ","None","Unknown","Unkn\nown","Unkno\nwn","Unknow\nn"))
names.data <- c("Victim.Age","Victim.Race","Victim.Gender","Relationship", "Harm",
                "Location","Region","RSO","Offender.Age","Offender.Race",
                "Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery.Status", "Motive","Year")

colnames(data.raw) <- names.data


# Remove lines where victim info is not provided
data.cln1 <- data.raw[complete.cases(data.raw[,c(1:3)]),]

# Remove all carriage returns
for (i in 1:length(data.cln1))
{
  if (class(data.cln1[,i]) == "factor") levels(data.cln1[,i]) <- gsub("\n","", levels(data.cln1[,i]))
}
summary(data.cln1)

# targetVariable ----------------------------------------------------------
# Create a list of harm responses and identify them as 1/0 and add to full data
harm.list <- as.data.frame(unique(unlist(data.cln1$Harm), use.names=FALSE))
colnames(harm.list) <- c("Harm")
harm.list$target <- c(0,0,1,1,1,0,1)
data.cln1 <- merge(data.cln1, harm.list, by = "Harm")
table(data.cln1$target)

#Flag sexually related offenses and incidents where victim is deceased as target=1
w <- which(data.cln1$target==1 | (data.cln1$Motive == "Sexual") | (is.na(data.cln1$Motive) & data.cln1$Recovery.Status == "Deceased") | !(data.cln1$Motive == "False Allegation" | data.cln1$Motive == "Runaway") & 
  data.cln1$Recovery.Status == "Deceased")
data.cln1$target[w] = 1
table(data.cln1$target)


# Recovery Location Analysis ----------------------------------------------
# Isolate only indicents where recovery location is known (38 obs as of dec 13, 2014)
data.rec <- data.cln1[!is.na(data.cln1$Recovery.Location),]



# Remove fields with >50% NA -----------------------------------------------------
# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
data.empty.perc = NULL
for (i in 1:length(data.cln1))
{
  data.empty[i] <- length(which(is.na(data.cln1[i])))
  data.empty.perc[i] <- round(length(which(is.na(data.cln1[i])))/nrow(data.cln1)*100,1) 
}
x <- as.data.frame(cbind(colnames(data.cln1), data.empty, data.empty.perc)[order(-data.empty),])
x <- x[which(x$data.empty != 0),]
x

# Remove factors with >50% NA, except Recovery.Location
data.cln1 <- data.cln1[,-which(names(data.cln1) %in% x[c(1:4),1])]



# additionalVariables -----------------------------------------------------
## Create additional variable groups as needed

# Create victim age group
data.cln1$Victim.AgeGroup <- cut(data.cln1$Victim.Age, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data.cln1$Victim.AgeGroup)

data.cln1$Offender.AgeGroup <- cut(data.cln1$Offender.Age,
                              breaks=c(0,20,30,40,50,Inf), 
                              labels=c('<20','20-30','30-40','40-50','>50'))

table(data.cln1$Offender.AgeGroup)

relate.list <- as.data.frame(unique(unlist(data.cln1$Relationship), use.names=FALSE))
colnames(relate.list) <- c("Relationship")
# Use this list to create the mapping file for relationships to NIBRS standard

relmap <- read.csv("relmapFBI.csv")
data.cln1 <- merge(data.cln1, relmap, by='Relationship',all.x=TRUE)

table(data.cln1$Relate.Group)


# Fill others
# data <- knnImputation(data, k=5)
# check <- nrow(data[!complete.cases(data),])

# Replace NA with "Unknown"
# data <- as.data.frame(apply(data.cln1, 2, function(x){x[which(is.na(x))] <- "Unknown";x}))  

write.csv(data.cln1, file="FBI_cleaned_notfilled.csv")
