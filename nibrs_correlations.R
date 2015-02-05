library(plyr)
library(corrplot)
library(Hmisc)

fips.cor <- read.csv("fips_correlation.csv")
fips.cor <- subset(fips.cor, inc.5yr >= 5)

cor.data = NULL
cor.sig = NULL
cor.name = NULL
rownum = 1

for (i in 13:ncol(fips.cor))
{
  cor.data[rownum] <- cor(fips.cor$inc.rates, as.numeric(fips.cor[,i]), use='pairwise.complete.obs')
  cor.sig[rownum] <- ifelse(cor.test(fips.cor$inc.rates, as.numeric(fips.cor[,i]),alternative='t')[3]>0.05,0,1)
  cor.name[rownum] <- colnames(fips.cor)[i]
  rownum = rownum+1
}
  

cor.all.inc <- cbind(cor.name, as.numeric(cor.data), as.numeric(cor.sig))
corr.all.inc.high <- as.data.frame(subset(cor.all.inc, (cor.data>=0.2 | cor.data<=-0.2)))

cor.data = NULL
cor.sig = NULL
cor.name = NULL
rownum = 1

for (i in 13:ncol(fips.cor))
{
  cor.data[rownum] <- cor(fips.cor$harm.rates, as.numeric(fips.cor[,i]), use='pairwise.complete.obs')
  cor.sig[rownum] <- ifelse(cor.test(fips.cor$harm.rates, as.numeric(fips.cor[,i]),alternative='t')[3]>0.05,0,1)
  cor.name[rownum] <- colnames(fips.cor)[i]
  rownum = rownum+1
}


cor.all.harm <- cbind(cor.name, as.numeric(cor.data), as.numeric(cor.sig))
corr.all.harm.high <- as.data.frame(subset(cor.all.harm, (cor.data>=0.2 | cor.data<=-0.2)))

cor.matrix <- cbind(fips.cor$harm.rates,fips.cor[,which(names(fips.cor) %in% corr.all.harm.high$cor.name)])
colnames(cor.matrix) <- c('harm','white','older res','divorce','highschool','gradschool','samestate','foreign','income','house','rent')
corrplot.mixed(cor(cor.matrix, use='pairwise.complete.obs'))




for (i in 13:ncol(fips.cor))
{
  fips.cor[,i] <- as.numeric(fips.cor[,i])

}

fips.lm <- fips.cor[,-c(1:11)]
colnames(fips.lm) <- c('rates','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
                       'p','q','r','s','t','u','v','w','x','y','z','aa','ab','ac','ad','ae','af','ag',
                       'ah','ai','aj','ak','al','am','an','ao',
                       'ap','aq','ar','as','at','au','av','aw','ax','ay','az','ba','bb',
                       'bc','bd','be','bf','bg','bh')

model <- lm(log(rates+1)~.,fips.lm)


library(MASS)
step <- stepAIC(model, direction="both")
step$anova # display results

model <- lm(log(rates + 1) ~ a + b + c + k + m + q + w + x + z + af + ag + 
              ai + aj + am + an + aq + ar + as + at + au + ba + bb + be + 
              bf + bh + u, fips.lm)

summary(model)

model <- lm(log(rates+1) ~ k + x + aj + aq +at + ba + bh, fips.lm)

summary(model)
histogram(sqrt(fips.lm$rates))
plot(model)


fips.model <- fips.cor[,c(2,16,17,19, 27,52,66,71)]
