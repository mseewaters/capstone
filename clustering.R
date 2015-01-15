nibrs.cluster <- read.csv("nibrs_cluster.csv")

nibrs.factor <- nibrs.cluster[,c(2,6,7,9:15,18)]

for (i in 1:(ncol(nibrs.factor)-1))
{
tbl <- table(nibrs.factor[,i], nibrs.factor$cluster)
print(colnames(nibrs.factor[i]))
print(chisq.test(tbl))
}
