require(dplyr)
require(stats)
library(qcc)
require(caret)

path<-"C:\\DEV\\R\\Mesures_ACP_30012022_rework.txt"
df <- read.table(path, sep = '\t',header = TRUE, dec=',')
df$especes_num<-as.numeric(as.factor(df$especes))
y<-data.frame(species=df$especes_num)
x<- select(df, -especes, - Specimens, -especes_num)

pairs(c(x,y), diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth(), main="evaluation des relations")

#View(x)
pairs(c(x,y))
str(x)
str(y)

pca<-prcomp(data.frame(scale(x)))
vpca<-pca$sdev^2
pareto.chart(vpca, xlab="Composante", ylab="Variance", main="Pareto des dimensions")

t1<-preProcess(x, method="YeoJohnson")
t1
x_1<-predict(t1,x)
t2<-preProcess(x_1, method="pca", tresh=0.95)
t2
x_2<-predict(t2,x_1)
t3<-preProcess(x_2, method="range")
t3
x_3<-predict(t3,x_2 )
pairs(c(x_3,y))


