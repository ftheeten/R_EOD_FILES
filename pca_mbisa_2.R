require(dplyr)
require(FactoMineR)
library("factoextra")
library("corrplot")

path<-"C:\\DEV\\R\\Mesures_ACP_30012022_rework.txt"
df <- read.table(path, sep = '\t',header = TRUE, dec=',')
df$especes_num<-as.numeric(as.factor(df$especes))
df$Specimens_num<-as.numeric(as.factor(df$Specimens))
y<-data.frame(species=df$especes_num)
x<- select(df, -especes, - Specimens, -especes_num, -Specimens_num)
norm<-data.frame(scale(x))

d_pca<-PCA(x, scale.unit = TRUE, ncp = 23, graph = TRUE)
print(d_pca)
eig.val <- get_eigenvalue(d_pca)
eig.val
grap<-fviz_eig(d_pca, addlabels = TRUE, ylim = c(0, 100))
var <- get_pca_var(d_pca)
var
fviz_pca_var(d_pca, col.var = "black")
grap2<-corrplot(var$contrib, is.corr=FALSE)   
fviz_pca_var(d_pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"))
fviz_contrib(d_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(d_pca, choice = "var", axes = 2, top = 10)
fviz_pca_ind (d_pca)
