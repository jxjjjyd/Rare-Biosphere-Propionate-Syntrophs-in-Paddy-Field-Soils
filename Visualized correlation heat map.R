######Heatmap of the correlation between syntrophs and environmental factors#######
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
#Spearman correlation coefficients
data_r=read.table("relation_heatmap1.csv",header=T, sep=",",row.names =1,check.names = F)
#P value of coefficients
data_p=read.table("relation_heatmap2.csv",header=T, sep=",",row.names=1,check.names=F)
p<-as.matrix(data_p)
brewer.pal(11,"Spectral")
palette<-c("#9E0142","#9E0142","#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2","#5E4FA2","#5E4FA2")
tiff(file="relation_heatmap.tiff",width=6000,height=8000)
corrplot(t(data_r), cl.lim = c(-0.6,0.6),col=rev(palette),tl.col = "black", cl.pos = "r",p.mat =p,insig='label_sig',sig.level=c(.001,.01,.05),pch.cex=15,cl.cex=10,tl.cex=15,pch.col='black',cl.ratio=0.2,cl.align.text="r",outline="black",addgrid.col="black")
dev.off()
