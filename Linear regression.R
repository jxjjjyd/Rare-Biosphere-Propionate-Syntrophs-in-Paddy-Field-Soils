#######The associations between time lapse and properties of uncultured soil##########
library(ggplot2)

data_all=read.table("all.csv",header=T, sep=",") 
data2=read.table("initial_syn_soil_lagphase.csv",header=T, sep=",")
#set theme
theme_set(theme_bw())
windowsFonts(myFont1=windowsFont("Times New Roman"))
mytheme<-theme(axis.title= element_text(size=70,family="myFont1"),axis.text=element_text(size=65,family="myFont1"),legend.position = "none")

#synTotal
p1<-ggplot(data2,aes(x=Total,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x="Rel. abun. of synTotal (¡ë)")+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+theme(axis.title.y=element_blank())
#MAT
p2<-ggplot(data_all,aes(x=MAT,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x="MAT (¡æ)")+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())
#TS
p3<-ggplot(data_all,aes(x=TS,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('TS (g '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())+xlim(0,2.05)
#OM
p4<-ggplot(data_all,aes(x=OM,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('OM (g '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+xlim(c(0,105))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())
#MBC
p5<-ggplot(data_all,aes(x=MBC,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('MBC (mg '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+xlim(c(0,1550))+ylim(c(-5,55))+theme(axis.title.y=element_blank())
#AFe
p6<-ggplot(data_all,aes(x=AFe,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('AFe (mg '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+xlim(c(0,112))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())
#ACu
p7<-ggplot(data_all,aes(x=ACu,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('ACu (mg '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())
#AMn
p8<-ggplot(data_all,aes(x=AMn,y=lagphase))+geom_point(aes(colour=supp), size=7)+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)+labs(y="Lag phase (d)", x=expression('AMn (mg '~kg^'-1'~')'))+mytheme+scale_color_distiller(palette = "Spectral")+ylim(c(-5,55))+xlim(c(0,80))+theme(axis.title.y=element_blank(),axis.text.y = element_blank())

library(grid)
tiff(file="lagphase_sel2.tif",width=3500,height=1800)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,4)))
vplayout<-function(x,y)
viewport(layout.pos.row=x,layout.pos.col=y)
print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))
print(p3,vp=vplayout(1,3))
print(p4,vp=vplayout(1,4))
print(p5,vp=vplayout(2,1))
print(p6,vp=vplayout(2,2))
print(p7,vp=vplayout(2,3))
print(p8,vp=vplayout(2,4))
dev.off
