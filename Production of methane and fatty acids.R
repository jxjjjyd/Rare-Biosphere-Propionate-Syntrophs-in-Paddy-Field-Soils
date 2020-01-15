
install.packages("ggplot2")
library(ggplot2)
#################Line chart: methane###############
#Import datas of different patterns of methane production
ch1=read.table("ch4_pattern1.csv",header=T, sep=",") 
ch2=read.table("ch4_pattern2.csv",header=T, sep=",") 
ch3=read.table("ch4_pattern3.csv",header=T, sep=",") 
#Set theme
theme_set(theme_bw())
windowsFonts(myFont1=windowsFont("Times New Roman"))
mytheme<-theme(axis.title= element_text(size=65,family="myFont1"),axis.text=element_text(size=65,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"), panel.border=element_rect(color= "white"), axis.line=element_line(color= "black",size=0.5))

pattern1<-ggplot(ch1, aes(x=time1, y=ch4_1, colour=supp1,group=supp1))+geom_errorbar(aes(ymin=ch4_1-sd1,ymax=ch4_1+sd1,colour=supp1),width=1)+
geom_line(aes(colour=supp1),size=1.5)+geom_point(size=8,shape=16)+labs(y=expression(CH[4]~' (mmol '~L^'-1'~')'), x="Time (d)")+
annotate("text",x=5, y=33,label="Pattern I",size=28,family="myFont1")+geom_vline(xintercept=c(13,27), linetype="dashed",size=1)+mytheme+theme(legend.position="none")+ylim(0,35)
pattern2<-ggplot(ch2, aes(x=time2, y=ch4_2, colour=supp2,group=supp2))+geom_errorbar(aes(ymin=ch4_2-sd2,ymax=ch4_2+sd2,colour=supp2),width=1)+
geom_line(aes(colour=supp2),size=1.5)+geom_point(size=8,shape=17)+labs(y=expression(CH[4]~' (mmol '~L^'-1'~')'), x="Time (d)")+
annotate("text",x=9.5, y=33,label="Pattern II",size=28,family="myFont1")+geom_vline(xintercept=c(27,43), linetype="dashed",size=1)+mytheme+theme(legend.position="none")+ylim(0,35)
pattern3<-ggplot(ch3, aes(x=time3, y=ch4_3, colour=supp3,group=supp3))+geom_errorbar(aes(ymin=ch4_3-sd3,ymax=ch4_3+sd3,colour=supp3),width=1)+
geom_line(aes(colour=supp3),size=1.5)+geom_point(size=8,shape=18)+labs(y=expression(CH[4]~' (mmol '~L^'-1'~')'), x="Time (d)")+
annotate("text",x=19, y=28,label="Pattern III",size=28,family="myFont1")+geom_vline(xintercept=c(43,82), linetype="dashed",size=1)+mytheme+theme(legend.position="none")+ylim(0,30)

library(grid)
tiff(file="ch4.tif",width=2480,height=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout<-function(x,y)
viewport(layout.pos.row=x,layout.pos.col=y)
print(pattern1,vp=vplayout(1,1))
print(pattern2,vp=vplayout(1,2))
print(pattern3,vp=vplayout(1,3))
dev.off


############Point plot: fatty acids#####################
##Acetate
ace1=read.table("ace_pattern1.csv",header=T, sep=",") 
ace2=read.table("ace_pattern2.csv",header=T, sep=",") 
ace3=read.table("ace_pattern3.csv",header=T, sep=",") 
ace1<-ggplot(ace1, aes(x=time1, y=ace_1, colour=supp1,group=supp1))+
geom_point(size=8,alpha=0.7,shape=16)+labs(y=expression('Acetate (mmol '~L^'-1'~')'))+scale_y_continuous(limits=c(0,4.5),breaks=round(seq(0,4.5,length.out=5),2))+theme(axis.title.x=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none")+xlim(c(0,81))
ace2<-ggplot(ace2, aes(x=time2, y=ace_2, colour=supp2,group=supp2))+
geom_point(size=8,alpha=0.7,shape=17)+labs(y=expression('Acetate (mmol '~L^'-1'~')'))+scale_y_continuous(limits=c(0,4.5),breaks=round(seq(0,4.5,length.out=5),2))+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))
ace3<-ggplot(ace3, aes(x=time3, y=ace_3, colour=supp3,group=supp3))+
geom_point(size=8,alpha=0.7,shape=18)+labs(y=expression('Acetate (mmol '~L^'-1'~')'))+scale_y_continuous(limits=c(0,4.5),breaks=round(seq(0,4.5,length.out=5),2))+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))
##Propionate
pro1=read.table("pro_pattern1.csv",header=T, sep=",") 
pro2=read.table("pro_pattern2.csv",header=T, sep=",") 
pro3=read.table("pro_pattern3.csv",header=T, sep=",") 
pro1<-ggplot(pro1, aes(x=time1, y=pro_1, colour=supp1,group=supp1))+
geom_point(size=8,alpha=0.7,shape=16)+labs(y=expression('Propionate (mmol '~L^'-1'~')'))+ylim(0,12)+theme(axis.title.x=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none")+xlim(c(0,81))
pro2<-ggplot(pro2, aes(x=time2, y=pro_2, colour=supp2,group=supp2))+
geom_point(size=8,alpha=0.7,shape=17)+labs(y=expression('Propionate (mmol '~L^'-1'~')'))+ylim(0,12)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))
pro3<-ggplot(pro3, aes(x=time3, y=pro_3, colour=supp3,group=supp3))+
geom_point(size=8,alpha=0.7,shape=18)+labs(y=expression('Propionate (mmol '~L^'-1'~')'))+ylim(0,12)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))
##Butyrate
buty1=read.table("buty_pattern1.csv",header=T, sep=",") 
buty2=read.table("buty_pattern2.csv",header=T, sep=",") 
buty3=read.table("buty_pattern3.csv",header=T, sep=",") 
buty1<-ggplot(buty1, aes(x=time1, y=buty_1, colour=supp1,group=supp1))+
geom_point(size=8,alpha=0.7,shape=16)+labs(y=expression('Butyrate (mmol '~L^'-1'~')'), x="Time(d)")+ylim(0,1)+theme(axis.title.x=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none")+xlim(c(0,81))
buty2<-ggplot(buty2, aes(x=time2, y=buty_2, colour=supp2,group=supp2))+
geom_point(size=8,alpha=0.7,shape=17)+labs(y=expression('Butyrate (mmol '~L^'-1'~')'), x="Time(d)")+ylim(0,1)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))
buty3<-ggplot(buty3, aes(x=time3, y=buty_3, colour=supp3,group=supp3))+
geom_point(size=8,alpha=0.7,shape=18)+labs(y=expression('Butyrate (mmol '~L^'-1'~')'), x="Time(d)")+ylim(0,1)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.title= element_text(size=60,family="myFont1"),axis.text=element_text(size=50,family="myFont1"),legend.position="none",axis.text.y = element_blank())+xlim(c(0,81))

library(grid)
tiff(file="VFA2.tif",width=3000,height=2106)
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,3)))
vplayout<-function(x,y)
viewport(layout.pos.row=x,layout.pos.col=y)
print(ace1,vp=vplayout(2,1))
print(ace2,vp=vplayout(2,2))
print(ace3,vp=vplayout(2,3))
print(pro1,vp=vplayout(1,1))
print(pro2,vp=vplayout(1,2))
print(pro3,vp=vplayout(1,3))
print(buty1,vp=vplayout(3,1))
print(buty2,vp=vplayout(3,2))
print(buty3,vp=vplayout(3,3))
dev.off