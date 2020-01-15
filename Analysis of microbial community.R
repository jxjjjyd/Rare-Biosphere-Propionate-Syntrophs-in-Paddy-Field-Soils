install.packages("ggplot2")
library(ggplot2)
#Set theme and font
theme_set(theme_bw())
windowsFonts(myFont1=windowsFont("Times New Roman"))
mytheme<-theme(axis.title= element_text(size=25,family="myFont1"),axis.text=element_text(size=20,family="myFont1"),legend.title=element_blank(),legend.text=element_text(size=16,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"), panel.border=element_rect(color= "white"), axis.line=element_line(color= "black",size=0.5))
mytheme2<-theme(axis.title= element_text(size=25,family="myFont1"),axis.text=element_text(size=22,family="myFont1"),legend.title=element_blank(),legend.text=element_text(size=16,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"))+theme(panel.border = element_rect(size=1.5))

#OTU richness of syntrophs
data_all=read.table("richness_5all.csv",header=T, sep=",") 
p13<-ggplot(data_all,aes(x=lat,y=richness))+geom_point(aes(colour=color,shape=location),size=4)+scale_color_distiller(palette = "Spectral")+scale_shape_manual(values = c(19,15))+geom_smooth(method="lm",col="grey40",size=1,alpha=0.2)
p13<-p13+mytheme2+labs(y="OTU richness", x="Latitude (бу)")+theme(legend.text=element_text(size=16,family="myFont1"),legend.position = c(0.1,0.25),legend.title=element_blank(),legend.background = element_blank())+ylim(c(0,16))
print(p13)
ggsave(p13,filename = "richness_point_5all.tiff",width=6,height=6)

#Relative abundance of syntrohs in original soil samples
data1=read.table("group_compare4.csv",header=T, sep=",")
##Reorder the syntrophs in x-axis
data1$name<-factor(data1$name,levels=(c("Syntrophobacter","Pelotomaculum","Smithella","Desulfotomaculum","Syntrophomonas")),ordered=TRUE)
p6<-ggplot(data1,aes(x=name,y=value,fill=location))+geom_col( position="dodge")+mytheme2+ylim(c(0,0.11))+labs(y="Relative abundance (%)")+
theme(panel.border = element_rect(size=1.5),axis.text.x = element_text(angle=30,hjust=0.9,vjust=0.9,face="italic"),axis.title.x=element_blank(),legend.position=c(.85,.9))+
scale_fill_manual(values=c("#D73027","#4575B4"))
print(p6)
ggsave(p6,filename = "group_compare.tiff",width=6,height=6)

#Microbial community bar
install.packages("RColorBrewer")
library(RColorBrewer)
data=read.table("bar1.csv",header=T, sep=",")
data$group<-factor(data$group,levels=c("BN","BS","AN","AS"),ordered=TRUE)
data$Phylum<-factor(data$Phylum,levels=c("Acidobacteria","Actinobacteria","Bacteroidetes","Chloroflexi","Firmicutes","Gemmatimonadetes","Latescibacteria","Nitrospirae","Planctomycetes","Proteobacteria","Verrucomicrobia","Others"),ordered=TRUE)
##Create your own palette
brewer.pal(11,"Spectral")
mypalette<-colorRampPalette(c("#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2"))
mycolors<-mypalette(12)
p5<-ggplot(data,aes(group,100*abun,fill=Phylum))+geom_col( position="stack",width=0.75)+scale_y_continuous(name = "Relative Abundance (%)")+scale_x_discrete(name = "   ",labels=c("North", "South", "North", "South"))+mytheme
p5<-p5+scale_fill_manual(values=(mycolors))+theme(legend.text=element_text(size=16,family="myFont1",face="italic"))
print(p5)
ggsave(p5,filename = "bar.tiff",width=7,height=5)

#ж┴ diversity
data=read.table("a_diversity.csv",header=T, sep=",")
data$group<-factor(data$group,levels=c("BN","BS","AN","AS"),ordered=TRUE)
p2<-ggplot(data,aes(x=group,y=shannon,colour=group2))+geom_boxplot(alpha=0,size=1)+scale_y_continuous(name = "Shannon Index")+scale_x_discrete(name = "   ",labels=c("North", "South", "North", "South"))
p2<-p2+mytheme+geom_jitter(position=position_jitter(0.3),alpha=0.7,size=3)+theme(legend.position=c(0.87,0.9),legend.text=element_text(size=16,family="myFont1"))+scale_color_manual(values=c("#D73027","#4575B4"))
print(p2)
ggsave(p2,filename = "box.tiff",width=5,height=5)

#PCoA analysis
data=read.table("PCoA_1.csv",header=T, sep=",")
group=read.table("PCoA_group.csv",header=T, sep=",")
data<-merge(data, group,by='ID', all.x = TRUE)
p1<-ggplot(data,aes(x=PCoA1,y=PCoA2,color=location))+geom_point(aes(colour=location), size=4,alpha=0.7)+labs(y="PCoA2 (11.38%)", x="PCoA1 (20.35%)")+stat_ellipse(level=0.7,size=1)
p1<-p1+mytheme+theme(legend.text=element_text(size=16,family="myFont1"),legend.position = c(0.9,0.15),legend.title=element_blank(),legend.background = element_blank())+scale_color_manual(values=c("#D73027","#4575B4"))
print(p1)
ggsave(p1,filename = "PCoA.tiff",width=7,height=5)

#Relative abundance of syntrohs in soils after propionate incubation
data=read.table("4bacteria_abun_average_after.csv",header=T, sep=",")
data$bacteria<-factor(data$bacteria,levels=(c("Syntrophobacter","Syntrophomonas","Pelotomaculum","Smithella")),ordered=TRUE)
p11<-ggplot(data,aes(x=bacteria,y=value,fill=location))+geom_col( position="dodge",width=0.7)+mytheme2+labs(y="Relative abundance (%)")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle=15, hjust=.5, vjust=.5,face='italic'),legend.position=c(0.15,0.9))+scale_fill_manual(values=c("#D73027","#4575B4"))
print(p11)
ggsave(p11,filename = "4bacteria_abun_average_after.tiff",width=6,height=6)

#Fold changes of syntrophs after propionate incubaiton
data=read.table("bacteria_change3.csv",header=T, sep=",")
data$bacteria<-factor(data$bacteria,levels=(c("Syntrophobacter","Syntrophomonas","Pelotomaculum","Smithella")),ordered=TRUE)
p9<-ggplot(data,aes(x=bacteria,y=value,fill=location))+geom_col( position="dodge",width=0.7)+mytheme2+labs(y=expression('The change of syntrophs ('~log[2]~'R)'))+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle=15, hjust=.5, vjust=.5,face='italic'),legend.position=c(0.15,0.9))+ylim(c(0,7.5))+scale_fill_manual(values=c("#D73027","#4575B4"))
print(p9)
ggsave(p9,filename = "bacteria_change2.tiff",width=6,height=6)





