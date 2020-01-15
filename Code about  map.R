################Kriging interpolation##############
library(automap)
library(ggplot2)
library(rgdal)

#Theme set
theme_set(theme_bw())
windowsFonts(myFont1=windowsFont("Times New Roman"))
mytheme<-theme(axis.title= element_text(size=20,family="myFont1"),axis.text=element_text(size=18,family="myFont1"),legend.title=element_blank(),legend.text=element_text(size=16,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"))+theme(panel.border = element_rect(size=1.5))
mytheme2<-theme(axis.title= element_text(size=25,family="myFont1"),axis.text=element_text(size=22,family="myFont1"),legend.title=element_blank(),legend.text=element_text(size=16,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"))+theme(panel.border = element_rect(size=1.5))

#Import the China map data
china_map<-readOGR("bou2_4p.shp")
china_line<-readOGR("bou2_4l.shp")
#Import the predict point data (transformed from grid)
point<-readOGR("point1.shp")
#Import the factors that need kriging interpolation in our study
data=read.table("all2.csv",header=T, sep=",")
data2=read.table("all2.csv",header=T, sep=",")

names(data)[names(data) == 'lon'] <- 'coords.x1'
names(data)[names(data) == 'lat'] <- 'coords.x2'
coordinates(data) =~coords.x1+coords.x2

#cross validation
#"Factor" represent the factor that you want to used in kriging interpolation (e.g. "time lapse")
#"Model" include:Nug,Exp,Sph,Gau,Exc,Mat,Ste,Cir,Lin,Bes,Pen,Per,Wav,Hol,Log,Pow,Spl
kr.cv=autoKrige.cv(Factor~1,data, nfold =113,model=c("Model"))
cv_output<-kr.cv$krige.cv_output@data
cor.test(cv_output[,1],cv_output[,3],method="pearson")

##Choose the best fit model from the above cross-validation results
#Kriging interpolation
kriging_result=autoKrige(Factor~1,data,point,model=c("Model"))
plot(kriging_result)

#Draw the map by ggplot2 package
pre_data<-kriging_result$krige_output@data
coord_data<-kriging_result$krige_output@coords
result<-data.frame(lon=coord_data[,1],lat=coord_data[,2],pre=pre_data[,1])
map<-ggplot()+geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="grey95")+
geom_point(data=result,aes(x=lon,y=lat,colour=pre))+scale_color_distiller(palette = "Spectral")+
geom_path(data=china_line,aes(x=long,y=lat,group=group),colour="grey40")+theme(legend.position = c(0.85,0.15))+
mytheme2+labs(x="Longtitude (бу)",y="Latitude (бу)")
print(map)
ggsave(map,filename = "G_Exp.tiff",width=6,height=6)




#######The results of kriging interpolation were used to predict the relative abundance of Pelotomaculum,Smithella and Syntrophomonas in some initial soil samples########
kr.cv1=autoKrige.cv(Pelotomaculum~1,data, nfold =113,model=c("Exp"))
cv_output1<-kr.cv1$krige.cv_output@data
kr.cv2=autoKrige.cv(Smithella~1,data, nfold =113,model=c("Exp"))
cv_output2<-kr.cv2$krige.cv_output@data
kr.cv3=autoKrige.cv(Syntrophomonas~1,data, nfold =113,model=c("Exp"))
cv_output3<-kr.cv3$krige.cv_output@data
cv_output_3all<-data.frame(pel=cv_output1[,1],smi=cv_output2[,1],bac=cv_output3[,1])
write.csv(cv_output_3all,file="pre_3all.csv")





##################Map with bar chart#############
# '#4575B4' blue '#D73027' red
map2<-ggplot()+geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="grey95")+geom_path(data=china_line,aes(x=long,y=lat,group=group),colour="grey40")+
geom_errorbar(aes(x=lon, ymin=lat, ymax=lat+maxrate), data=data2, size=1, color="#D73027", width=0, alpha=0.8)+mytheme2+ylim(c(18,55))+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank())
print(map2)
ggsave(map2,filename = "bar_maxrate.tiff",width=2.5,height=2)




########Maps with point representing different patterns of methanogenesis##################
map3<-ggplot()+geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="grey95")+geom_path(data=china_line,aes(x=long,y=lat,group=group),colour="grey40")+
geom_point(aes(x=lon, y=lat,colour=pattern,size=pattern), data=data2)+mytheme2+scale_color_manual(values= c("#ff4040","#00cd00","#0000ff"))+scale_size_manual(values = c(1.5,1.5,3))+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position=c(0.85,0.15))
print(map3)
ggsave(map3,filename = "pattern3.tiff",width=6,height=6)




 
