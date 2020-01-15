#########Importance of environmental factors calculated by Boruta algorithm#######
install.packages("caret")
install.packages("Boruta")
library(ranger)
library(Boruta)
library(lattice)
library(ggplot2)
library(caret)
##Boruta package was used to to find all relevant environmental factors and estimate their importance  to the relative abundance of syntrophs

#desulfotomaculum
envi=read.table("randomforest2.csv",header=T, sep=",") 
set.seed(123)
boruta.train <-Boruta(Desulfotomaculum~MAT+OM+DOC+pH+CEC+MBC+TN+TP+TK+TS+TFe+TMn+TCu+TZn+NH4+NO3,data =envi,doTrace = 2)
print(boruta.train)
final.boruta<-TentativeRoughFix(boruta.train)
print(final.boruta)
boruta.df<-attStats(final.boruta)
write.csv(boruta.df,"boruta.df1.csv")

#pelotomaculum
set.seed(124)
boruta.train <-Boruta(Pelotomaculum~MAT+OM+DOC+pH+CEC+MBC+TN+TP+TK+TS+TFe+TMn+TCu+TZn+NH4+NO3,data =envi,doTrace = 2)
print(boruta.train)
final.boruta<-TentativeRoughFix(boruta.train)
print(final.boruta)
boruta.df<-attStats(final.boruta)
write.csv(boruta.df,"boruta.df2.csv")

#smithella
set.seed(125)
boruta.train <-Boruta(Smithella~MAT+OM+DOC+pH+CEC+MBC+TN+TP+TK+TS+TFe+TMn+TCu+TZn+NH4+NO3,data =envi,doTrace = 2)
print(boruta.train)
final.boruta<-TentativeRoughFix(boruta.train)
print(final.boruta)
boruta.df<-attStats(final.boruta)
write.csv(boruta.df,"boruta.df3.csv")

#syntrophobacter
set.seed(126)
boruta.train <-Boruta(Syntrophobacter~MAT+OM+DOC+pH+CEC+MBC+TN+TP+TK+TS+TFe+TMn+TCu+TZn+NH4+NO3,data =envi,doTrace = 2)
print(boruta.train)
final.boruta<-TentativeRoughFix(boruta.train)
print(final.boruta)
boruta.df<-attStats(final.boruta)
write.csv(boruta.df,"boruta.df4.csv")

#Syntrophomonas
set.seed(127)
boruta.train <-Boruta(Syntrophomonas~MAT+OM+DOC+pH+CEC+MBC+TN+TP+TK+TS+TFe+TMn+TCu+TZn+NH4+NO3,data =envi,doTrace = 2)
print(boruta.train)
final.boruta<-TentativeRoughFix(boruta.train)
print(final.boruta)
boruta.df<-attStats(final.boruta)
write.csv(boruta.df,"boruta.df5.csv")

#Integration above five csv files to boruta_bac.csv
#The meanImp value was used to represent the importance of environmental factors
#The meanImp value of rejected facors would be set to zero

library(ggplot2)
theme_set(theme_bw())
windowsFonts(myFont1=windowsFont("Times New Roman"))
mytheme<-theme(axis.title= element_text(size=20,family="myFont1"),axis.text=element_text(size=16,family="myFont1"),legend.title=element_blank(),legend.text=element_text(size=16,family="myFont1"))+theme(panel.grid.major=element_line(color= "white"), panel.grid.minor=element_line(color= "white"), panel.border=element_rect(color= "white"), axis.line=element_line(color= "black",size=0.5))

data=read.table("boruta_5bac.csv",header=T, sep=",")
data$envi<-factor(data$envi,levels=rev(c("MAT","OM","DOC","pH","CEC","MBC","TN","TP","TK","TS","TFe","TMn","TCu","TZn","NH4","NO3")),ordered=TRUE)
mycolors<-brewer.pal(5,"Spectral")
p1<-ggplot(data,aes(x=envi,y=value,fill=Syntrophs))+geom_col(position="stack",width=0.65)+coord_flip()+mytheme+labs(y="Importance")+theme(legend.text=element_text(size=20,family="myFont1"),axis.title.y=element_blank(),axis.title= element_text(size=20,family="myFont1"),axis.text=element_text(size=20,family="myFont1"),axis.text.y = element_blank(),legend.position="none")+scale_fill_manual(values=(mycolors))+scale_y_continuous(position = "right")
p2<-ggplot(data,aes(x=envi,y=value,fill=Syntrophs))+geom_col(position="stack",width=0.75)+coord_flip()+mytheme+labs(y="Importance")+theme(legend.text=element_text(size=20,family="myFont1"),axis.title.y=element_blank(),axis.title= element_text(size=20,family="myFont1"),axis.text=element_text(size=20,family="myFont1"),axis.text.y = element_blank(),legend.position="right")+scale_fill_manual(values=(mycolors))
print(p1)
ggsave(p1,filename = "5syntrophs_imp_boruta.tiff",width=2,height=11)

