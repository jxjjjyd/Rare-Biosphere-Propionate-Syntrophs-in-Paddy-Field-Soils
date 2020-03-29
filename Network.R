install.packages("psych")
library(psych)

install.packages("BiocManager") 
BiocManager::install("WGCNA") 
library(WGCNA)

#Import a a community table with samples as rows and taxa as columns
data2=read.table("network2.csv",sep=",",head=T, row.names=1,check.names=F)

#Calculate Pearson¡¯s correlation coefficient
occor2 = corAndPvalue(data2,method="pearson",use="p")
occor2.r = occor2$cor
occor2.p = occor2$p

#Select the Pcoefficient of >0.6 and P value of <0.01
occor2.r[occor2.p>0.01|abs(occor2.r)<0.6] = 0

write.csv(occor2.r,file="network_0.01occor2.csv")

