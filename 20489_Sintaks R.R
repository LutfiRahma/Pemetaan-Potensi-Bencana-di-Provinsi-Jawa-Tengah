library(ca)
library(Hmisc)
library(devtools)
library(factoextra)
library(FactoMineR)
library(readxl)
library(magrittr)

Data_Bencana_Alam_Jateng <- read_excel("C:/Users/Hp/Downloads/Data Bencana Alam Jateng.xlsx")
data = Data_Bencana_Alam_Jateng
data

#ANALISIS WILAYAH POTENSI BENCANA ALAM
data1=as.data.frame(data[1:36])
data1
str(data1)

#melakukan pemilihan data yang akan digunakan
data_1 = data1[,-1]
row.names(data_1)=data1[,1]
data11=data_1 [-3,]
data11=data11[-3,]
data11=data11[-9,]
data11=data11[-7,]
#data11=data11[,] %>% scale()
data11=t(data11)
data11

##uji Chisq untuk setiap aspek yang ingin diuji
chisq.test(data11[-1,]) 

##profil baris 
profil.baris=prop.table(data11,1)
profil.baris

##profil kolom
profil.kolom =prop.table(data11,2)
profil.kolom

##Melakukan analisis korespondensi
kores1=ca(data11,graph=FALSE)
print(kores1)
summary(kores1)

##Plot korespondensi
plot(kores1)
plot.ca(kores1, what = c("all", "all"), mass = F, contrib="relative", main="Grafik Korespondensi")
fviz_ca_biplot(kores1, repel=T)
fviz_screeplot(kores1, addlabels = TRUE, ylim = c(0, 50))


