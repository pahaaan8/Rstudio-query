# Praktikum 5 APG
# 5 Desember 2018
# ANALISIS FAKTOR


#Load Packages
library(psych)
install.packages("fmsb")
library(fmsb)

data = read.delim("clipboard")
str(data) #jumlah observasi dan berapa variabel
head(data)
summary(data)


##Uji Validitas dan Reliabilitas
#Validitas
cor(data)  #data pakai total

#Reliabilitas
CronbachAlpha(data) #packages fmbs
alpha(data) #packages psych


##Korelasi Data, untuk lihat multikolinieritas
alpha(data)


##Uji Asumsi
#Uji kecukupan sampel dengan KMO/MSA (digunakan hanya jika mengambil sampel) dan uji barlett
#Jika nilai KMO berada dalam selang 0.5 sampai 1, maka asumsi terpenuhi
#Jika nilai MSA anti image lebih besar dari 0.5 maka X berpengaruh terhadap faktor
KMO(data) #melihat anti image korelasi juga

#Kehomogenan Varians (uji Barlet)
#H0:Data homogen
#H1:Data tidak homogen
bartlett.test(list(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6],data[,7],data[,8],data[,9],data[,10],data[,11],data[,12],data[,13],data[,14],data[,15],data[,16],data[,17],data[,18]))


##Pembentukan faktor
#Penentuan jumlah faktor
#Menggunakan PCA
data.pca = princomp(data)
summary(data.pca)

#Penentuan explained variance
cov_data = cov(data)
eigen_data = eigen(cov_data)
eigen_data
PVE = eigen_data$values / sum(eigen_data$values)
round(PVE, 4)


##Penentuan faktor
data.fa = factanal(data, factors = 4, scores = "regression", rotation = "varimax")
#Jelaskan loadings
#Uji kecukupan jumlah faktor
#H0 : dua faktor telah cukup (jumlah faktor telah cukup)
#H1 : ... (jumlah faktor tidak cukup)

#Pembuatan plot
load = data.fa$loadings[,1:2]
plot(load, type="n")  #set up plot
text(load, cex=0.7)
