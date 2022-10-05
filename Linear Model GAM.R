library(readxl)


kapita=read.delim("clipboard")
attach(kapita)
summary(kapita)
str(kapita)
summary(data)

################################## Packages #######################################
install.packages("psych")
install.packages("fitdistrplus")
library(psych)
library(fitdistrplus)
par(mfrow=c(3,2))

boxplot(kapita$IPM, data=kapita, lwd=2, main="IPM")
stripchart(kapita$IPM, data=kapita, vertical=TRUE,method="jitter", add=TRUE, pch=20, col="blue")

boxplot(kapita$TPAK , data=kapita, lwd=2, main="Tingkat Partisipasi Angkatan Kerja")
stripchart(kapita$TPAK, data=kapita, vertical=TRUE,method="jitter", add=TRUE, pch=20, col="red")

boxplot(kapita$TPT, data=kapita, lwd=2, main="Tingkat Pengangguran Terbuka")
stripchart(kapita$TPT, data=kapita, vertical=TRUE,method="jitter", add=TRUE, pch=20, col="green")

boxplot(kapita$PDRB_K , data=kapita, lwd=2, main="PDRB_K")
stripchart(kapita$PDRB_K , data=kapita, vertical=TRUE,method="jitter", add=TRUE, pch=20, col="orange")

boxplot(kapita$PDRB_B, data=kapita, lwd=2, main="PDRB_b")
stripchart(kapita$PDRB_B, data=kapita, vertical=TRUE,method="jitter", add=TRUE, pch=20, col="yellow")

#################################### Scatter Plot Matrix ##########################

pairs.panels(kapita[,1:5], method="spearman",hist.col="blue",density=TRUE,ellipses=TRUE, main="Scatterplots Matrix")

dataIPM=ts(kapita$IPM, start=c(201,1),freq=(12))
dataTPAK=ts(kapita$TPAK, start=c(2010,1),freq=(12))
dataTingkat_Pengangguran_Terbuka=ts(kapita$TPT, start=c(2010,1),freq=(12))
dataPDRB_K=ts(kapita$PDRB_K, start=c(2010,1),freq=(12))
dataPengeluaran_perkapita=ts(kapita$PDRB_B, start=c(2010,1),freq=(12))

par(mfrow=c(2,3))
plot.ts(dataIPM,ylab="jiwa",main="IPM",col=1,lwd=2)
plot.ts(dataTPAK,ylab="jiwa",main="Tingkat Partisipasi Angkatan Kerja",col=2,lwd=2)
plot.ts(dataTingkat_Pengangguran_Terbuka,ylab="Tahun",main="Tingkat Pengangguran Terbuka",col=3,lwd=2)
plot.ts(dataPDRB_K,ylab="Indeks",main="PDRB_K",col=4,lwd=2)
plot.ts(dataPengeluaran_perkapita,ylab="Rupiah",main="Pengeluaran perkapita",col=5,lwd=2)

par(mfrow=c(2,3))
qqnorm(kapita$IPM, pch = 1, frame = FALSE,main="IPM",col=1)
qqline(kapita$IPM, col = "steelblue", lwd = 2)
qqnorm(kapita$TPAK, pch = 1, frame = FALSE,main="Tingkat Partisipasi Angkatan Kerja",col=2)
qqline(kapita$TPAK, col = "steelblue", lwd = 2)
qqnorm(kapita$TPT, pch = 1, frame = FALSE,main="Tingkat Pengangguran Terbuka",col=3)
qqline(kapita$TPT, col = "steelblue", lwd = 2)
qqnorm(kapita$PDRB_K, pch = 1, frame = FALSE,main="PDRB_K",col=4)
qqline(kapita$PDRB_K, col = "steelblue", lwd = 2)
qqnorm(kapita$PDRB_B, pch = 1, frame = FALSE,main="Pengeluaran Perkapita",col=6)
qqline(kapita$PDRB_B , col = "steelblue", lwd = 2)

################################ Plot Time Series ################################

#menentukan parameter distribusi gamma
gammafit1  <-  fitdistrplus::fitdist(kapita$IPM, "gamma")
gammafit2  <-  fitdistrplus::fitdist(kapita$TPAK, "gamma")
gammafit3  <-  fitdistrplus::fitdist(kapita$TPT, "gamma")
gammafit4  <-  fitdistrplus::fitdist(kapita$PDRB_K/1000000, "gamma")
gammafit5  <-  fitdistrplus::fitdist(kapita$PDRB_B/1000000, "gamma")

#menentukan parameter distribusi exponential
expfit1  <-  fitdistrplus::fitdist(kapita$IPM, "exp")
expfit2  <-  fitdistrplus::fitdist(kapita$TPAK, "exp")
expfit3 <-  fitdistrplus::fitdist(kapita$TPT, "exp")
expfit4  <-  fitdistrplus::fitdist(kapita$PDRB_K/1000000, "exp")
expfit5  <-  fitdistrplus::fitdist(kapita$PDRB_B/1000000, "exp")


#menentukan parameter distribusi lognormal
lognormalfit1  <-  fitdistrplus::fitdist(kapita$IPM, "lnorm")
lognormalfit2  <-  fitdistrplus::fitdist(kapita$TPAK, "lnorm")
lognormalfit3  <-  fitdistrplus::fitdist(kapita$TPT, "lnorm")
lognormalfit4  <-  fitdistrplus::fitdist(kapita$PDRB_K, "lnorm")
lognormalfit5  <-  fitdistrplus::fitdist(kapita$PDRB_B, "lnorm")

#menentukan parameter distribusi normal
normalfit1  <-  fitdistrplus::fitdist(kapita$IPM, "norm")
normalfit2  <-  fitdistrplus::fitdist(kapita$TPAK, "norm")
normalfit3  <-  fitdistrplus::fitdist(kapita$TPT, "norm")
normalfit4  <-  fitdistrplus::fitdist(kapita$PDRB_K, "norm")
normalfit5  <-  fitdistrplus::fitdist(kapita$PDRB_B, "norm")

#PENDING
ks.test(kapita$IPM, "pnorm", mean= mean(IPM), sd=sd(IPM))
ks.test(kapita$TPAK, "pnorm",mean= mean(TPAK), sd=sd(TPAK))
ks.test(kapita$TPT, "pnorm",mean= mean(TPT), sd=sd(TPT))
ks.test(kapita$PDRB_K, "pnorm",mean= mean(PDRB_K), sd=sd(PDRB_K))
ks.test(kapita$PDRB_B, "pnorm",mean= mean(PDRB_B), sd=sd(PDRB_B))


################################## Struktur Data (Melihat distribusi yang cocok) #############################
str(kapita)

par(mfrow=c(2,3))
satu<-descdist(kapita$IPM, discrete = FALSE)
dua<-descdist(kapita$TPAK, discrete = FALSE)
tiga<-descdist(kapita$TPT, discrete = FALSE)
empat<-descdist(kapita$PDRB_K, discrete = FALSE)
lima<-descdist(kapita$PDRB_B, discrete = FALSE)

############################### Linear Model ##############################
model1=lm(IPM~TPAK+TPT+PDRB_K+PDRB_B, data=kapita)
model1
summary(model1)
AIC(model1)

modelglm1=glm(IPM~TPAK+TPT+PDRB_K+PDRB_B, data = kapita)
summary(modelglm1)
library(nortest)
library(mgcv)
ad.test(kapita$IPM)

modelgam1=gam(IPM~s(TPAK)+s(TPT)+s(PDRB_K)+s(PDRB_B), data = kapita)
modelgam2=gam(IPM~s(TPAK,k=5)+s(TPT,k=5)+s(PDRB_K,k=5)+s(PDRB_B,k=5),family=inverse.gaussian(link=log))

summary(modelgam1)
AIC(modelgam1)
summary(modelgam2)

############################### GAM Model with basis cyclic cubic ##############################
modelgamcc=gam(IPM~s(TPAK,bs="cc")+s(TPT,bs="cc")+s(PDRB_K,bs="cc")+s(PDRB_B,bs="cc"))
modelgamcc1=gam(IPM~s(TPAK,bs="cc",k=12)+s(TPT,bs="cc",k=12)+s(PDRB_K,bs="cc",k=12)+s(PDRB_B,bs="cc",k=12),family=inverse.gaussian(link=log))
modelgamcc3=gam(IPM~s(TPAK,bs="cc",k=15)+s(TPT,bs="cc",k=15)+s(PDRB_K,bs="cc",k=15)+s(PDRB_B,bs="cc",k=15),family=inverse.gaussian(link=log))
modelgamcc4=gam(IPM~s(TPAK,bs="cc",k=17)+s(TPT,bs="cc",k=17)+s(PDRB_K,bs="cc",k=17)+s(PDRB_B,bs="cc",k=17),family=inverse.gaussian(link=log))

summary(modelgamcc)
AIC(modelgamcc)
summary(modelgamcc1)
summary(modelgamcc3)
summary(modelgamcc4)

############################### GAM Model with basis B-Spline ##############################
modelgambs=gam(IPM~s(TPAK,bs="bs")+s(TPT,bs="bs")+s(PDRB_K,bs="bs")+s(PDRB_B,bs="bs"))
modelgambs1=gam(IPM~s(TPAK,bs="bs",k=12)+s(TPT,bs="bs",k=12)+s(PDRB_K,bs="bs",k=12)+s(PDRB_B,bs="bs",k=12),family=inverse.gaussian(link=log))
modelgambs3=gam(IPM~s(TPAK,bs="bs",k=15)+s(TPT,bs="bs",k=15)+s(PDRB_K,bs="bs",k=15)+s(PDRB_B,bs="bs",k=15),family=inverse.gaussian(link=log))
modelgambs4=gam(IPM~s(TPAK,bs="bs",k=17)+s(TPT,bs="bs",k=17)+s(PDRB_K,bs="bs",k=17)+s(PDRB_B,bs="bs",k=17),family=inverse.gaussian(link=log))

summary(modelgambs)
AIC(modelgambs)
summary(modelgambs1)
summary(modelgambs3)
summary(modelgambs4)

############################### GAM Model with basis Duncan-spline  ##############################
modelgamds=gam(IPM~s(TPAK,bs="ds")+s(TPT,bs="ds")+s(PDRB_K,bs="ds")+s(PDRB_B,bs="ds"))
modelgamds1=gam(IPM~s(TPAK,bs="ds",k=12)+s(TPT,bs="ds",k=12)+s(PDRB_K,bs="ds",k=12)+s(PDRB_B,bs="ds",k=12),family=inverse.gaussian(link=log))
modelgamds3=gam(IPM~s(TPAK,bs="ds",k=15)+s(TPT,bs="ds",k=15)+s(PDRB_K,bs="ds",k=15)+s(PDRB_B,bs="ds",k=15),family=inverse.gaussian(link=log))
modelgamds4=gam(IPM~s(TPAK,bs="ds",k=17)+s(TPT,bs="ds",k=17)+s(PDRB_K,bs="ds",k=17)+s(PDRB_B,bs="ds",k=17),family=inverse.gaussian(link=log))

summary(modelgamds)
AIC(modelgamds)
summary(modelgamds1)
summary(modelgamds3)
summary(modelgamds4)

############################### GAM Model with basis P-Spline  ##############################
modelgamps=gam(IPM~s(TPAK,bs="ps")+s(TPT,bs="ps")+s(PDRB_K,bs="ps")+s(PDRB_B,bs="ps"))
modelgamps1=gam(IPM~s(TPAK,bs="ps",k=12)+s(TPT,bs="ps",k=12)+s(PDRB_K,bs="ps",k=12)+s(PDRB_B,bs="ps",k=12),family=inverse.gaussian(link=log))
modelgamps3=gam(IPM~s(TPAK,bs="ps",k=15)+s(TPT,bs="ps",k=15)+s(PDRB_K,bs="ps",k=15)+s(PDRB_B,bs="ps",k=15),family=inverse.gaussian(link=log))
modelgamps4=gam(IPM~s(TPAK,bs="ps",k=17)+s(TPT,bs="ps",k=17)+s(PDRB_K,bs="ps",k=17)+s(PDRB_B,bs="ps",k=17),family=inverse.gaussian(link=log))

summary(modelgamps)
summary(modelgamps1)
summary(modelgamps3)
summary(modelgamps4)

############################### GAM Model Gaussian Spline  ##############################
modelgamgp=gam(IPM~s(TPAK,bs="gp")+s(TPT,bs="gp")+s(PDRB_K,bs="gp")+s(PDRB_B,bs="gp"))
modelgamgp1=gam(IPM~s(TPAK,bs="gp",k=12)+s(TPT,bs="gp",k=12)+s(PDRB_K,bs="gp",k=12)+s(PDRB_B,bs="gp",k=12),family=inverse.gaussian(link=log))
modelgamgp3=gam(IPM~s(TPAK,bs="gp",k=15)+s(TPT,bs="gp",k=15)+s(PDRB_K,bs="gp",k=15)+s(PDRB_B,bs="gp",k=15),family=inverse.gaussian(link=log))
modelgamgp4=gam(IPM~s(TPAK,bs="gp",k=17)+s(TPT,bs="gp",k=17)+s(PDRB_K,bs="gp",k=17)+s(PDRB_B,bs="gp",k=17),family=inverse.gaussian(link=log))

summary(modelgamgp)
summary(modelgamgp1)
summary(modelgamgp3)
summary(modelgamgp4)


############################### GAM Model Markov Random Fields  ##############################
modelgamre=gam(IPM~s(TPAK,bs="re")+s(TPT,bs="re")+s(PDRB_K,bs="re")+s(PDRB_B,bs="re"))
modelgamre1=gam(IPM~s(TPAK,bs="re",k=12)+s(TPT,bs="re",k=12)+s(PDRB_K,bs="re",k=12)+s(PDRB_B,bs="re",k=12),family=inverse.gaussian(link=log))
modelgamre3=gam(IPM~s(TPAK,bs="re",k=15)+s(TPT,bs="re",k=15)+s(PDRB_K,bs="re",k=15)+s(PDRB_B,bs="re",k=15),family=inverse.gaussian(link=log))
modelgamre4=gam(IPM~s(TPAK,bs="re",k=17)+s(TPT,bs="re",k=17)+s(PDRB_K,bs="re",k=17)+s(PDRB_B,bs="re",k=17),family=inverse.gaussian(link=log))

summary(modelgamre)
summary(modelgamre1)
summary(modelgamre3)
summary(modelgamre4)

AIC(model1)
AIC(model2)
AIC(modelgamgp)
AIC(modelgamgp1)
AIC(modelgamgp3)
