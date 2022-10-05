#####Tugas ADW VAR######

##Packages
install.packages("dynlm")
install.packages("vars")
install.packages("nlWaldTest")
install.packages("lmtest")
install.packages("broom")
install.packages("car")
install.packages("sandwich")
install.packages("knitr")
install.packages("forecast")

library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast)

#Untuk menginstall PoEdata
install.packages("devtools")
library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)

#manggil data
data = read.delim("clipboard")
data
attach(data)

#Membuat plot gabungan
fred <- ts(data, frequency=31)
ts.plot(fred[,"France"],fred[,"Inggris"], type="l", lty=c(1,2), col=c(1,2))
legend("topleft", border=NULL, legend=c("France","Inggris"), lty=c(1,2), col=c(1,2))

#Melihat plot AcF dan PACF
par(mfrow=c(1,2))
Acf(fred[,"France"])
Acf(fred[,"Inggris"])

#Uji stasioner mean
adf.test(fred[,"France"])
adf.test(fred[,"Inggris"])

#Differencing karena keduanya tidak stasioner
adf.test(diff(fred[,"France"]))
adf.test(diff(fred[,"Inggris"]))

data.diff = diff(data$rusia, differences = 2)
adf.test(data.diff)
data.diff = diff(data$israel, differences = 2)
adf.test(data.diff)

cointcy <- dynlm(rusia~israel, data=fred)
ehat <- resid(cointcy)
adf.test(ehat)

#Model Hasil Estimasi Var
library(vars)
Dr <- diff(fred[,"rusia"])
Di <- diff(fred[,"israel"])
varmat <- as.matrix(cbind(Dr,Di))
varfit <- VAR(varmat) # `VAR()` from package `vars`
summary(varfit)

#Dekomposisi Varians
impresp <- irf(varfit)
plot(impresp)
plot(fevd(varfit)) # `fevd()` is in package `vars`
<Return>
  