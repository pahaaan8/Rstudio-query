matrx = matrix(c(1,3,4,2,3,2), nrow = 2, byrow = T)
matry = matrix(c(3,1,-4,2,5,6,1,4,8), nrow = 3, byrow = T)
matrz = matrix (c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = T)
#a
matrx%*%matry
#b
matry*matrz
#c
matry%*%matry
#d
dim(matrx)
dim(matry)
dim(matrz)
#e
diag(matry)
diag(matrx)
diag(matrz)
#f
solve(matry)
#g
det(matrz)
#h
t(matrx)
#i
eigen(matry)
#j
sum(diag(matrz))
#2
tabel2arah = matrix(c(86,19,16,79), nrow = 2, byrow = T)
dimnames(tabel2arah)=list(c("1-3 days", "4-7 days"), c("yes","NO"))
names(dimnames(tabel2arah)) = c("cold", "medicine")
tabel2arah
#odds rasio
data_test=prop.test(tabel2arah)
data_test
data_test$estimate
odds = data_test$estimate/(1-data_test$estimate)
odds
odds_rasio = odds[1]/odds[2]
odds_rasio

#3
data = c("setiap hari", "2xseminggu")
data1 = factor(rep(data, rep(4, length(data))), level = data)
rokok = factor(rep(rep(c("ya","tidak"),c(2,2)),2), levels = c("ya", "tidak"))
hipertensi = factor(rep(c("Ya", "Tidak"),4), levels = c("Ya", "Tidak"))
count = c(36,12,2,6,17,8,15,9)
tabel = data.frame(data1, rokok, hipertensi, count)
tabel
data2 = tapply(count,list(rokok, hipertensi, data1),c)
data2
names(dimnames(data2))=c("rokok","hipertensi","olahraga")
tabel3arah=ftable(data2,row.vars=c("olahraga","rokok"),col.vars="hipertensi")
#4
#tabel kontigensi
tabel= matrix(c(4,2,3,2,4,3,7,4,25,10,12,4,18,24,33,13,10,6,7,2), nrow =5, byrow = T)
dimnames(tabel) = list(c("st mansger", "jr manager", "st employess","jr employess", "secretaries"), c("none", "light", "medium", "heavy"))
names(dimnames(tabel)) = c("kategori pegawai","kategori merokok")
tabel
#koresponden
library(ca)
kores = ca(tabel)
plot(kores)
