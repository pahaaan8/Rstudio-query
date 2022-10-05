#LAPORAN 5 SPASIAL
#Autokorelasi spasial


library(spdep)
library(maptools)

sultra = readShapePoly("D:/kuliah/spasial/UAS/SHP/Indo_Kab_Kot.shp")
summary(sultra)
coords = coordinates(sultra)
plot(sultra, axes = T, border = "black", col = c("lightblue","lightblue1","lightblue2","lightblue3","lightblue4","skyblue","skyblue1","skyblue2","skyblue3","skyblue4","aliceblue","powderblue"),
     main = "Peta Wilayah Provinsi Aceh")
text(coords, labels = sultra$Kabupaten_, cex = 0.5)
attach(sultra)


f# melihat korelasi
Y = sultra$TPT
X1 = sultra$IPM
X2 = sultra$PKT
X3 = sultra$PPM
X4 = sultra$JAK
X5 = sultra$JBAK
cor(X5, Y)


# mencari nilai OLS Model (kuadrat terkecil)
olsreg=(lm(Y~X))
olsreg

# uji parsial
summary(olsreg)

# uji serentak
aov(olsreg)
anova(olsreg)


#spasial weight matriks based on k nearest neighbors (KNN, k=4)
coords = coordinates(sultra)
IDs = row.names(as(sultra, "data.frame"))
sultra_kn4 = knn2nb(knearneigh(coords, k=4), row.names = IDs)
plot(sultra, axes = T, border = "black", col = c("lightblue","lightblue1","lightblue2","lightblue3","lightblue4","skyblue","skyblue1","skyblue2","skyblue3","skyblue4","aliceblue","powderblue"),
     main = "Peta Wilayah Provinsi Sulawesi Tenggara dengan 4 Tetangga Terdekat")
text(coords, labels = sultra$Kabupaten_, cex = 0.5)
plot(sultra_kn4, coords, col = "red", add = T)
k1neigh2 = knearneigh(coords, k=4, longlat = T)


#Binary matrix (0 dan 1)
sultra_wb = nb2listw(sultra_kn4, style = "B")
summary(sultra_wb)

#Row standardized weights matrix
sultra_w = nb2listw(sultra_kn4)
listw = sultra_w
listw

# indeks moran global
moran.test(Y, listw, alternative = "two.sided")

# indeks moran lokal
localmoran(Y, listw)
moran.plot(Y, listw)
