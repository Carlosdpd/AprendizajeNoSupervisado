install.packages('scatterplot3d')
library(scatterplot3d)
setwd("C:\\Users\\Carlos\\Desktop\\Universidad\\Mineria\\AprendizajeNoSupervisado-master")

#K medias a.csv

dataset_a = read.csv(file = "a.csv", header = F)

head(dataset_a)

table(dataset_a$V3)

dataset_a[dataset_a == 0] = 3

table(dataset_a$V3)

plot(x=dataset_a$V1, y=dataset_a$V2, col= dataset_a$V3)

kmedias_a = kmeans(x = dataset_a, centers = 3)

plot(x=dataset_a$V1, y=dataset_a$V2, col= kmedias_a$cluster)

points(x = kmedias_a$centers, col = 1:3, pch = 19, cex = 3)

table(kmedias_a$cluster, dataset_a$V3)

#fin_a
#a_big
#fin a_big

#guess.csv
dataset_guess = read.csv(file = "guess.csv", header = F)

plot(dataset_guess)

InerciaIC.Hartigan = rep(0, 30)
for (k in 1:30) {
  grupos = kmeans(dataset_guess, k, iter.max = 100, algorithm = "Hartigan-Wong")
  InerciaIC.Hartigan[k] = grupos$tot.withinss
}
plot(InerciaIC.Hartigan, col = "blue", type = "b")
#Observamos que 2 sería el número adecuado de clusters

kmedias_guess = kmeans( x = dataset_guess, center = 2)

plot(x = dataset_guess$V1, y = dataset_guess$V2, col = kmedias_guess$cluster)

#fin guess.csv

#moon.csv

dataset_moon = read.csv(file = "moon.csv", header = F)

plot(dataset_moon$V1, dataset_moon$V2)

#se uso k medias, no es bueno

entrada_moon = dataset_moon

entrada_moon$V3 = NULL

entrada_moon = as.matrix(entrada_moon)

distancia_moon = dist(entrada_moon)

cluster_moon = hclust(distancia_moon, method = 'single')

plot(cluster_moon)

corte_moon = cutree(cluster_moon, k = 2)

head(corte_moon)

plot(x = dataset_moon$V1, y = dataset_moon$V2, col = corte_moon)

table(dataset_moon$V3, corte_moon)
#quedó malandro

#h.csv
dataset_h = read.csv(file = "h.csv", header = F)

plot(dataset_h)

dataset_h$V4[dataset_h$V4 < 7] = 1

dataset_h$V4[dataset_h$V4 > 7 & dataset_h$V4 < 10] = 2

dataset_h$V4[dataset_h$V4 > 10 & dataset_h$V4 < 12] = 3

dataset_h$V4[dataset_h$V4 > 12] = 4

unique(dataset_h$V4)

plot(dataset_h$V1, dataset_h$V2, col= dataset_h$V4)

plot(dataset_h$V1, dataset_h$V3, col= dataset_h$V4)

plot(dataset_h$V2, dataset_h$V3, col= dataset_h$V4)

scatterplot3d(dataset_h$V1, dataset_h$V2, dataset_h$V3, highlight.3d = TRUE)

kmedias_h = kmeans(dataset_h, centers = 4)

plot(dataset_h$V1, dataset_h$V3, col= kmedias_h$cluster)

points(x = kmedias_h$centers, col = 1:4, pch = 19, cex = 3)

table(dataset_h$V4, kmedias_h$cluster)

#Clustering dataset_h

enum_h = dataset_h

enum_h$V4 = NULL

enum_h = as.matrix(enum_h)

distancia_h = dist(enum_h)

cluster_h = hclust(distancia_h, method = 'average')

plot(cluster_h)

corte_h = cutree(cluster_h, k=4)

plot(dataset_h$V1, dataset_h$V2, col= corte_h)

plot(dataset_h$V1, dataset_h$V3, col= corte_h)



#ambos metodos no sirven para nada
#fin de h.csv

#s.csv

dataset_s = read.csv(file = 's.csv', header = F)

range(dataset_s$V4)

plot(dataset_s)

dataset_s$V4[dataset_s$V4 > 2] = 4

dataset_s$V4[dataset_s$V4 > 0 & dataset_s$V4 < 2] = 3

dataset_s$V4[dataset_s$V4 > -2 & dataset_s$V4 < 0] = 2

dataset_s$V4[dataset_s$V4 < -2] = 1

unique(dataset_s$V4)

plot(dataset_s$V1, dataset_s$V2, col= dataset_s$V4)

plot(dataset_s$V1, dataset_s$V3, col= dataset_s$V4)

plot(dataset_s$V2, dataset_s$V3, col= dataset_s$V4)

kmedias_s = kmeans(dataset_s, centers = 4)

plot(x = dataset_s$V1, y = dataset_s$V3, col = kmedias_s$cluster)

plot(x = dataset_s$V1, y = dataset_s$V2, col = kmedias_s$cluster)

points(x = kmedias_s$centers, col = 1:4, pch = 19, cex = 3)

scatterplot3d(dataset_s$V1, dataset_s$V2, dataset_s$V3, highlight.3d = TRUE)

table(dataset_s$V4, kmedias_s$cluster)
#Clustering s.csv

enum_s = dataset_s

enum_s$V4 = NULL

enum_s = as.matrix(enum_s)

distancia_s = dist(enum_s)

cluster_s = hclust(distancia_s)

plot(cluster_s)

corte_s = cutree(cluster_s, k=4)

plot(dataset_s$V1, dataset_s$V2, col= corte_s)

plot(dataset_s$V1, dataset_s$V3, col= corte_s)


#ambos metodos no sirven para nada
#fin s.csv


#help.csv

dataset_help = read.csv(file = 'help.csv', header = F)

range(dataset_help$V4)

scatterplot3d(dataset_help$V1, dataset_help$V2, dataset_help$V3, highlight.3d = TRUE)

dataset_help$V5[dataset_help$V1 > 17 & dataset_help$V1 < 42] = 2

dataset_help$V5[dataset_help$V1 < 17] = 1

dataset_help$V5[dataset_help$V1 > 42] = 3

dataset_help$V4 = NULL

unique(dataset_help$V5)

plot(dataset_help$V1, dataset_help$V2, col= dataset_help$V4)

plot(dataset_help$V1, dataset_help$V3, col= dataset_help$V5)

plot(dataset_help$V2, dataset_help$V3, col= dataset_help$V4)

kmedias_help = kmeans(x = dataset_help, centers = 3)

plot(dataset_help$V1, dataset_help$V3, col= kmedias_help$cluster)

table(dataset_help$V5, kmedias_help$cluster)
#Fin help

#goodluck.csv
dataset_luck = read.csv(file = 'good_luck.csv', header = F)

plot(dataset_luck$V1, dataset_luck$V2, col = dataset_luck$V11)

plot(dataset_luck$V1, dataset_luck$V3, col = dataset_luck$V11)

plot(dataset_luck$V1, dataset_luck$V4, col = dataset_luck$V11)

scatterplot3d(dataset_luck$V1, dataset_luck$V2, dataset_luck$V3, highlight.3d = TRUE)

kmedias_goodluck = kmeans(x = dataset_luck, centers = 2)

plot(dataset_luck$V1, dataset_luck$V2, col = kmedias_goodluck$cluster)

points(x = kmedias_goodluck$centers, col = 1:4, pch = 19, cex = 3)

#fin goodluck