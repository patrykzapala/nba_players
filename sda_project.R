library(dplyr)
library(writexl)
library(reshape2)
library(ggplot2)
file <- "dane_NBA.csv"
data <- read.csv(file, header = TRUE, sep = ";", row.names = 1)

## STATYSTYKI ZMIENNYCH
for(i in 1:ncol(data)){
  print(colnames(data)[i])
  print(sd(data[,i])/mean(data[,i]))
}

boxplot(data$AGE, main="AGE")
summary(data$AGE)
sd(data$AGE)

boxplot(data$W, main="WINS")
summary(data$W)
sd(data$W)

boxplot(data$L, main="LOSES")
summary(data$L)
sd(data$L)

boxplot(data$MIN, main="MINUTES PLAYED")
summary(data$MIN)
sd(data$MIN)

boxplot(data$PTS, main="POINTS")
summary(data$PTS)
sd(data$PTS)


boxplot(data$FG., main="FIELD GOAL PERCENTAGE")
summary(data$FG.)
sd(data$FG.)


boxplot(data$X3P., main="3 POINT FIELD GOAL PERCENTAGE")
summary(data$X3P.)
sd(data$X3P.)


boxplot(data$FT, main="FREE THROW PERCENTAGE")
summary(data$FT)
sd(data$FT)

cormat <- round(cor(data),2)
cormat[upper.tri(cormat)] <- NA
head(cormat)

melted_cormat <- melt(cormat, na.rm =  TRUE)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low="red", mid="white", high="green",limit=c(-1,1), midpoint = 0, name = "Pearson \nCorrelation")+
  theme_classic()

## PORZ¥DKOWANIE LINIOWE

optAge = 26

dataStand <- data %>%
  mutate(data, AGE = ifelse(AGE == optAge, 1, 
                                      ifelse(AGE < optAge, -1/(AGE - optAge - 1), 
                                             1/(AGE - optAge + 1) )))
dataStand <- dataStand %>%
  mutate(dataStand, L = (-1) * L)

dataStand <- as.data.frame(scale(dataStand))

pattern <- c()
for(i in 1:ncol(dataStand)){
  pattern <- c(pattern, max(dataStand[i]))
}

distData <- data.frame(dataStand)
for(i in 1:ncol(distData)){
  for(j in 1:nrow(distData)){
      distData[j,i] = (dataStand[j,i] - pattern[i])**2
  } 
}

distData <- distData %>%
  mutate(distData, distance = sqrt(rowSums(distData)))

mean <- mean(distData$distance)
sd <- sd(distData$distance)
d0 <- mean + 2 * sd

measure_value <- c()
for(i in 1:nrow(distData)){
  measure_value <- c(measure_value, 1- (distData$distance[i]/d0))
}

result <- data.frame(measure_value)

ranking <- result %>% 
  arrange(desc(measure_value))


ranking <- cbind(Player = rownames(ranking), ranking)


## ANALIZY SKUPIEÑ

dataStand <- scale(data)

#szukanie najlepszego podzialu
fviz_nbclust(dane_NBA_stand,kmeans, method = "wss")
fviz_nbclust(dane_NBA_stand,kmeans, method = "silhouette")

#podzial na 4 grupy
gr4 <- kmeans(dataStand, centers = 4, nstart = 10)
data_cl <- data
data_cl$cluster <- gr4$cluster
sort(gr4$cluster)
aggregate(data_cl[, 1:8], list(data_cl$cluster), mean)
gr4$cluster 
fviz_cluster(gr4, data= dataStand)


#----------gr hierarchiczne----
##wyzn odleglosci
dane_NBA_stand <- scale(data)
dist_euclidean <- dist(dane_NBA_stand,method="euclidean")
dist_maximum <- dist(dane_NBA_stand,method="maximum")
dist_manhattan <- dist(dane_NBA_stand,method="manhattan")
dist_minkowski <- dist(dane_NBA_stand,method="minkowski", p = 3)

#hclust
ward_euclidean <- hclust(dist_euclidean, method = "ward.D2")
ward_maximum <- hclust(dist_maximum, method = "ward.D2")
ward_manhattan <- hclust(dist_manhattan, method = "ward.D2")
ward_minkowski <- hclust(dist_minkowski, method = "ward.D2")

par(mfrow=c(2,2))
plot(ward_euclidean, main= "Odl. euklidesowa, metoda warda")
cut_euclidean <- cutree(ward_euclidean, k = 4)
rect.hclust(ward_euclidean, k = 4, border = 2:6)
abline(h = 7, col = 'red')
plot(ward_maximum, main = "Odl. maximum, metoda warda")
cut_maximum <- cutree(ward_maximum, k = 4)
rect.hclust(ward_maximum, k = 4, border = 2:6)
abline(h = 5, col = 'red')
plot(ward_manhattan, main = "Odl. manhattan, metoda warda")
cut_manhattan <- cutree(ward_manhattan, k = 4)
rect.hclust(ward_manhattan, k = 4, border = 2:6)
abline(h = 16.5, col = 'red')
plot(ward_minkowski, main = "Odl. minkowski, metoda warda")
cut_minkowski <- cutree(ward_minkowski, k = 4)
rect.hclust(ward_minkowski, k = 4, border = 2:6)
abline(h = 5.6, col = 'red')


#analiza wykresu odl
plot(x=1:29, y= ward_euclidean$height, type="S")
plot(x=1:29, y= ward_maximum$height, type="S")
plot(x=1:29, y= ward_manhattan$height, type="S")
plot(x=1:29, y= ward_minkowski$height, type="S")
par(mfrow=c(1,1))

data_cl <- mutate(data, cluster = cut_euclidean)
s <- aggregate(data_cl[, 1:8], list(data_cl$cluster), mean)

#skalowanie wielowymiarowe
odl <- dist(dataStand)

sww2 <- cmdscale(odl, k=2)

as.data.frame(sww2)
sww2 <- cbind(sww2, cut_euclidean)
sww2 <- as.data.frame(sww2)
plot(x = sww2[,1], y = sww2[,2],
     xlab = "wymiar1", ylab = "wymiar2",
     xlim= c(-3,5), ylim=c(-4,3))
text(x=sww2[,1],y=sww2[,2], col = sww2$cut_euclidean, labels = rownames(sww2),pos=3)

odl2 <- dist(sww2)

stress <- function(d1, d2){
  l <- sum((d1-d2)^2)
  m <- sum(d1^2)
  result <- sqrt(l/m)
  return (result)
}

s <- stress(odl,odl2)