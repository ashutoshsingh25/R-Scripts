

load("C:/Users/imart/Downloads/City_Distance.Rdata")
city_df <- NULL

city_df1 <- NULL

city_df2$Tier <- sub("^Tier ","",city_df2$Tier)

class(city_df2$Tier)
city_df2$Tier <- as.numeric(city_df2$Tier)

for (i in 2:10) {
  
Kmean_cluster <- kmeans(city_df2[,c(2:5)],i)

print(table(Kmean_cluster$cluster))
}


Kmean_cluster_f <- kmeans(city_df2[,c(2:5)],10)




city_df2$Cluster <- Kmean_cluster_f$cluster

Kmeans_Distance <- kmeans(city_df2[,c(4)],10)

table(Kmeans_Distance$cluster)

city_df2$Cluster_distance <- Kmeans_Distance$cluster

for (i in 1:10) {
  print(summary(city_df2[city_df2$Cluster_distance==i,"DISTANCE_VAL"]))
}
summary(city_df2[city_df2$Cluster_distance==i,"DISTANCE_VAL"])

?kmeans


write.csv(city_df2,"c:/Users/imart/Desktop/city_distancs_kmeans.csv",row.names = F,quote = F)



New_data <-  readxl::read_excel("C:/Users/imart/Downloads/City_Distance_Data.xlsx",sheet = 2)

library(kmed)
class(New_data$`Aerial Distance in KM`)
mat_data <- as.matrix(New_data[,c(1)])

mrwdist <- distNumeric(mat_data, mat_data, method = "mrw")

mrwdist <- NULL

mrwdist[1:6,1:6]

result <- fastkmed(mrwdist, ncluster = 3, iterate = 50)
(fastiris <- table(result$cluster, iris[,5]))
?fastkmed



library(cluster)
library(factoextra)

ls('package:cluster')
?pam

result <- pam(New_data[,5],10)

Medoiids <- result$clustering

New_data$Kmedoids <- Medoiids

for (i in 1:10) {
  print(paste("Kmedoid",i))
  print(summary(New_data[New_data$Kmedoids==i,"Aerial Distance in KM"]))
}

table(New_data$Kmedoids)

colnames(New_data)


New_data[New_data$`To City Name`=="Delhi",]

New_data$Minn <- 0
New_data$maxx <- 0

i <- 1


for (i in 1:10) {
  New_data[New_data$Kmedoids==i,"Minn"] <- min(New_data[New_data$Kmedoids==i,"Aerial Distance in KM"],na.rm = T)#
  
}

for (i in 1:10) {
  New_data[New_data$Kmedoids==i,"Maxx"] <- max(New_data[New_data$Kmedoids==i,"Aerial Distance in KM"],na.rm = T)#
  
}

New_data$maxx <- NULL

write.xlsx(New_data,"distance_kmedoid.xlsx",row.names = F)


Minn_val <- unique(New_data$Minn)
Maxx_val <- unique(New_data$Maxx)

Cluster_a <- seq(1,10)


Clust_data <- data.frame(cbind(Cluster_a,Minn_val,Maxx_val))

Clust_data$City <- "Coimbatore"

Clust_data <- Clust_data[,c(4,1:3)]

Clust_data$Number_of_cities <- NUmbers_a$Freq

NUmbers_a <- data.frame(table(New_data$Kmedoids))

data_Del <- readxl::read_excel("C:/Users/imart/Downloads/City_Distance_Data.xlsx",sheet = 3)
data_Mum <- readxl::read_excel("C:/Users/imart/Downloads/City_Distance_Data.xlsx",sheet = 4)
data_Coimb <- readxl::read_excel("C:/Users/imart/Downloads/City_Distance_Data.xlsx",sheet = 2)

Final_data_city <- rbind(data_Del,data_Mum,data_Coimb)

result2 <- pam(Final_data_city[,5],10)

Medoiids2 <- result2$clustering


ls('package:cluster')
?pam
