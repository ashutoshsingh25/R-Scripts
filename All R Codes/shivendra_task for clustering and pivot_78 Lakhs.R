df <- read.csv("E:/Pricing_analytics/MCAT_AOV_City_seller_final_new_mcat_city.csv")

df$CLUST <- paste0(df$TOTAL_APPROVED_BUY_LEAD,"_",df$Paid_Seller_Count)


df <- df[order(df$CLUST),]
class(df$CLUST)


Index <- unique(df$CLUST)
Index2 <- seq(1:length(Index))
Index2 <- as.character(Index2)
for (i in 1:length(Index)) {
Index2[i] <- as.character(paste("Cluster",i))
}

length(unique(df$CLUST))


Index[1]== df$CLUST[1]
i <- 1
df$CLUSTERS <- ""
for (i in 1:length(Index)) {
     Index_a <- which(df$CLUST==Index[i])
     df$CLUSTERS[Index_a] <- Index2[i]
  }


df2 <- df %>% dplyr::group_by(CLUSTERS) %>% summarise(Sum_Approved_BL = sum(TOTAL_APPROVED_BUY_LEAD,na.rm = T))
df3 <- df %>% dplyr::group_by(CLUSTERS) %>% summarise(Sum_Paid_Seller = sum(Paid_Seller_Count,na.rm = T))
colnames(df)

df_final <- cbind(df2,df3)

df_final <- df_final[,-3]


write.csv(df,"E:/Pricing_analytics/MCAT_AOV_City_seller_final_new_mcat_city_clst.csv",row.names = F,quote = F)
write.csv(df_final,"E:/Pricing_analytics/MCAT_AOV_City_seller_pivot.csv",row.names = F,quote = F)
