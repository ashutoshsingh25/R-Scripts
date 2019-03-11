df <- readxl::read_excel("E:/Ashutosh/R projects/Product classifications/MCATs id .xlsx",sheet = 1)

df_a <- df
df_a$GLCAT_MCAT_NAME <- gsub("Planter","",df_a$GLCAT_MCAT_NAME)
index <- grepl("Plant",df_a$GLCAT_MCAT_NAME)
which(index[index==T])
df$GLCAT_MCAT_NAME[index]
index2 <- grepl(" Plants ",df$GLCAT_MCAT_NAME)
index2[index2==T]

index3 <- grepl("Plant ",df$GLCAT_MCAT_NAME)
index3[index3==T]

index4 <- grepl(" Plants",df$GLCAT_MCAT_NAME)
index3[index4==T]

grepl(stopword,df$GLCAT_MCAT_NAME)
stopword <- c("Plant "," Plant","Plants "," Plants")

df1 <- as.data.frame(df)

for (i in 1:nrow(df)) {
  ifelse(index[i]==T,df1$GLCAT_MCAT_NAME[i] <-df_a$GLCAT_MCAT_NAME[i],df1$GLCAT_MCAT_NAME[i] <- "" )
}

Plant <- df1[df1$GLCAT_MCAT_NAME!="",]
for (i in 1:nrow(df1)) {
  ifelse(index2[i]==T,df1$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df1$GLCAT_MCAT_NAME[i] <- df1$GLCAT_MCAT_NAME[i] )
}

for (i in 1:nrow(df1)) {
  ifelse(index3[i]==T,df1$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df1$GLCAT_MCAT_NAME[i] <- df1$GLCAT_MCAT_NAME[i] )
}

for (i in 1:nrow(df1)) {
  ifelse(index4[i]==T,df1$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df1$GLCAT_MCAT_NAME[i] <- df1$GLCAT_MCAT_NAME[i] )
}

df2 <- df1[df1$GLCAT_MCAT_NAME!="",]
Plant <- df2

df3 <- df
index_a <- grepl("Equipment",df3$GLCAT_MCAT_NAME)
for (i in 1:nrow(df3)) {
  ifelse(index_a[i]==T,df3$GLCAT_MCAT_NAME[i] <-df3$GLCAT_MCAT_NAME[i],df3$GLCAT_MCAT_NAME[i] <- "" )
}

Equipment <- df3[df3$GLCAT_MCAT_NAME!="",]

df4 <- df

index_b <- grepl("System",df4$GLCAT_MCAT_NAME)
length(index_b[index_b==T])
for (i in 1:nrow(df1)) {
  ifelse(index_b[i]==T,df4$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df4$GLCAT_MCAT_NAME[i] <- "" )
}

Systems <- df4[df4$GLCAT_MCAT_NAME!="",]
df5 <- df
index_c <- grepl("Apparel",df5$GLCAT_MCAT_NAME)
length(index_c[index_c==T])
for (i in 1:nrow(df1)) {
  ifelse(index_c[i]==T,df5$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df5$GLCAT_MCAT_NAME[i] <- "" )
}

Apparel <- df5[df5$GLCAT_MCAT_NAME!="",]
df6 <- df

index_d <- grepl("Wear",df6$GLCAT_MCAT_NAME)
length(index_d[index_d==T])

for (i in 1:nrow(df1)) {
  ifelse(index_d[i]==T,df6$GLCAT_MCAT_NAME[i] <-df$GLCAT_MCAT_NAME[i],df6$GLCAT_MCAT_NAME[i] <- "" )
}

Wear <- df6[df6$GLCAT_MCAT_NAME!="",]

final_mcat <- rbind(Plant,Equipment,Apparel,Systems,Wear)

write.xlsx(final_mcat,"E:/Ashutosh/R projects/Product classifications/mcat_name_final.xlsx",row.names = F)

#2Another way-- 


v1 <- c("Addfg Plant","bagsdj Plants ","jfsty Plants","Plants dgsjss","Ahskdvs Planters")
str_detect(v1,"\\Plant\\b")

