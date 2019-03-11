
validate <- readxl::read_excel("E:/excavator-validation.xlsx",sheet = 1)
k <- 1
Check <- data.frame(Var1 = numeric(),
                    Freq = numeric())


for (k in 1:3) {
   df <- readxl::read_excel("E:/Product Classification_new/NPS/Child1/Tuned_Ngram_out/final/processed.xlsx",sheet = k)
  df <- cbind(df,validate)
   df2 <- df[!is.na(df$`Category Suggestion`),]
  df2 <- df2[-nrow(df2),]
  df2$rating1 <- as.numeric(df2$rating1)
  df2$rating2 <- as.numeric(df2$rating2)
  df3 <- df2[df2$rating1>mean(df2$rating1,na.rm = T),]
  df3$Match <- df3$`Category Suggestion`==df3$mcat1
  
  Var1 <- table(df3$Match)
  var2 <- prop.table(Var1)
  Var2 <- as.data.frame(var2)
  Var2 <- Var2[-1,]
  Check <- rbind(Check,Var2)
  #Check <- Check[Check$Var1=="TRUE",]
  
}

df2[12,seq(1,5)]

#lr <- seq(0.75,.82,by=0.01)
#Check$LR <- lr
WordNgram <- c(1,2,3)
Check$WordNgram <- WordNgram
Check2 <- Check[,-1]
fix(Check2)
Check2
Plot <- ggplot(data = Check2) + ggtitle("LR fixed at .75,epoch =5,minn =5, varying wordNgrams from 1 to 3")

Plot <- Plot + aes(x=WordNgram,y=Accuracy) +scale_x_discrete(limit = c(1,2,3)) +ylim(0.90,0.94)

Plot <- Plot + geom_point() +geom_line(color ="green4",size =1)

Plot 
 