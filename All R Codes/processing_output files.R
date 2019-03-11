
# processing final data:
#options(scipen = 999)
#options("scipen"=100, "digits"=4)
#format(1810032000, scientific = FALSE)
library(psych)
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")


Files <- list.files(path = "C:\\Users\\imart\\Documents\\Child_test_1\\final/",pattern = ".csv$")



m <- 3

for (m in 1:5) {
  
  locations <- paste0("C:\\Users\\imart\\Documents\\BL-Saleability\\Kfold\\final/",m,".csv")
  df <- read.csv(locations,header = F,sep = ",",fill = T)
  
  df <- read.csv("C:/Users/imart/Documents/Child_test_1/pmcat_t.csv",header = F,sep = ",",fill = T)
  df <- data.frame(df[,"V1"])
  sum(df$V1=="")
  #df$df....V1.. <- as.character(df$df....V1..)   
  #  for (j in nrow(df)) {
  #   ifelse(nchar(df$df....V1..[j])>200,df$df....V1..[j] <- paste(word(df$df....V1..[j],1),word(df$df....V1..[j],2),word(df$df....V1..[j],3),word(df$df....V1..[j],4)),df$df....V1..[j] <- as.character(df$df....V1..[j]))
  #}
  
  Index <- seq(1,nrow(df),2)
  Index2 <- seq(2,nrow(df),2)
  df_check <- data.frame(df[Index,])
  df_check2 <- data.frame(df[Index2,])
  which(grepl("__label__",df_check)==T)
  sum(grepl("__label__",df_check2))
  
  class(paste(word(df$df....V1..[1],1),word(df$df....V1..[1],2),word(df$df....V1..[1],3),word(df$df....V1..[1],4)))
  length <- nrow(df)
  #Creating two empty dataframes
  my_df<-data.frame(df.i...=character()) 
  my_df1<-data.frame(df.i...=character())
  #df<-df[-c(10060,13358),]
  #df<-data.frame(df)
  #for(i in seq(from=1,to=length,by=2)){
    
   # a<-data.frame(df[i,])
    #b<-data.frame(df[i+1,])
    #my_df<-rbind(my_df,a)
    #my_df1<-rbind(my_df1,b)
  #}
  
  
  #sum(grepl("__label__",a$df.i...))
  #sum(grepl("__label__",b$df.i...1...))
  
  #my_df$df.i... <- as.character(my_df$df.i...)
  #my_df1$df.i...1... <- as.character(my_df1$df.i...1...)
  # Creating a dataframe new_df by joining two dataframe my_df and my_df1
  #new_df<-cbind(my_df,my_df1)
  
  new_df <- cbind(df_check,df_check2)
  colnames(new_df)
  #new_df <- new_df[-nrow(new_df),]
  # renaming columns
  new_df<-rename(new_df,V1=df.Index...)
  new_df<-rename(new_df,V2=df.Index2...)
  #new_df<-data.frame(paste0(new_df$V1," ",new_df$V2))
  #new_df<-rename(new_df,V1=paste0.new_df.V1.......new_df.V2.)
  
  #colnames(new_df)[names(new_df)=="df.i..."] <- V1
  
  
  #splitting word data:
  # removing last comma character from column
  char_array <-new_df$V2
  a <-data.frame("data"=char_array,"data2"=1)
  a$data<-as.character(a$data)
  new_df$V2 = substr(a$data,1,nchar(a$data)-1)
  
  #Extracting rating based on whitespace as delimiter
  new_df$rating2<-word(new_df$V2,-1)
  
  # Extracting label
  new_df$mcat1<-word(new_df$V2,-4)
  new_df$mcat1 <- gsub("__label__","",new_df$mcat1)
  new_df$mcat1 <- gsub("_"," ",new_df$mcat1)
  
  # Extracting second mcat_match
  new_df$mcat2<-word(new_df$V2,-2)
  # Replacing __label__ with empty
  new_df$mcat2<-gsub("__label__", "", new_df$mcat2)
  new_df$mcat2 <- gsub("_"," ",new_df$mcat2)
  #new_df$third<-gsub("_", " ", new_df$third)
  new_df$rating1 <- word(new_df$V2,-3)
  
  final_out <- new_df[,c(1,2,4,6,5,3)]
  final_out <- rename(final_out,test_data=V1)
  final_out[2,]
  index <- grepl("e",final_out$rating2)
  #index <- as.character(index)
  for (i in 1:nrow(final_out)) {
    ifelse(index[i]==TRUE,final_out$rating2[i] <- 0,final_out$rating2[i] <- as.numeric(final_out$rating2[i]))
  }
  
  #final_out$rating2 <- format(final_out$rating2,scientific = FALSE)
  for (k in 1:nrow(final_out)) {
    ifelse(final_out$mcat1[k]=="",final_out$mcat1[k] <- as.character(final_out$mcat2[k]),final_out$mcat1[k] <- final_out$mcat1[k])
  }
  
  for (k in 1:nrow(final_out)) {
    ifelse(final_out$rating1[k]=="",final_out$rating1[k] <- as.numeric(final_out$rating2[k]),final_out$rating1[k] <- final_out$rating1[k])
  }
  
  Index3 <- (final_out$mcat1==final_out$mcat2)
  for (k in 1:nrow(final_out)) {
    ifelse(Index3[k]==TRUE,final_out$mcat2[k] <- "",final_out$mcat2[k] <- final_out$mcat2[k] )
  }
  
  for (k in 1:nrow(final_out)) {
    ifelse(Index3[k]==TRUE,final_out$rating2[k] <- "",final_out$rating2[k] <- final_out$rating2[k] )
  }
  #options(digits = 22)
  #library(bit64)
  #inal_out$rating2 <- as.integer64(final_out$rating2)
  #as.integer()
  #write_csv(tbl_df,'test.csv')
  #location2 <- paste0("C:\\Users\\imart\\Documents\\BL-Saleability\\Kfold\\final/K",m,".csv")
  #write.csv(final_out,location2,row.names = F,quote = F)
  write.csv(final_out,"C:/Users/imart/Documents/Child_test_1/pmcat_t_p.csv",row.names = F,quote = F)
  
}

getwd()

###Reading all folds and process in one go
i <- 1
df <- read.csv(LOCC)
for (i in 2:10) {
  LOCC <- paste0("E:/Conduit/Kfold/final/Fold",i,".csv")
  df1 <- read.csv(LOCC)
  df <-rbind(df,df1) 
}

write.csv(df,"E:/Conduit/Kfold/final/compiled.csv",row.names = F,quote = F)

#Lrnn <- length
#length <- NULL
#Seperation of files from PMCAT level to child

RES1 <- readxl::read_excel("E:/Conduit/Kfold/final2/compiled_fold.xlsx",sheet = 2)

RES1 <- RES1[order(RES1$mcat1),]

write.csv(RES1,"E:/Conduit/Kfold/final2/vlook_up_child_updated.csv",row.names = F,quote = F)

out3 <- split(RES1,f=RES1$mcat1)
i <- 1

for (i in 2:8) {
  testing_data <- out3[[i]]$test_data
  hold <- unique(out3[[i]]$mcat1)
  LOCC <- paste0("E:/Conduit/pmcat_testing_file/",hold,".txt")
  write.table(testing_data,LOCC,row.names = F,sep = "\t",col.names = F,quote = F)
}


#Reading files using loop
df1 <- read_excel("E:/Conduit/PMCAT_wise_child label/final/processed/final_child_processed_data.xlsx",sheet = 1)
for (i in 2:8) {
  df <- read_excel("E:/Conduit/PMCAT_wise_child label/final/processed/final_child_processed_data.xlsx",sheet = i)
  df1 <- rbind(df1,df)
}

write.xlsx(df1,"E:/Conduit/PMCAT_wise_child label/final_child_15606.xlsx")


#For another k-fold
Files <- list.files(path = "E:\\Excavator-PG2\\kfold\\final2\\processed",pattern = ".csv")
df1 <- read.csv("E:/Flooring/kfold/final/K1.csv")
for (i in 2:10) {
  Location_c <- paste0("E:/Flooring/kfold/final/K",i,".csv")
  df <- read.csv(Location_c)
  df1 <- rbind(df1,df)
}

df2 <- read.csv("E:/Flooring/kfold/vlookup_kfold.csv")


df_merge <- merge(x=df1,y=df2,by.x = "test_data",by.y = "Fasttext_test",all.x = T)

write.x(df1,"E:\\Excavator-PG2\\kfold\\final2\\processed/compiled_kfold2.csv",row.names = F,quote = F)

df_unique <- df_merge[!duplicated(df_merge$PC_ITEM_ID),]

df_unique <- df_unique[,c(-2,-12)]
colnames(df_merge)
df_unique$Match <- ""
for (i in 1:length(df_unique$Match)) {
  df_unique$Match[i] <- df_unique$mcat1[i]==df_unique$PMCAT2[i]
}

table(df_unique$Match)

df_unq2 <- df_unique[df_unique$rating1 > 0.964,]
df_unq2 <- df_unq2[is.na(df_unq2$Match)]

a <- table(df_unq2$Match)
a <- as.data.frame(a)

#sum(is.na(df_unq2$Match))


i=1
for (i in 1:2) {
  
}
a$per[i] <-a$Freq[i]/nrow(df_unq2)


write.xlsx(df_unq2,"C:/Users/Imart/Desktop/Flooring Test.xlsx", row.names = F)


df_merge <- NULL

a <- as.data.frame(a)

summary(df_unique$rating1)

FREQ <- read.table("E:/Excavator-PG2/PMCAT/PMCAT_training/pmcat_training.txt",sep = "\n",header = F)
warnings()

FREQ$V2 <- word(FREQ$V1,1,sep = " ")
table(FREQ$V2)