?db_write_table
?dbWriteTable
##Script to get child data against each PMCAT
library(RJDBC)
library(psych)
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")
#?qdapRegex
df<- readxl::read_excel("E:/Yarn-product classification/27-09-2018/yarn_complete_dump_215K.xlsx",sheet = 7)
#df2 <- NULL
#df2 <- df[df$FK_MCAT_TYPE_ID!=2,]
#df2 <- df2[df2$FK_MCAT_TYPE_ID!=3,]
#df2 <- df2[!is.na(df2$PRIME_MCAT_NAME),]
varr <- names(df)
varr
#colnames(df)[names(df)=="Final Label"] <- "Label1"
#df1 <- df[,c("GOOD PMCAT","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","Label1")]
#df1 <- df[,c(1,2,8,9,15,21)]
df1 <- df[,c("PRIME_MCAT_ID","PRIME_MCAT_NAME","PMCAT1","PC_ITEM_ID","PC_ITEM_NAME","PC_ITEM_DESC_SMALL","FK_IM_SPEC_MASTER_DESC","FK_IM_SPEC_OPTIONS_DESC","Label","Label1")]
#df1 <- df[,c(1:5,8,9)]
#fix(df)
#df2 <- df1[1:2000,]

df2 <- df1
colnames(df2)[colnames(df2)=="PC_ITEM_DESC_SMALL"] <- "New_Desc"
table(df2$PRIME_MCAT_NAME)
#?cut
#creating a user defined function to identify 
fun_bullet <- function(x)
{
  test1 <- rm_between(x,"<ul>","</ul>",extract = T)
  test1 <- test1[[1]]
  split_data <- unlist(str_split(test1,"([<ul>]\\<)"))
  my_str<-""
  for(i in 1:length(split_data))
  {
    my_str<-paste0(my_str," ",split_data[i])
  }
  return(my_str)
}

class(df2$New_Desc)

df2$New_Desc <- lapply(df2$New_Desc,fun_bullet)


#now we need to clean the data

#df_check2 <- cbind(df1[1:2000,],df2)
df_check2 <- df2
df_check2$New_Desc <- as.character(df_check2$New_Desc)
df_check2$New_Desc <- tolower(df_check2$New_Desc)

#df_check2 <- df_check2[,-c(6:9)]
df_check2$New_Desc <- rm_white(df_check2$New_Desc)
df_check2$New_Desc <- gsub("</li li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("</li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<li>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<ol>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("</ul>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<ul>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<p>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<b>"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("<br />"," ",df_check2$New_Desc)
df_check2$New_Desc <- as.character(df_check2$New_Desc)
df_check2$New_Desc <- gsub("[[:punct:]]"," ",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9] \\w+ *", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9]\\w+ *", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("[0-9]", "",df_check2$New_Desc)
df_check2$New_Desc <- gsub("\\s+"," ",str_trim(df_check2$New_Desc))
#df_check2$New_Desc <- gsub("stee","steel",str_trim(df_check2$New_Desc))
df_check2$New_Desc <- rm_white(df_check2$New_Desc) #to remove multiple white spaces with single space

for (i in 1:nrow(df_check2)) {
  ifelse(df_check2$New_Desc[i]=="na",df_check2$New_Desc[i] <- "",df_check2$New_Desc[i] <- as.character(df_check2$New_Desc[i]))
}

#removing duplicate words:
rem_dup.one <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = " ")
}

df_check2$New_Desc <- lapply(df_check2$New_Desc,rem_dup.one)
df_check2$New_Desc <- as.character(df_check2$New_Desc)
#df_check2$PC_ITEM_DESC_SMALL.1 <- tolower(df_check2$PC_ITEM_DESC_SMALL.1)



?removeWords

Stopwords1 <- c(" we "," are "," dealing "," quality "," manufacturers "," manufacturer "," exporters "," supplier "," dealer ",
                " good "," topmost "," business "," trusted "," finest "," offer ","offering"," involved "," provide "," reputed "," company ",
                " organization "," trader "," trading ")
#create a user defined function to remove stop words
library(tm)
Funrem_stop <- function(x)
{
  Stopwords <- c("we","are","dealing","quality","manufacturers","manufacturer","exporters","supplier","dealer",
                 "good","topmost","business","trusted","finest","offer","offering","involved","provide","reputed","company",
                 "organization","trader","trading","li","inr","indian","rupees","rupee")
  x <- removeWords(x,Stopwords)
  return(x)
}


df_check2$New_Desc <- lapply(df_check2$New_Desc,Funrem_stop)
length(!duplicated(df_check2$PC_ITEM_ID))
#data <- data.frame(lapply(df_check2, function(x) { gsub("na", "", x)}))
df_check2$New_Desc <- as.character(df_check2$New_Desc)
#data_unique <- df_check2[!duplicated(df_check2$PC_ITEM_ID),]
#data_unique <- dplyr::mutate(data_unique,concat_data <- paste0())
data_unique <- df_check2

data_unique$PC_ITEM_NAME1 <- tolower(data_unique$PC_ITEM_NAME)
data_unique$PC_ITEM_NAME1 <- rm_white(data_unique$PC_ITEM_NAME1)
data_unique$PC_ITEM_NAME1 <- gsub("[[:punct:]]","",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[0-9] \\w+ *", "",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[0-9]\\w+ *", "",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- gsub("[[:digit:]]","",data_unique$PC_ITEM_NAME1)
#data_unique$PC_ITEM_NAME1 <- rm_white(data_unique$PC_ITEM_NAME1)
#data_unique$Label2 <- NULL
index <- grepl("\\d",data_unique$Label1)
#for (i in 1:nrow(data_unique)) {
# ifelse(index[i]==TRUE,data_unique$Label1[i] <- "",data_unique$Label1[i] <- as.character(data_unique$Label1[i]))
#}

for (i in 1:nrow(data_unique)) {
  ifelse(data_unique$New_Desc[i]=="na",data_unique$New_Desc[i] <- "",data_unique$New_Desc[i] <- as.character(data_unique$New_Desc[i]))
}
#fix(data_unique)
data_unique$Label1 <- rm_white(data_unique$Label1)
data_unique$Label1 <- gsub("[[:punct:]]","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9] \\w+ *","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9]\\w+ *","",data_unique$Label1)
data_unique$Label1 <- gsub("[0-9]","",data_unique$Label1)
data_unique$Label1 <- lapply(data_unique$Label1,rem_dup.one)
data_unique$Label1 <- as.character(data_unique$Label1)
##removing stopeords from option_description:
# Import list of all stopwords
stopwords = readLines('E:/Product classification-fasttext/stopwords_excavator.txt')     #Your stop words file

# Function to remove stopwords
removeWords1 <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}
#i<-2
#data_unique$Label2<-0
for(i in 1:nrow(data_unique))
{
  data_unique$Label1[i]<-removeWords1(data_unique$Label1[i], stopwords)
  
}

#data_unique$Label1 <- tolower(data_unique$Label1)
data_unique$concat_name <- paste(data_unique$PC_ITEM_NAME1,data_unique$Label1,data_unique$New_Desc)
data_unique$concat_name <- rm_white(data_unique$concat_name)
data_unique$concat_name <- gsub("NA","",data_unique$concat_name)
data_unique$concat_name <- tolower(data_unique$concat_name)
data_unique$concat_name <- lapply(data_unique$concat_name,rem_dup.one)
data_unique$concat_name <- as.character(data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9] \\w+ *","",data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9]\\w+ *","",data_unique$concat_name)
#data_unique$concat_name <- gsub("[0-9]","",data_unique$concat_name)
data_unique$concat_name <- gsub(":","",data_unique$concat_name)
data_unique$PRIME_MCAT_NAME1 <- gsub(" ","_",data_unique$PRIME_MCAT_NAME)
data_unique$Fasttext_label <- paste0("__label__",data_unique$PRIME_MCAT_NAME1," ",data_unique$concat_name)
data_unique$Fasttext_test <- paste0(data_unique$concat_name)
data_unique$Fasttext_test <- rm_white(data_unique$Fasttext_test)

table(data_unique$PRIME_MCAT_NAME)


##*********Splitting data to get PMCAT wise training files of their child levels
##**************##
##**********Now we are creating training files PMCAT wise************##

data_unique <- data_unique[order(data_unique$PMCAT1),]

out2 <- split(data_unique,f=data_unique$PMCAT1)
for (i in 1:32) {
  print(unique(out2[[i]]$PMCAT1))
  print(length(unique(out2[[i]]$PRIME_MCAT_NAME)))
}
table(out2[[6]]$PRIME_MCAT_NAME)

#table(data_unique$PMCAT1,data_unique$PRIME_MCAT_NAME)

unique(out2[[6]]$PMCAT1)
#write.table(data_unique$Fasttext_label,"E:\\Yarn-product classification\\27-09-2018\\33_PMCAT_Yarn\\Training_data_PMCAT/33_pmcat_training.txt",sep = "\t",row.names = F,quote = F,col.names = F)
table(data_unique$PRIME_MCAT_NAME)

for (k in 1:length(out2)) {
  hold <- unique(out2[[k]]$PMCAT1)
  Data_out <- out2[[k]]$Fasttext_label
  LOCC2 <- paste0("E:/Yarn-product classification/27-09-2018/33_PMCAT_Yarn/Child_training_files/vlookup_",hold,".csv")
  write.csv(out2[[k]],LOCC2,row.names = F,quote = F)
  LOCC <- paste0("E:/Yarn-product classification/27-09-2018/33_PMCAT_Yarn/Child_training_files/",hold,".txt")
  write.table(Data_out,LOCC,sep = "\t",row.names = F,col.names = F,quote = F)
}

##K-Fold validation on child##
final_df <- out2[[15]]
final_df <- final_df[,c(1:5,14,15)]
final_df<-final_df[sample(nrow(final_df)),]
write.csv(final_df,"E:/Yarn-product classification/27-09-2018/33_PMCAT_Yarn/Child_training_files/Polyester_Yarn-KFold/Training Files/vlookup_polyester_yarn.csv",row.names = F,quote = F)
#write.csv(final_df,"E:/Product Classification_new/Motor Grader/Non Brand/non_brand_vlookUp_motor_grader",row.names = F,quote = F)
#Create 10 equally size folds
folds <- cut(seq(1,nrow(final_df)),breaks=10,labels=FALSE)
#dir.create("E:/Product_classification_YARN/Polyester Yarn/Training Files2")
#Perform 10 fold cross validation
i<-1
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final_df[testIndexes, ]
  trainData <- final_df[-testIndexes, ]
  trainData <- data.frame(trainData[,c(6)])
  trainLocation<-paste0("E:/Yarn-product classification/27-09-2018/33_PMCAT_Yarn/Child_training_files/Polyester_Yarn-KFold/Training Files/",i,".txt")
  testLocation<-paste0("E:/Yarn-product classification/27-09-2018/33_PMCAT_Yarn/Child_training_files/Polyester_Yarn-KFold/Testing Files/",i,".txt")
  write.table(trainData,trainLocation,sep = "\t",row.names = F,col.names = F,quote = F)
  testData<-data.frame(testData[,c(7)])
  write.table(testData,testLocation,sep = "\t" ,row.names = F,col.names = F,quote = F)
  
  #Use the test and train data partitions however you desire...
}


#***************************************END**************************************************#

