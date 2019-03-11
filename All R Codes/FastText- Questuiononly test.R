library(dplyr)
library(readxl)
library(stringi)
library(stringr)

setwd("~/Ashutosh/")
#Break files in different format for cygwin:

setwd("C:/Users/Imart/Desktop/pump2")
getwd()

df<-readxl::read_excel("C:/Users/Imart/Desktop/QUESTION/Inverter/Inverter_PMCAT.xlsx",sheet = 1 )# Path of excel containing, MCAT ID and their corresponding labels
#df$PC_ITEM_ATTRIBUTE_MOD_DATE <- NULL
df$PC_ITEM_ATTRIBUTE_DETAIL_UOM <- NULL


#df$IM_SPEC_MASTER_DESC <- gsub("*\\(.*?\\)","",df$IM_SPEC_MASTER_DESC)

df$IM_SPEC_MASTER_DESC <- gsub("[[:digit:]]", "", df$IM_SPEC_MASTER_DESC)
df$IM_SPEC_MASTER_DESC <- gsub("<.*?>", " ", df$IM_SPEC_MASTER_DESC)
df$IM_SPEC_MASTER_DESC <- tolower(df$IM_SPEC_MASTER_DESC)
df$concat <- gsub(" ","_",df$IM_SPEC_MASTER_DESC)
df$IM_SPEC_MASTER_DESC <- gsub("*\\(.*?\\)","",df$IM_SPEC_MASTER_DESC)
df$label <-  paste0("__label__",df$MCAT_ID,"_",df$concat," ",df$IM_SPEC_MASTER_DESC)
df$label <- tolower(df$label)
df$label <- gsub("?","",df$label)
df$label <- gsub("/","",df$label)
df$IM_SPEC_MASTER_DESC <- gsub("?","",df$IM_SPEC_MASTER_DESC)
df$IM_SPEC_MASTER_DESC <- gsub("/","",df$IM_SPEC_MASTER_DESC)
df$IM_SPEC_MASTER_DESC <- gsub("\\.","",df$IM_SPEC_MASTER_DESC)
df$label <- gsub("\\.","",df$label)
#df$newcol <- paste0(df$label," ",df$IM_SPEC_MASTER_DESC)
#df$value <- tolower(df$value)
#df$IM_SPEC_MASTER_DESC <- tolower(df$IM_SPEC_MASTER_DESC)
fix(df)
#df
#df$GLCAT_MCAT_NAME<-NULL
#df$MASTER_DESC<-NULL
#df$OPTIONS_DESC<-NULL
#df$RN<-NULL

colnames(df)[colnames(df) == "label"] <- "value"
?df
df<-df[order(df$MCAT_ID),]

fix(df)

df$value<-as.character(df$value)

out <- split( df , f = df$MCAT_ID)

#out <- split(df,f = df$MCAT_ID)
#A) create txt files for labels.
i <- 1
for(i in 1:length(out)){
  hold<-unique(out[[i]]$MCAT_ID)
  location<-paste0("training/",hold,".txt")
  location<-paste0("C:/Users/Imart/Desktop/QUESTION/Inverter/",location)
  
  a<-out[[i]]$value
  
  write.table(a,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
}



## Creating txt file for each id containing content like id_1.txt,id_2.txt ...id_n.txt. The name of file would be id.txti

i <- 1
j <- 1

for(i in 1:length(out))
{
  my_df<-data.frame(name=character())
  a<-length(out[[i]]$value)
  hold<-unique(out[[i]]$MCAT_ID)
  location<-paste0("txtlist/",hold,".txt")
  #location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("C:/Users/Imart/Desktop/QUESTION/Inverter/",location)
  
  for(j in 1:a)
  {
    
    name<-paste(hold,"_",j,".txt")
    name<-str_replace_all(string=name, pattern=" ", repl="")
    name<-str_replace_all(string=name, pattern="\r", repl="")
    name<-data.frame(name)
    my_df<-rbind(my_df,name)
    
    
    
  }
  write.table(my_df,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
}

#####


#C) 

#################### Custom Folder for testing data########################

#df$test <- paste0(df$MASTER_DESC," ",df$OPTIONS_DESC)
#df$value <- dplyr::rename(df,"value2"="value")
colnames(df)[colnames(df)== 'value'] <- 'value2'
colnames(df)[colnames(df)=='IM_SPEC_MASTER_DESC'] <- 'value'
df$RN <- NULL
fix(df)
out <- split(df,f = df$MCAT_ID) 
i <- 1
j <- 1
for(i in 1:length(out))
{
  a<-out[[i]]$value
  hold<-unique(out[[i]]$MCAT_ID)
  
  for(j in 1:length(a))
  {
    name<-paste(hold,"_",j,".txt")
    name<-str_replace_all(string=name, pattern=" ", repl="")
    #location<-paste("Custom/",name,".txt")
    #location<-str_replace_all(string=location, pattern=" ", repl="")
    #location<-paste0("F:/Abhishek Work/Output/",location)
    
    mainDir<-"C:/Users/Imart/Desktop/QUESTION/Inverter/testing/"
    subDir<-hold
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    setwd(file.path(mainDir, subDir))
    location<-paste0(getwd(),"/",name)
    #mainDir<-paste("Custom/",hold)
    #mainDir<-str_replace_all(string=mainDir, pattern=" ", repl="")
    write.table(a[j],location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
  }
  
}
########################## End Custom############################




D) 

library(readxl)

check<-list.files("E:/Data", pattern=NULL, all.files=FALSE,full.names=FALSE)
#check<-(noquote(check))
check<-data.frame(check)
check$check<-as.character(check$check)
check$check<-as.numeric(check$check)
check<-data.frame(check[order(check$check),])
check<-data.frame(check[order(check$check),])


value<-data.frame(Id=character())


for(i in 1:nrow(check))
{
  
  a<-paste0(check[i,],".txt")
  a<-data.frame(Id=a)
  value<-rbind(value,a)
}
file<-paste0("E:/Additional Files/id.txt")
write.table(value,file = file, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)


setwd("~/Ashutosh")
