df<-readxl::read_excel("C:/Users/Imart/Desktop/Aditya/Ashutosh.xlsx",sheet = 1)# Path of excel containing, MCAT ID and their corresponding labels
fix(df)
df
df$GLCAT_MCAT_NAME<-NULL
df$MASTER_DESC<-NULL
df$OPTIONS_DESC<-NULL
df$RN<-NULL

?df
df<-df[order(df$PC_ITEM_ATTRIBUTE_MCATID),]

fix(df)

df$value<-as.character(df$value)

out <- split( df , f = df$PC_ITEM_ATTRIBUTE_MCATID)

A)

for(i in 1:length(out)){
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  location<-paste0("Aditya/",hold,".txt")
  location<-paste0("C:/Users/Imart/Desktop/",location)
  
  a<-out[[i]]$value
  
  write.table(a,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
}


## Creating txt file for each id containing content like id_1.txt,id_2.txt ...id_n.txt. The name of file would be id.txt
B)
##################For Combined Id######################
i<-1
for(i in 1:length(out))
{
  my_df<-data.frame(name=character())
  a<-length(out[[i]]$value)
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  location<-paste0("list of txt/",hold,".txt")
  #location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("C:/Users/Imart/Desktop/Aditya/",location)
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


#C) 

#################### Custom Folder########################
i <- 1
for(i in 1:length(out))
{
  a<-out[[i]]$value
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  
  for(j in 1:length(nrow(df)))
  {
    name<-paste(hold,"_",j,".txt")
    name<-str_replace_all(string=name, pattern=" ", repl="")
    #location<-paste("Custom/",name,".txt")
    #location<-str_replace_all(string=location, pattern=" ", repl="")
    #location<-paste0("F:/Abhishek Work/Output/",location)
    
    mainDir<-"C:/Users/Imart/Desktop/Aditya/"
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



