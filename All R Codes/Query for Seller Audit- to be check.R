# Import the data
df<-readxl::read_excel("C:/Users/Imart/Desktop/Custom for fastext.xlsx",sheet = 1)# Path of excel containing, MCAT ID and their corresponding labels

df$newCol<-gsub(",","",df$FK_IM_SPEC_OPTIONS_DESC)
df$value<-paste0(df$FK_IM_SPEC_MASTER_DESC," , ",df$newCol)

my_df<-df[,c(8,10)]


#Rearranging or sorting
my_df<-my_df[order(my_df$PC_ITEM_ATTRIBUTE_MCATID),]

#Rename column name to value
fix(df)

df$value<-as.character(df$value)

#Splitting data into different groups
out <- split( my_df , f = my_df$PC_ITEM_ATTRIBUTE_MCATID)

### A) To divide the data such that output is id.txt having contents as respective labels, to make corpus

for(i in 1:length(out)){
  hold<-unique(out[[i]]$MCAT_ID)
  location<-paste("50k/",hold,".txt")
  location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("C:/Users/Imart/Desktop/CITF/Output/",location)
  
  a<-out[[i]]$value
  
  write.table(a,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
  #write.xlsx(a,location,row.names = FALSE)
}






## B) Creating txt file for each id containing content like id_1.txt,id_2.txt ...id_n.txt. The name of file would be id.txt

##################For Combined Id######################
i<-2
for(i in 2:length(out))
{
  my_df<-data.frame(name=character())
  a<-length(out[[i]]$value)
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  location<-paste("IDs/",hold,".txt")
  location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("C:/Users/Imart/Desktop/CITF/",location)
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

# C) To create directories of each Id containing txt files such as id_1.txt,id_2.txt..etc having each corresponding label against them

#################### Custom Folder########################
i<-2
for(i in 2:length(out))
{
  a<-out[[i]]$value
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  j<-1
  for(j in 1:length(a))
  {
    name<-paste(hold,"_",j,".txt")
    name<-str_replace_all(string=name, pattern=" ", repl="")
    #location<-paste("Custom/",name,".txt")
    #location<-str_replace_all(string=location, pattern=" ", repl="")
    #location<-paste0("C:/Users/Imart/Desktop/CITF/",location)
    
    mainDir<-"C:/Users/Imart/Desktop/CITF/Custom"
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




# D) Create a txt file having content as id_1.txt,id_2.txt.. etc

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



R query for FastText.txt
Displaying R query for FastText.txt.