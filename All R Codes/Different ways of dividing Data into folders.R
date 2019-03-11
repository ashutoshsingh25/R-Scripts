# Import all the libraries
library(readxl)

#1a)Code to import  multiple excel files with names having pattern, like File(i).xlsx where i is 1...name
#1b) Copying ids from each excel, getting unique ids and storing in excel
############################ Start Code###########################
options(java.parameters = "-Xmx8000m")

my_file<- data.frame(MCAT_ID=numeric(),
                     GLCAT_MCAT_NAME=character(), 
                     MASTER_DESC=character(), 
                     OPTIONS_DESC=character(),
                     `CONCAT('__LABEL__',MCAT_ID||'_'||MASTER_DESC1||''||MASTER_DESC||''||OPTIONS_DESC)`=character(),
                     RN=numeric()) 


for(i in 1:51)
{
  file<-paste(i,".xlsx")
  file<-str_replace_all(string=file, pattern=" ", repl="")
  file<-paste("File",file)
  file<-paste0("F:/Abhishek Work/50k/",file)
  
  
  df<-read_excel(file)
  my_file<-rbind(my_file,df)
}

my_file <- my_file[order(my_file$MCAT_ID),] 

id1<-data.frame(unique(my_file$MCAT_ID))

write.table(id,"F:/Abhishek Work/output/50kIds.txt", sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)

###############################End Code###################################

#2) Divide multiple imported excels  into different txt with name as id.txt and having corresponsing labels column.

###############################Start Code###################################
my_file1<-my_file

my_file$MASTER_DESC<-NULL
my_file$GLCAT_MCAT_NAME<-NULL
my_file$RN<-NULL
my_file$OPTIONS_DESC<-NULL

fix(my_file)

my_file$value<-as.character(my_file$value)

out <- split( my_file , f = my_file$MCAT_ID)



for(i in 1:length(out)){
  hold<-unique(out[[i]]$MCAT_ID)
  location<-paste("50k/",hold,".txt")
  location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("F:/Abhishek Work/Output/",location)
  
  a<-out[[i]]$value
  
  write.table(a,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
  #write.xlsx(a,location,row.names = FALSE)
}

###############################End Code###################################


#3) Divide a single excel having Ids and their corresponding labels into id.txt having lables inside it
################################# Start Code #######################################
#START#
###########################################################################################
library("xlsx")
file<-"C:/Users/Imart/Downloads/corpus_IDwise.xlsx"
my_file<-read.xlsx(file, 1, header=TRUE, colClasses=NA)

my_file1<-my_file

my_file$MASTER_DESC<-NULL
my_file$GLCAT_MCAT_NAME<-NULL
my_file$RN<-NULL
my_file$OPTIONS_DESC<-NULL

fix(my_file)

my_file$value<-as.character(my_file$value)

#set.seed(1)

out <- split(my_file , f = my_file$MCAT_ID)


for(i in 1:length(out)){
  location<-paste("50k/",out[[i]]$MCAT_ID[i],".txt")
  location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("F:/Abhishek Work/Output/",location)
  
  a<-out[[i]]$value
  
  write.table(a,location, sep="\t",row.names = FALSE,quote = FALSE,col.names = FALSE)
  #write.xlsx(a,location,row.names = FALSE)
}
#################################End Code#######################################
#END#
###########################################################################################


#3a)Code to import  multiple excel files with names having pattern, like File(i).xlsx where i is 1...name

############################ Start Code###########################
options(java.parameters = "-Xmx8000m")
##################################################################
my_file_c<- data.frame(PC_ITEM_ATTRIBUTE_MCATID=numeric(),
                       GLCAT_MCAT_NAME=character(), 
                       MASTER_DESC=character(), 
                       OPTIONS_DESC=character(),
                       `MASTER_DESC||''||','||''||OPTIONS_DESC`=character()) 
my_file_c<-rename(my_file_c,`MASTER_DESC||''||','||''||OPTIONS_DESC`=MASTER_DESC...............OPTIONS_DESC)


for(i in 1:51)
{
  file<-paste(i,"export.xlsx")
  file<-str_replace_all(string=file, pattern=" ", repl="")
  file<-paste("file",file)
  file<-paste0("F:/Abhishek Work/Corpus custom isq Data/",file)
  
  
  df<-read_excel(file)
  my_file_c<-rbind(my_file_c,df)
}

#3b) Create different folders with id name, each folders contain different txt as id_1.txt,id_2.txt... having value of labels for each row
my_file_c <- my_file_c[order(my_file_c$PC_ITEM_ATTRIBUTE_MCATID),]


my_file1_c<-my_file_c

my_file_c$MASTER_DESC<-NULL
my_file_c$GLCAT_MCAT_NAME<-NULL
my_file_c$OPTIONS_DESC<-NULL

fix(my_file_c)

my_file_c$value<-as.character(my_file_c$value)

out <- split( my_file_c , f = my_file_c$PC_ITEM_ATTRIBUTE_MCATID)

i<-1
j<-1

#################### Custom Folder########################
for(i in 1:length(out))

  a<-out[[i]]$value
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  for(j in 1:length(a))
  {
  name<-paste(hold,"_",j,".txt")
  name<-str_replace_all(string=name, pattern=" ", repl="")
  #location<-paste("Custom/",name,".txt")
  #location<-str_replace_all(string=location, pattern=" ", repl="")
  #location<-paste0("F:/Abhishek Work/Output/",location)
  a[j]<-str_replace_all(string=a[j], pattern="\r", repl="")
 
   mainDir<-"F:/Abhishek Work/Output/Custom"
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


## Creating txt file for each id containing content like id_1.txt,id_2.txt ...id_n.txt. The name of file would be id.txt
##################For Combined Id######################
for(i in 1:length(out))
{
  my_df<-data.frame(name=character())
  a<-length(out[[i]]$value)
  hold<-unique(out[[i]]$PC_ITEM_ATTRIBUTE_MCATID)
  location<-paste("Combined/",hold,".txt")
  location<-str_replace_all(string=location, pattern=" ", repl="")
  location<-paste0("F:/Abhishek Work/Output/",location)
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
##############################Combined Data End#######################################

