################Business CASE#################################
#1) Breaking multiple csv files with name as MCATID.csv containing several rows like:
#  material , bearing steel __label__55485_material_m 0.990234
#into different columns of rating(0.990234),CustomISQ(material),
#ExistingISQ(material_m),MCATID(55485)
###############################################################
# Import libraries
library("xlsx")
library("dplyr")
library("stringi")
library("stringr")
library("tm")
# Create an empty dataframe
master_Data<-data.frame(V1=character(),
                        rating2=character(),
                        second=character(),
                        third=character(), 
                        rating1=character(),
                        match1=character(),
                        forth=character()) 

# Reading the names of files inside a folder
remaining<-dir("C:/Users/Imart/Desktop/QuesndOpt/Pumps/output")
# Making above a table
remaining<-data.frame(remaining)

#Extract the names of file
id<-as.character(remaining$remaining)


###################Copy Files for Ashish Corpus#####################
# Loop for all value  of ID
j <- 4
i <- 1

for(j in 1:length(id))
{
  # Read CSV one by one
  tot_files<-paste0("C:/Users/Imart/Desktop/QuesndOpt/Pumps/output/",id[j])
  new_df<-read.csv(tot_files,header = FALSE)
  
  # Concatenate forst and secodn columns
  #new_df$V1<-paste0(new_df$V1,",",new_df$V2)
  new_df$V2<-NULL
  new_df$V4 <- NULL
  new_df$V3 <- NULL
  # Storing total no. of rows in length variable
  length<-length(new_df$V1)
  
  #Creating two empty dataframes
  my_df<-data.frame(new_df.i...=character()) 
  my_df1<-data.frame(new_df.i...=character())
  
  # Loop to store odd rows in my_df and even rows in my_df1
  for(i in seq(from=1,to=length,by=2))
  {
    a<-data.frame(new_df[i,])
    b<-data.frame(new_df[i+1,])
    my_df<-rbind(my_df,a)
    my_df1<-rbind(my_df1,b)
  }
  
  # Creating a dataframe new_df by joining two dataframe my_df and my_df1
  new_df<-cbind(my_df,my_df1)
  # renaming columns
  new_df<-rename(new_df,V1=new_df.i...)
  new_df<-rename(new_df,V2=new_df.i...1...)
  new_df<-data.frame(paste0(new_df$V1," ",new_df$V2))
  new_df<-rename(new_df,V1=paste0.new_df.V1.......new_df.V2.)
  
  # removing last comma character from column
  char_array <-new_df$V1
  a <-data.frame("data"=char_array,"data2"=1)
  a$data<-as.character(a$data)
  new_df$V1 = substr(a$data,1,nchar(a$data)-1)
  
  #Extracting rating based on whitespace as delimiter
  new_df$rating2<-word(new_df$V1,-1)
  
  # Extracting Custom ISQ
  new_df$second<-word(new_df$V1,1,2,sep = "_")
  new_df$second <- gsub("_","",new_df$second)
  
  # Extracting thirdd Column
  new_df$third<-word(new_df$V1,-2)
  # Replacing __label__ with empty
  new_df$third<-gsub("__label__", "", new_df$third)
  #new_df$third<-gsub("_", " ", new_df$third)
  new_df$rating1 <- word(new_df$V1,-3)
  #match1
  new_df$match1 <- word(new_df$V1,-4)
  new_df$match1 <- gsub("__label__","",new_df$match1)
  
  # Extracting Id from third Column
  new_df$forth<-word(new_df$third,1,sep = "_")
  
  # Extracting Existing ISQ
  #new_df$fifth<-gsub(new_df$forth, "", new_df$third)
  
  #Removing column with name as Third
  #new_df$third<-NULL
  
  # Extracting Existing ISQ
  #new_df$fifth<-substring(new_df$fifth,2)
  
  # Appending rows in master data frame
  master_Data<-rbind(master_Data,new_df)
  
}
# renaming Columns

colnames(master_Data)[colnames(master_Data)=="second"] <- "keyword"
colnames(master_Data)[colnames(master_Data)=="third"] <- "match2"
colnames(master_Data)[colnames(master_Data)=="forth"] <- "MCAT ID"
master_Data <- master_Data[,c("MCAT ID","keyword","match1","rating1","match2","rating2","V1")]
# Exporting Data 
write.csv(master_Data,file = "C:/Users/Imart/Desktop/QuesndOpt/Pumps/output/out_2085.csv",row.names = FALSE)




