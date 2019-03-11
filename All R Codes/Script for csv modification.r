library("xlsx")
library("dplyr")
library("stringi")
library("stringr")
library("tm")
# Create an empty dataframe
master_Data<-data.frame(V1=character(),
                        WORD=character(), 
                        Id=character(), 
                        ISQ1=character(),
                        RATING1=character(),
                        ISQ2=character(),
                        RATING2=character(),
                        ISQ3=character(),
                        RATING3=character()) 



# Reading the names of files inside a folder
remaining<-dir("C:/Users/Imart/Desktop/files/out3csv")
# Making above a table
remaining<-data.frame(remaining)

#Extract the names of file
id<-as.character(remaining$remaining)

for(j in 1:length(id))
{
  # Read CSV one by one
  tot_files<-paste0("C:/Users/Imart/Desktop/files/out3csv/",id[j])
  new_df<-read.csv(tot_files,header = FALSE)
  
  # Concatenate forst and secodn columns
  new_df$V1<-paste0(new_df$V1,",",new_df$V2)
  new_df$V2<-NULL
  
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
  
  # Extracting Custom ISQ
  new_df$WORD<-word(new_df$V1,1,sep = ",")
  
  
  #Extracting LAST rating based on whitespace as delimiter
  new_df$RATING3<-word(new_df$V1,-1)
  
  
  
  
  # Extracting thirdd Column
  new_df$third<-word(new_df$V1,-2)
  # Replacing __label__ with empty
  new_df$third<-gsub("__label__", "", new_df$third)
  #new_df$third<-gsub("_", " ", new_df$third)
  
  # Extracting Id from third Column
  new_df$forth<-word(new_df$third,1,sep = "_")
  
  # Extracting Existing ISQ
  new_df$fifth<-gsub(new_df$forth, "", new_df$third)
  
  #Removing column with name as Third
  new_df$third<-NULL
  
  new_df$ID<-new_df$forth
  new_df$forth<-NULL
  
  # Extracting Existing ISQ
  new_df$ISQ3<-substring(new_df$fifth,2)
  new_df$fifth<-NULL
  
  ###################################################################
  new_df$RATING2<-word(new_df$V1,-3)
  
  # Extracting thirdd Column
  new_df$third<-word(new_df$V1,-4)
  # Replacing __label__ with empty
  new_df$third<-gsub("__label__", "", new_df$third)
  #new_df$third<-gsub("_", " ", new_df$third)
  
  # Extracting Id from third Column
  new_df$forth<-word(new_df$third,1,sep = "_")
  
  # Extracting Existing ISQ
  new_df$fifth<-gsub(new_df$forth, "", new_df$third)
  
  #Removing column with name as Third
  new_df$third<-NULL
  
  new_df$forth<-NULL
  
  # Extracting Existing ISQ
  new_df$ISQ2<-substring(new_df$fifth,2)
  new_df$fifth<-NULL
  
  ###################################################################
  
  ###################################################################
  new_df$RATING1<-word(new_df$V1,-5)
  
  # Extracting thirdd Column
  new_df$third<-word(new_df$V1,-6)
  # Replacing __label__ with empty
  new_df$third<-gsub("__label__", "", new_df$third)
  #new_df$third<-gsub("_", " ", new_df$third)
  
  # Extracting Id from third Column
  new_df$forth<-word(new_df$third,1,sep = "_")
  
  # Extracting Existing ISQ
  new_df$fifth<-gsub(new_df$forth, "", new_df$third)
  
  #Removing column with name as Third
  new_df$third<-NULL
  
  new_df$forth<-NULL
  
  # Extracting Existing ISQ
  new_df$ISQ1<-substring(new_df$fifth,2)
  new_df$fifth<-NULL
  
  ###################################################################
  new_df<-new_df[,c(1,2,4,9,8,7,6,5,3)]
  
  # Appending rows in master data frame
  master_Data<-rbind(master_Data,new_df)
  
}
# renaming Columns

# Exporting Data 
write.csv(master_Data,file = "C:/Users/Imart/Desktop/files/outexc/output.csv",row.names = FALSE)


