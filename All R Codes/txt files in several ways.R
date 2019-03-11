# To get list of all files inside a directory
check<-list.files("E:/Data", pattern=NULL, all.files=FALSE,full.names=FALSE)
# Converting those into dataframe
check<-as.data.frame(check)
# Output
write.xlsx(check,"C:/Ankit/Automation of reports/Abhishek Work/Output/CCorpus (D drive).xlsx")

#dir(path, pattern=NULL, all.files=FALSE,full.names=FALSE)

#Another way to reas all the files inside directory
remaining<-dir("F:/fasttext/rasika corpus")
remaining<-data.frame(remaining)
write.xlsx(check,"C:/Ankit/Automation of reports/Abhishek Work/Output/remainig corpus.xlsx")

# Removing the extension from column. for eg the files are 1.txt,2.txt... etc inside a directory, we want to delete '.txt' use below code
char_array <- check$check
a <-data.frame("data"=char_array)
a$data<-as.character(a$data)
a$data <-substr(a$data,1,nchar(a$data)-4)


char_array <- remaining$remaining
a <-data.frame("data"=char_array)
a$data<-as.character(a$data)
a$data <-substr(a$data,1,nchar(a$data)-4)

# Storing all the dis inside a vector
id<-as.character(check$check)

# Folowing loop would create the empty directories/folders with name as that of id
for(j in 1:length(id))
{
  path<-"C:/Ankit/Abhishek Transfered Data/Final Data_16Apr2018/New folder/"
  path<-paste0(path,id[j])
  dir<-dir.create(path)
}


for(j in 1:length(id))
{
  # Get the folder location from where files needs to be copied
  tot_files<-paste0("C:/Ankit/Abhishek Transfered Data/Final Data_16Apr2018/Custom/",id[j])
  # List of txt files inside those directory
  list.of.files <- list.files(tot_files,".txt$")
  # Location where files needs to be copied
  new.folder <- paste0("C:/Ankit/Abhishek Transfered Data/Final Data_16Apr2018/New folder/",id[j])
  for (i in 1:length(list.of.files)) 
  {	# Copy each file and paste it to newfolder location
    test<-paste0("C:/Ankit/Abhishek Transfered Data/Final Data_16Apr2018/Custom/",id[j],"/",id[j])
    test<-paste0(test,"_",i,".txt")
    file.copy(test, new.folder)
  }
  
}

# Code to copy combined data like 2.txt containing content like 2_1.txt,2_2.txt....etc 
for(j in 1:length(id))
{
  test<-paste0("C:/Ankit/Abhishek Transfered Data/Final Data_16Apr2018/Combined/",id[j],".txt")
  new.folder <- paste0("D:/Ankit/combined rasika")
  file.copy(test, new.folder)
}
