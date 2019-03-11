library(tm)
library(psych)
library("readxl")
library("xlsx")
library("stringr")
library("stringi")
library("dplyr")
library("stopwords")
library("tm")
library("qdapRegex")
library(reshape2)

df <- read.csv("C:/Users/Imart/search.csv")

colnames(df)[1] <- "S.No"
  colnames(df)[2] <- "Search_Term"
colnames(df)[3] <- "MCAT_ID"
colnames(df)[4] <- "CAT_ID"
colnames(df)[5] <- "Search_Freq"
colnames(df)[6] <- "Date"
  colnames(df)[7] <- "Result_Count"
nrow(df)
dim(df)
colnames(df)
df$Search_Term <- tolower(df$Search_Term)

df$Search_Term <- as.character(df$Search_Term)
#Creating a function to remove

Funrem_stop <- function(x)
{
  Stopwords <- c("manufacturers","manufacturer","wholesaler","wholesalers","Retailer","Retailers","exporter","exporters","retailer","retailers","dealer","dealers","indiamart")
  x <- removeWords(x,Stopwords)
  return(x)
}
#city <- readxl::read_excel("E:/Related Search Data/City Names Indian.xlsx",col_names = T)
#city$GL_CITY_NAME <- tolower(city$GL_CITY_NAME)
stopwords = readLines('E:/Related Search Data/city_names.txt')     #Your stop words file

# Function to remove stopwords
removeWords1 <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}


df$Serach_term_new <- lapply(df$Search_Term,Funrem_stop)
df$Serach_term_new <- as.character(df$Serach_term_new)
df$Serach_term_new <- rm_white(df$Serach_term_new)
for(i in 1:nrow(df))
{
  df$Serach_term_new[i]<-removeWords1(df$Serach_term_new[i], stopwords)
  
}
df$Serach_term_new2 <- as.character(df$Serach_term_new)
df$Serach_term_new2 <- rm_white(df$Serach_term_new2)
#selecting only words avauilable before in and from
df$Serach_term_new3 <- sapply(strsplit(df$Serach_term_new2," in$"),"[",1)
df$Serach_term_new_final <- sapply(strsplit(df$Serach_term_new3," from "),"[",1)
df$Serach_term_new_final <- as.character(df$Serach_term_new_final)

#removing extra spaces:
df$Serach_term_new_final <- rm_white(df$Serach_term_new_final)

#Creating pivot

result <- df %>% dplyr::group_by(Serach_term_new_final) %>% summarise(Sum_serachFreq = sum(Search_Freq,na.rm = T))

write.csv(result,"Result.csv",row.names = F,quote = F)

