
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


#df <- read.table(file = "c:/Users/imart/Desktop/PARUL_Processed.txt",sep = "\n")

Data1 = choose.files()

df <- read.table(file = Data1,sep = "\n")

df$gl_sid_variant_id <- word(df$V1,1,sep = "\t")

df$gl_sid_variant_name <- word(df$V1,2,sep = "\t")

df$fk_glcat_mcat_id <- word(df$V1,3,sep = "\t")

df$glcat_mcat_name <- word(df$V1,4,sep = "\t")


df$gl_sid_to_glcat_mcat_isprime <- word(df$V1,5,sep = "\t")

df1 <- df[,-1]
gsub(".*       (.*)   .*", "\\1", as.character(df$V1[1]))
df1$gl_sid_variant_name <- trimws(df1$gl_sid_variant_name)

Final_data <- df[,-1]

Final_data$fk_glcat_mcat_id <- as.numeric(Final_data$fk_glcat_mcat_id)
Final_data$gl_sid_variant_id <- as.numeric(Final_data$gl_sid_variant_id)
Final_data$gl_sid_to_glcat_mcat_isprime <- as.numeric(Final_data$gl_sid_to_glcat_mcat_isprime)
#df1$V5 <- gsub(".*       (.*)   .*", "\\1", df1$V1)

write.xlsx(Final_data,"processed_data.xlsx",row.names = FALSE,showNA = F,col.names = TRUE)





