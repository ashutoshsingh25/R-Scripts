
library(RJDBC)
library(xlsx)
library(dplyr)
library(sqldf)
library(stringr)
options(java.parameters = "-Xmx8000m")


BL_PATH <- choose.dir()
dir.create(paste0(BL_PATH,"/Train"),showWarnings = F)
dir.create(paste0(BL_PATH,"/Test"),showWarnings = F)

startDate<-Sys.Date()-30
endDate<- Sys.Date()-1
startDate <-format(startDate,format="%d-%b-%y")
endDate <-format(endDate,format="%d-%b-%y")
############################################Testing Code###############################################
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Imart/Desktop/sqldeveloper/jdbc/lib/ojdbc6.jar")

jdbcReportDB <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora9.intermesh.net:1521/STSGEN", "report", "report")

#Change rownum values as per the requirement

Query <- paste("select blstudy_atul.*,SECONDARY_MCAT, PRIME_MCAT_NAME, FREQUENCY, LOOKING_FOR_SUPPLIERS, PORPOSE
from blstudy_atul, blstudy_atul2
where blstudy_atul.ETO_OFR_DISPLAY_ID = blstudy_atul2.ETO_OFR_DISPLAY_ID
and rownum <=200000")



QueryResult <- dbGetQuery(jdbcReportDB,Query)


# BL Saleability
library(qdapRegex)

#File <- choose.files()  # Select excel file

#df <- readxl::read_excel(File,sheet = 1) 

df <- QueryResult


write.csv(df,"2lakhs_dump.csv",row.names = F,quote = F)

df1 <- df[order(df$ETO_OFR_DISPLAY_ID),]
#df1$FK_IM_SPEC_OPTIONS_DESC <- gsub("_"," ",df1$FK_IM_SPEC_OPTIONS_DESC)

df1$DUP_CHECK <- paste(df1$ETO_OFR_DISPLAY_ID,df1$FK_IM_SPEC_MASTER_DESC,df1$FK_IM_SPEC_OPTIONS_DESC)

df1 <- df1[!duplicated(df1$DUP_CHECK),]

df1$DUP_CHECK <- NULL
df1$OPTION_DESC_A <- ""

df1$OPTION_DESC_A[1] <- df1$FK_IM_SPEC_OPTIONS_DESC[1]
j <- 1

for (j in 1:nrow(df1)) {
  ifelse(df1$ETO_OFR_DISPLAY_ID[j+1]==df1$ETO_OFR_DISPLAY_ID[j],df1$OPTION_DESC_A[j+1] <- as.character(paste(df1$OPTION_DESC_A[j],df1$FK_IM_SPEC_OPTIONS_DESC[j+1])),df1$OPTION_DESC_A[j+1] <- as.character(df1$OPTION_DESC_A[j+1]))
}


df1$OPTION_DESC_B <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$ETO_OFR_DISPLAY_ID[i]!=df1$ETO_OFR_DISPLAY_ID[i+1],df1$OPTION_DESC_B[i] <- as.character(df1$OPTION_DESC_A[i]),df1$OPTION_DESC_B[i] <- "aaaooovvv")
}



df1 <- df1[df1$OPTION_DESC_B!="aaaooovvv",]

df1$S_NS <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$UNIQUE_SOLD[i]==1,df1$S_NS[i] <- "Sold",df1$S_NS[i] <- "Not_Sold")
}

df1$Retail_NR <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$RETAIL_FLAG[i]==1,df1$Retail_NR[i] <- "retail",df1$Retail_NR[i] <- "non_retail")
}

df1$Email_NE <- ""

for (i in 1:nrow(df1)) {
  ifelse(df1$EMAIL_FLAG[i]==1,df1$Email_NE[i] <- "email",df1$Email_NE[i] <- "no_email")
}

df1$FK_IM_SPEC_OPTIONS_DESC <- gsub("_"," ",df1$FK_IM_SPEC_OPTIONS_DESC)
#df1$ETO_OFR_TITLE <- gsub("_"," ",df1$ETO_OFR_TITLE)
#df1$GLUSR_USR_CITY <- gsub("_"," ",df1$GLUSR_USR_CITY)
df1$GLUSR_USR_CITY <- gsub("0","",df1$GLUSR_USR_CITY)
df1$PRIME_MCAT_NAME <- sub("_"," ",df1$PRIME_MCAT_NAME)
df1$SECONDARY_MCAT <- sub("_"," ",df1$SECONDARY_MCAT)
df1$PRIME_MCAT_NAME <- gsub("pmcat","",df1$PRIME_MCAT_NAME)
df1$SECONDARY_MCAT <- gsub("smcat","",df1$SECONDARY_MCAT)
df1$SECONDARY_MCAT <- rm_white(df1$SECONDARY_MCAT)
df1$PRIME_MCAT_NAME <- rm_white(df1$PRIME_MCAT_NAME)
df1$FREQUENCY <- gsub("0","",df1$FREQUENCY)
df1$PORPOSE <- gsub("0","",df1$PORPOSE)
df1$LOOKING_FOR_SUPPLIERS <- gsub("0","",df1$LOOKING_FOR_SUPPLIERS)


#Removing row items from Secondary mcats where prime_mcat=secondary_mcat

Index_MCAT <- df1$PRIME_MCAT_NAME == df1$SECONDARY_MCAT

Index_val <- which(Index_MCAT==T)

df1$SECONDARY_MCAT[Index_val] <- ""

df1$L_M_H <- ""

for (i in 1:nrow(df1)) {
  ifelse((df1$TOTAL_SOLD[i]>=1 && df1$TOTAL_SOLD[i]<=3),df1$L_M_H[i] <- "Low_Sold",
         ifelse((df1$TOTAL_SOLD[i]>=4 && df1$TOTAL_SOLD[i]<=7),df1$L_M_H[i] <- "Medium_Sold",
                ifelse(df1$TOTAL_SOLD[i]>7,df1$L_M_H[i] <- "High_Sold",df1$L_M_H[i] <- "Not_Sold")))
}

table(df1$L_M_H)
table(df1$S_NS)

df1$Concat <- paste(df1$ETO_OFR_TITLE,df1$OPTION_DESC_B,df1$GLUSR_USR_CITY,df1$PRIME_MCAT_NAME,df1$SECONDARY_MCAT,df1$FREQUENCY,df1$LOOKING_FOR_SUPPLIERS,df1$PORPOSE,df1$Retail_NR,df1$Email_NE)

df1$Concat <- tolower(df1$Concat)
df1$Concat <- rm_white(df1$Concat)

sum(grepl("[[:punct:]]",df1$Concat))

#df1$Concat <- gsub("[[:punct:]]","",df1$Concat)

df1$Label <- paste0("__label__",df1$S_NS," ",df1$Concat)

df1$Label_LMH <- paste0("__label__",df1$L_M_H," ",df1$Concat)

length(unique(df1$ETO_OFR_DISPLAY_ID))


df1$Label[seq(2,50,3)]
#df1$Label_LMH[seq(2,26,2)]

df2 <- df1[order(df1$ETO_OFR_DISPLAY_ID),]

df_SOLD <- df1[df1$S_NS=="Sold",]
df_Not_SOLD <- df1[df1$S_NS=="Not_Sold",]

#randomizing data so as to select random data-set for training and testing
 
df3 <- df2[sample(nrow(df2)),]

write.csv(df3,"processed_2lakhs.csv",row.names = F,quote = F)

Train_Index <- seq(1:floor(nrow(df3)*0.80))

Train_data <- df3[Train_Index,]
Test_data <- df3[-Train_Index,]

table(Train_data$S_NS)
table(Test_data$S_NS)



write.table(Train_data$Label,paste0(BL_PATH,"/Train/train.txt"),sep = "\t",row.names = F,col.names = F,quote = F)
write.table(Test_data$Concat,paste0(BL_PATH,"/Test/test.txt"),sep = "\t",row.names = F,col.names = F,quote = F)

write.csv(df3,paste0(BL_PATH,"/data_final.csv"),row.names = F,quote = F)
write.csv(Test_data,paste0(BL_PATH,"/test_data_vlookup2.csv"),row.names = F,quote = F)

Train_concat <- Train_data[Train_data$S_NS=='Sold',]
write.table(Train_concat$Label,paste0(BL_PATH,"/Train/train_concat.txt"),sep = "\t",row.names = F,col.names = F,quote = F)


CHeck <- read.table("E:/BL_Saleability/Train/train.txt",sep = "\t")
CHeck$S_NS <- word(CHeck$V1,1,sep = " ")

table(CHeck$S_NS)
