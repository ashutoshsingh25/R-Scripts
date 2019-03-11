library(h2o)
?h2o
library(RJDBC)
library(xlsx)
library(dplyr)

input_file <- readxl::read_excel("C:/Users/Imart/Downloads/Complete List of PMCATs for Flagging.xlsx",sheet = 1)
my_file<-input_file[,1]
my_file<-data.frame(my_file)
my_file <- dplyr::rename(my_file, ID = PARENT_MCAT_ID)

my_file1<-my_file
my_file1<-unique(my_file1)
#write.xlsx(my_file1,"C:/Users/Imart/Downloads/export ID.xlsx",row.names = FALSE)
#my_file1$A[-length(my_file1$A)] <- paste0(my_file1$A[-length(my_file1$A)], ',')

parts<-ceiling(length(my_file1$ID)/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts


df <- data.frame(MCAT_ID=numeric(),
                 FK_IM_SPEC_MASTER_ID=numeric(), 
                 IM_SPEC_AFFIX_TYPE=character(), 
                 CUSTOM_STANDARD=character(),
                 UGC=character(),
                 GLCAT_MCAT_NAME=character(),
                 MASTER_DESC=character(),
                 OPTIONS_DESC=character(),
                 OPTION_DESC_ORIG=character(),
                 `CONCAT('__LABEL__',MCAT_ID||'_'||MASTER_DESC1||''||MASTER_DESC||''||OPTIONS_DESC)`=character(),
                 RN=numeric()) 
options(java.parameters = "-Xmx8000m")
# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Imart/Desktop/sqldeveloper/jdbc/lib/ojdbc6.jar")


jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")

if(exists("jdbcConIMBLR"))
{
  
  #Query for extracting data for each loop

  for(i in 1:length(a)){
    
    a[[i]]$ID[-length(a[[i]]$ID)]<-paste0(a[[i]]$ID[-length(a[[i]]$ID)],',')# to assign comma at end of every value
    select<-a[[i]]$ID#Extracting Id
    select<-paste(a[[i]]$ID,collapse=" ")# making different value of vector into single string so that it can be passed in query
    
    query<-paste("SELECT  MCAT_ID,FK_IM_SPEC_MASTER_ID,IM_SPEC_AFFIX_TYPE,decode(FK_IM_SPEC_MASTER_ID,-1,'Custom','Standard') CUSTOM_STANDARD, 
 
                 decode(IM_SPEC_OPTIONS_DESC,'Other','UGC',NULL) UGC, 
                 
                 GLCAT_MCAT_NAME,MASTER_DESC, OPTIONS_DESC , OPTION_DESC_ORIG, 
                 
                 
                 
                 
                 CONCAT('__label__' , MCAT_ID || '_' || MASTER_DESC1 || ' ' || MASTER_DESC || ' ' || OPTIONS_DESC), 
                 
                 row_number() over ( partition by GLCAT_MCAT_NAME order by GLCAT_MCAT_NAME) as rn 
                 
                 FROM 
                 
                 ( 
                 
                 select MCAT_ID,FK_IM_SPEC_MASTER_ID,IM_SPEC_OPTIONS_DESC, REPLACE(lower(glcat_mcat_name),' ','_') GLCAT_MCAT_NAME, 
                 
                 REPLACE(LTRIM(RTRIM(REGEXP_REPLACE(FK_IM_SPEC_MASTER_DESC ,'[^[:alpha:][:digit:]-]',' '))),' ','_') MASTER_DESC1, 
                 
                 LTRIM(RTRIM(REGEXP_REPLACE(FK_IM_SPEC_MASTER_DESC ,'[^[:alpha:][:digit:]-]',' '))) MASTER_DESC, 
                 
                 
                 
                 
                 REPLACE(LTRIM(RTRIM(REGEXP_REPLACE(FK_IM_SPEC_OPTIONS_DESC ,'[^[:alpha:][:digit:]-]',' '))),'-',' ') OPTIONS_DESC, 
                 
                 OPTION_DESC_ORIG,IM_SPEC_AFFIX_TYPE 
                 
                 from  
                 
                 ( 
                 
                 select distinct PC_ITEM_ATTRIBUTE_MCATID as MCAT_ID,PC_ITEM_ATTRIBUTE.FK_IM_SPEC_MASTER_ID, REPLACE(lower(FK_IM_SPEC_MASTER_DESC),' ','_') FK_IM_SPEC_MASTER_DESC ,IM_SPEC_OPTIONS_DESC ,FK_IM_SPEC_OPTIONS_DESC OPTION_DESC_ORIG, 
                 
                 LOWER(FK_IM_SPEC_OPTIONS_DESC) FK_IM_SPEC_OPTIONS_DESC 
                 
                 from 
                 
                 PC_ITEM_ATTRIBUTE@MESHR,IM_SPECIFICATION_OPTIONS 
                 
                 where PC_ITEM_ATTRIBUTE_MCATID in ( ",select,") 
                 
                 and PC_ITEM_ATTRIBUTE.FK_IM_SPEC_MASTER_ID <>-1 
                 
                 and PC_ITEM_ATTRIBUTE.FK_IM_SPEC_MASTER_ID = IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID(+) 
                 
                 union 
                 
                 select distinct IM_CAT_SPEC_CATEGORY_ID as MCAT_ID ,IM_CAT_SPECIFICATION.FK_IM_SPEC_MASTER_ID, REPLACE(lower(IM_SPEC_MASTER_DESC),' ','_') FK_IM_SPEC_MASTER_DESC , IM_SPEC_OPTIONS_DESC,IM_SPEC_OPTIONS_DESC OPTION_DESC_ORIG, 
                 
                 LOWER(IM_SPEC_OPTIONS_DESC) FK_IM_SPEC_OPTIONS_DESC 
                 
                 from im_cat_specification,im_specification_master,IM_SPECIFICATION_OPTIONS 
                 
                 where IM_CAT_SPECIFICATION.FK_IM_SPEC_MASTER_ID=IM_SPEC_MASTER_ID 
                 
                 and IM_CAT_SPEC_CATEGORY_TYPE=3 
                 
                 and IM_CAT_SPEC_STATUS=1 
                 
                 and  IM_CAT_SPECIFICATION.FK_IM_SPEC_MASTER_ID = IM_SPECIFICATION_OPTIONS.FK_IM_SPEC_MASTER_ID 
                 
                 and IM_SPEC_OPTIONS_STATUS =1 
                 
                 and IM_SPEC_MASTER_BUYER_SELLER <> 1 
                 
                 and IM_CAT_SPEC_CATEGORY_ID in (",select,") 
                 
                 ) C, GLCAT_MCAT,im_specification_master 
                 WHERE C.MCAT_ID = GLCAT_MCAT_ID  
                 
                 
                 and C.FK_IM_SPEC_MASTER_ID = IM_SPEC_MASTER_ID 
                 AND C.MCAT_ID = GLCAT_MCAT_ID  
                 
                 )"
)
    
    
    imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
    df<-rbind(df,imblrQuery)
    
  }
  
}
df1<-df

#s1<-sqldf("select FK_GLCAT_MCAT_ID,sum(TOTAL_PRODUCTS) as TOTAL_PRODUCTS,sum(ITEMS_WITH_ISQ) as ITEMS_WITH_ISQ,sum(UNIQUE_MCATS) as UNIQUE_MCATS,sum(UNIQUE_MCATS_HAVING_ISQ) as UNIQUE_MCATS_HAVING_ISQ,sum(NUM_QUESTIONS) as NUM_QUESTIONS ,sum(`SUM(FILLED)`) as `SUM(FILLED)` ,sum(`SUM(CUSTOM_ISQ_FILLED)`) as `SUM(CUSTOM_ISQ_FILLED)` from df1 group by FK_GLCAT_MCAT_ID", method = "raw")
write.xlsx(df1,"E:/Ashutosh/export.xlsx",row.names = FALSE,showNA = F)
