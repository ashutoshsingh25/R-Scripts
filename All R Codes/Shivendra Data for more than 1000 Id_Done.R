library(RJDBC)
# Loading the RJDBC package to connect to dataabase
#install.packages("RJDBC")
#install.packages("xlsx")
#install.packages("taskscheduleR")
startDate<-Sys.Date()-8
endDate<- Sys.Date()-2
startDate <-format(startDate,format="%d-%b-%y")
endDate <-format(endDate,format="%d-%b-%y")
############################################Testing Code###############################################
# Connection for Main R
options(java.parameters = "-Xmx8000m")
# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Users/Imart/Desktop/sqldeveloper/jdbc/lib/ojdbc6.jar")

jdbcConMain <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora7-dl.intermesh.net:1521/main", "indiamart", "ref0cl39")
if(exists("jdbcConMain"))
{
  query9<-paste("SELECT STS_COMPANY.COMPANYID,
                COMPANY.COMPANY,
                COMPANY.CITY,
                COMPANY.URL,
                COMPANY.CUSTTYPE_NAME,
                NVL(SERVICE_SHORTNAME,SERVICE_NAME)SNAME,
                TO_CHAR(CUST_TO_SERV_STARTDATE,'DD-MON-YYYY') SERVICE_START_DATE,
                TO_CHAR(CUST_TO_SERV_VALIDUPTO,'DD-MON-YYYY') SERVICE_END_DATE,
                EM2.ENAME ,
                E2.EMPLOYEENAME TELE,
                EM2.HNAME,
                DECODE(CUST_TO_SERV_ENABLED,1,TO_CHAR(CUST_TO_SERV_TEMPBLOCKEDDATE,'DD-MON-YYYY'),''),
                COMPANY.COMPANYID,
                EM2.SMNAME,
                TO_CHAR(CUST_TO_SERV_DOWNLOADDATE,'DD-MON-YYYY') SERVICE_DWN_DATE,
                COMPANY.ENABLED,
                Company.STATE,
                'W' TYPE,
                (
                CASE
                WHEN EXISTS
                (SELECT 1
                FROM
                (SELECT COMPANYID FROM ITRA_COMPTOSUBCATS
                UNION
                SELECT COMPANYID FROM IHOT_COMPTOSUBCATS
                )ITRA_COMPTOSUBCATS
                WHERE COMPANYID = COMPANY.COMPANYID
                )
                THEN 'TRAVEL/HOTEL'
                ELSE 'B2B'
                END) COMP_NATURE,
                NVL(CNT1YR,0) QYR,
                NVL(FK_GLUSR_USR_ID,0) GLUSR_USR_ID ,
                V1.IIL_VERTICAL_NAME SALES_VERTICAL,
                V2.IIL_VERTICAL_NAME TELE_VERTICAL ,
                EM2.EID SALESID,
                EM2.SMID2 HOD2ID,
                EM2.SMID HOD1ID,
                EM2.HID HODID,
                E2.EMPLOYEEID TELEID ,
                DWStatus,
                TO_CHAR(DOWNLOAD_CHECKLIST_CREATION,'dd-Mon-yy') CREATEDDATE,
                TO_CHAR(REVIEW1ON,'dd-Mon-yy') REVIEW1ON,
                (SELECT EMPLOYEENAME FROM EMPLOYEE WHERE EMPLOYEEID=REVIEW1BY
                ) REVIEW1BY,
                TO_CHAR(REVIEW2ON,'dd-Mon-yy') REVIEW2ON,
                (SELECT EMPLOYEENAME FROM EMPLOYEE WHERE EMPLOYEEID=REVIEW2BY
                ) REVIEW2BY,
                TO_CHAR(DOWNLOAD_CHECKLIST_APPROVEDON,'dd-Mon-yy') APPROVEDON,
                DWREASON1,
                DWREASON2 ,
                (SELECT EMPLOYEENAME
                FROM EMPLOYEE
                WHERE EMPLOYEEID=FK_CUST_TO_SERV_SALES_HOD_ID
                ) OLD_SALES_HODNAME ,
                (SELECT IIL_VERTICAL_NAME
                FROM IIL_VERTICAL
                WHERE IIL_VERTICAL_ID=
                (SELECT FK_IIL_VERTICAL_ID
                FROM EMPLOYEE
                WHERE EMPLOYEEID=FK_CUST_TO_SERV_SALES_HOD_ID
                )
                ) OLD_VERTICAL_NAME ,
                DECODE(PAYMENT_PLAN,10,'Monthly',11,'Monthly','Annually') DEAL_TYPE ,
                TO_CHAR(
                (SELECT AR_ECS_END_DATE
                FROM AR_ECS_MASTER
                WHERE ar_ecs_id =
                (SELECT MAX(ar_ecs_id)
                FROM AR_ECS_MASTER
                WHERE FK_COMPANY_ID=COMPANY.COMPANYID
                AND FK_SERVICE_ID  = SERVICE_ID
                )
                ),'dd-Mon-yy') NACH_END_DATE ,
                (SELECT CUSTTYPE_NAME FROM CUSTTYPE WHERE CUSTTYPE_ID=OLD_CUSTTYPE_ID
                ) CUSTTYPE_NAME_OLD
                FROM CUST_TO_SERV,
                SERVICE,
                COMPANY,
                COMPANY_QUERYCOUNT,
                STS_COMPANY,
                EMPLOYEE_MANAGER EM2 ,
                EMPLOYEE E1,
                IIL_VERTICAL V1,
                EMPLOYEE E2,
                IIL_VERTICAL V2 ,
                (SELECT A.*
                FROM
                (SELECT ROW_NUMBER() OVER (PARTITION BY FK_COMPANY_ID ORDER BY DOWNLOAD_CHECKLIST_ID DESC ) RN,
                DECODE(DOWNLOAD_CHECKLIST_STATUS,'P','Pending','R','Reviewed 2','A','Approved','D','Downloaded','X','Canceled','C','Reviewed 1','M','Reviewed 1') AS DWStatus,
                FK_COMPANY_ID,
                DOWNLOAD_CHECKLIST_CREATION,
                NVL(DOWNLOAD_CHECKLIST_REVIEWCCON,DOWNLOAD_CHECKLIST_REVIEWRMON) REVIEW1ON,
                NVL(FK_EMP_REVIEWCCBY,FK_EMP_REVIEWRMBY) REVIEW1BY,
                DOWNLOAD_CHECKLIST_REVIEWON REVIEW2ON,
                FK_EMP_REVIEWBY REVIEW2BY,
                DOWNLOAD_CHECKLIST_APPROVEDON,
                DOWNLOAD_CHECKLIST_REASONDONE DWREASON1,
                DOWNLOAD_CHECKLIST_REASONDTWO DWREASON2
                FROM DOWNLOAD_CHECKLIST
                WHERE DOWNLOAD_CHECKLIST_STATUS <> 'X'
                )A
                WHERE A.RN=1
                )CHECKLIST
                WHERE COMPANY.COMPANYID = STS_COMPANY.FK_COMPANYID(+)
                AND COMPANY.COMPANYID   = CUST_TO_SERV.FK_COMPANY_ID
                AND COMPANY.COMPANYID   = COMPANY_QUERYCOUNT.COMPANYID(+)
                AND EM2.EID             =ASSIGNEDTO
                AND SERVICE_ID          = CUST_TO_SERV.FK_SERVICE_ID
                AND FK_CUSTTYPE_WEIGHT BETWEEN 500 AND 1000
                AND TRUNC(CUST_TO_SERV_STARTDATE) >= '",startDate,"'
                AND TRUNC(CUST_TO_SERV_STARTDATE) <= '",endDate,"'
                AND (SERV_UPGRADED_FRM_CUST2SERVID =-1
                OR SERV_UPGRADED_FRM_CUST2SERVID  IS NULL)
                AND ASSIGNEDTO                     =E1.EMPLOYEEID
                AND E1.FK_IIL_VERTICAL_ID          =V1.IIL_VERTICAL_ID
                AND TELEASSIGNEDTO                 =E2.EMPLOYEEID
                AND E2.FK_IIL_VERTICAL_ID          =V2.IIL_VERTICAL_ID
                AND CHECKLIST.fk_company_id(+)     = COMPANY.COMPANYID
                ORDER BY COMPANY")
  
  mainRQuery9 <- dbGetQuery(jdbcConMain,query9)
  #write.xlsx(mainRQuery9,"C:/Ankit/Automation of reports/Generated Reports//NewHosted_GlUser.xlsx",row.names=FALSE)
}


#######################################################################################################

library(RJDBC)
library(xlsx)
library(dplyr)
options(java.parameters = "-Xmx8000m")
# JDBC Driver Loading from the jar file
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")

#file<-"C:/Users/Imart/Downloads/GLUSER.xlsx"
#my_file<-read.xlsx(file, 1, header=TRUE, colClasses=NA)

my_file<-mainRQuery9[,21]
my_file<-data.frame(my_file)
my_file <- dplyr::rename(my_file, ID = my_file)
#my_file<-rename(my_file,ID=my_file)
#fix(my_file)# Assign name Id to column1

#Continue
my_file1<-my_file
my_file1<-unique(my_file1)
#write.xlsx(my_file1,"C:/Users/Imart/Downloads/export ID.xlsx",row.names = FALSE)
#my_file1$A[-length(my_file1$A)] <- paste0(my_file1$A[-length(my_file1$A)], ',')

parts<-ceiling(length(my_file1$ID)/1000)

a <- split(my_file1, factor(sort(rank(row.names(my_file1))%%parts)))#Divide into equal parts

#a[[1]]$ID[-length(a[[1]]$ID)]<-paste0(a[[1]]$ID[-length(a[[1]]$ID)],',')#Character value#

df <- data.frame(FK_GLCAT_MCAT_ID=numeric(),
                TOTAL_PRODUCTS=numeric(), 
                ITEMS_WITH_ISQ=numeric(), 
                UNIQUE_MCATS=numeric(),
                UNIQUE_MCATS_HAVING_ISQ=numeric(),
                NUM_QUESTIONS=numeric(),
                `SUM(FILLED)`=numeric(),
                `SUM(CUSTOM_ISQ_FILLED)`=numeric()) 

#Connect to IMBLR
jdbcConIMBLR <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//206.191.151.214:1521/IMBL", "indiamart", "blalrtdb4iil")

if(exists("jdbcConIMBLR"))
{

#Query for extracting data for each loop
i <- 1
  for(i in 1:length(a)){
  
  a[[i]]$ID[-length(a[[i]]$ID)]<-paste0(a[[i]]$ID[-length(a[[i]]$ID)],',')# to assign comma at end of every value
  select<-a[[i]]$ID#Extracting Id
  select<-paste(a[[i]]$ID,collapse=" ")# making different value of vector into single string so that it can be passed in query
  
  query<-paste("WITH MY_PRODS AS
  (
               SELECT * FROM
               (
               SELECT 
               GLUSR_USR_ID, PC_ITEM_ID, ITEM_MAPPING_ISPRIME, FK_GLCAT_MCAT_ID, nvl(PC_ITEM_IS_ECOM,0) PC_ITEM_IS_ECOM,PC_ITEM_STATUS_APPROVAL,
               ROW_NUMBER() OVER(PARTITION BY PC_ITEM_ID ORDER BY ITEM_MAPPING_ISPRIME NULLS LAST) RN 
               FROM
               PC_ITEM_TO_GLCAT_MCAT@MESHR MP,
               PC_ITEM@MESHR P,
               GLUSR_USR@MESHR,
               CUSTTYPE@MESHR
               WHERE 
               PC_ITEM_ID = FK_PC_ITEM_ID(+)
               AND PC_ITEM_GLUSR_USR_ID in  (" ,select, ")
               AND GLUSR_USR_ID = PC_ITEM_GLUSR_USR_ID
               AND GLUSR_USR_CUSTTYPE_ID = CUSTTYPE_ID
               AND FK_GL_COUNTRY_ISO = 'IN'
               ) WHERE RN = 1
  ), MY_ISQS AS 
               (
               SELECT GLCAT_MCAT_ID, COUNT(IM_SPEC_MASTER_ID) NUM_QUESTIONS FROM 
               (
               select GLCAT_MCAT_ID , IM_SPEC_MASTER_ID, IM_SPEC_MASTER_DESC
               from im_cat_specification a, glcat_mcat b , IM_SPECIFICATION_MASTER
               where IM_CAT_SPEC_CATEGORY_ID = glcat_mcat_id and im_cat_spec_category_type =3 and im_cat_spec_status=1
               and FK_IM_SPEC_MASTER_ID = IM_SPEC_MASTER_ID
               and IM_CAT_SPEC_PRIORITY <> -1
               AND IM_SPEC_MASTER_BUYER_SELLER <> 1
               union
               select fk_glcat_mcat_id AS c, FK_IM_SPEC_MASTER_ID AS cnt_avail, IM_SPEC_MASTER_DESC
               from im_cat_specification, glcat_cat_to_mcat,IM_SPECIFICATION_MASTER,
               ( select im_cat_spec_category_id MCAT_EXCLUDE_ID from im_cat_specification where im_cat_spec_category_type =3 and im_cat_spec_status=1 )
               where im_cat_spec_category_type =2 and im_cat_spec_status=1
               and im_cat_spec_category_id = fk_glcat_cat_id
               and FK_IM_SPEC_MASTER_ID = IM_SPEC_MASTER_ID
               and fk_glcat_mcat_id = MCAT_EXCLUDE_ID(+)
               AND MCAT_EXCLUDE_ID IS NULL
               and IM_CAT_SPEC_PRIORITY <> -1
               AND IM_SPEC_MASTER_BUYER_SELLER <> 1
               ) GROUP BY GLCAT_MCAT_ID
               ), MY_FILLED_ISQS AS
               (
               SELECT 
               PC_ITEM_ATTRIBUTE.FK_PC_ITEM_ID,
               COUNT(DISTINCT DECODE(FK_IM_SPEC_MASTER_ID,-1,null,FK_IM_SPEC_MASTER_ID)) QUESTIONS_FILLED,
               COUNT(DISTINCT DECODE(FK_IM_SPEC_MASTER_ID,-1,PC_ITEM_ATTRIBUTE_ID)) CUSTOM_ISQ_FILLED
               FROM
               PC_ITEM_TO_GLCAT_MCAT@MESHR MP,
               PC_ITEM@MESHR P,
               GLUSR_USR@MESHR,
               CUSTTYPE@MESHR,
               PC_ITEM_ATTRIBUTE@MESHR
               WHERE 
               PC_ITEM_ID = MP.FK_PC_ITEM_ID(+)
               AND PC_ITEM_ID = PC_ITEM_ATTRIBUTE.FK_PC_ITEM_ID
               AND PC_ITEM_GLUSR_USR_ID in  (",select,")
               AND GLUSR_USR_ID = PC_ITEM_GLUSR_USR_ID
               AND GLUSR_USR_CUSTTYPE_ID = CUSTTYPE_ID
               AND FK_GL_COUNTRY_ISO = 'IN'
               GROUP BY PC_ITEM_ATTRIBUTE.FK_PC_ITEM_ID
               )
               select FK_GLCAT_MCAT_ID,count(pc_item_id) TOTAL_PRODUCTS, COUNT(DECODE(NVL(NUM_QUESTIONS,0),0,NULL,1)) ITEMS_WITH_ISQ, 
               COUNT(DISTINCT FK_GLCAT_MCAT_ID) UNIQUE_MCATS,
               COUNT(DISTINCT DECODE(NVL(NUM_QUESTIONS,0),0,NULL,FK_GLCAT_MCAT_ID)) UNIQUE_MCATS_HAVING_ISQ, sum(NUM_QUESTIONS) NUM_QUESTIONS,
               sum(filled), sum(CUSTOM_ISQ_FILLED)
               from
               (
               SELECT FK_GLCAT_MCAT_ID, PC_ITEM_ID,NUM_QUESTIONS, (case when NUM_QUESTIONS = 0 or NUM_QUESTIONS is null then 0 else QUESTIONS_FILLED end)filled,CUSTOM_ISQ_FILLED 
               FROM MY_PRODS, MY_ISQS , MY_FILLED_ISQS
               WHERE FK_GLCAT_MCAT_ID = GLCAT_MCAT_ID(+)
               AND PC_ITEM_ID = FK_PC_ITEM_ID(+))
               group by FK_GLCAT_MCAT_ID")
  
  
  imblrQuery <- dbGetQuery(jdbcConIMBLR,query)
  df<-rbind(df,imblrQuery)
  
}

}
df1<-df

s1<-sqldf("select FK_GLCAT_MCAT_ID,sum(TOTAL_PRODUCTS) as TOTAL_PRODUCTS,sum(ITEMS_WITH_ISQ) as ITEMS_WITH_ISQ,sum(UNIQUE_MCATS) as UNIQUE_MCATS,sum(UNIQUE_MCATS_HAVING_ISQ) as UNIQUE_MCATS_HAVING_ISQ,sum(NUM_QUESTIONS) as NUM_QUESTIONS ,sum(`SUM(FILLED)`) as `SUM(FILLED)` ,sum(`SUM(CUSTOM_ISQ_FILLED)`) as `SUM(CUSTOM_ISQ_FILLED)` from df1 group by FK_GLCAT_MCAT_ID", method = "raw")
write.xlsx(s1,"C:/Users/Imart/Downloads/export.xlsx",row.names = FALSE)









