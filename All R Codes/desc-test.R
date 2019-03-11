#DATASET 3
library(readxl)
library(discretization)
library(dplyr)
library(arules)
ls("package:discretization")
?mdlp
#ISQ Probability
#dfC <- read_excel("C:/Users/IMART/Downloads/o_c.xlsx")
dfC <- read.csv("E:/Pricing_analytics/final_AOV_data_city_paid_seller.csv")
dfc2 <- dfC[,c(2,4,10)]
dfc2$MCAT_City <- paste(dfc2$GLCAT_MCAT_NAME,dfc2$GL_CITY_NAME``)
dfc2 <- dfc2[,c(4,3)]
class(dfc2$MCAT_City)
dfc2$MCAT_City <- as.factor(dfc2$MCAT_City)
#df1<-dfC[,c(8,2)]
df2 <- dfC[1:15000,c(10,2)]
dfC <- NULL
dfc2 <- dfc2[,c(2:1)]
c3 <- mdlp(dfc2)$Disc.data
df1$`PMCAT Name` <- "Label"
c_2c <- mdlp(df_sold_ratio)$Disc.data

dfc3 <- dfc2[,1]
dfc3 <- as.data.frame(dfc3)
class(dfc3$dfc3)
c4 <- discretize(dfc3$dfc3,method = "frequency",breaks = 2)
#df1$`PMCAT Name`<-as.factor(df1$`PMCAT Name`)
class(df2$GLCAT_MCAT_NAME)
class(df2$Sold_Ratio_UBL)
df2 <- df2[!is.na(df2$GLCAT_MCAT_NAME),]
df2<-as.data.frame(df2)
df2[is.na(df2)]<-0
c2 <-mdlp(df2)$Disc.data
c2 <- df2 
  class(df2$Sold_Ratio_UBL)
c1 <- discretize(df1$Sold_Ratio_UBL,breaks = 2)

#c2 <- c2 %>% dplyr::mutate(c2$descretized =discretize(df1$Sold_Ratio_UBL,breaks = 2))

c2 <- c2 %>% mutate_if(is.numeric, funs(discretize(c2$Sold_Ratio_UBL, method="frequency",breaks = 2)))

c2$old <- df2$Sold_Ratio_UBL

#mutate()
?mutate
c1 <- mdlp(df1)$Disc.data
df1$'DiscretizedProbabiltyISQ' <- c1$Probability_ISQ




#CHEKCS

#DATASET 3
library(readxl)
library(discretization)
library(dplyr)
ls("package:discretization")
?mdlp
#ISQ Probability
dfC <- read_excel("C:/Users/Imart/Downloads/o_a.xlsx")
#dfC <- read.csv("E:/Pricing_analytics/final_AOV_data_city_paid_seller.csv")
df1<-dfC[,c(9,2)]
#df1 <- dfC[,c(2,4,10)]
df1$`PMCAT Name`<-as.factor(df1$`PMCAT Name`)
#class(df1$GLCAT_MCAT_NAME)
df1<-as.data.frame(df1)
df1[is.na(df1)]<-0
c1 <-mdlp(df1)$Disc.data
c1 <- mdlp(df1)$Disc.data
df1$'DiscretizedProbabiltyISQ' <- c1$Probability_ISQ