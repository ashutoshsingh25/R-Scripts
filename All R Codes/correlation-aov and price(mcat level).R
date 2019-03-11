require(psych)
require(GPArotation)
require(readxl)
require(rapportools)
ls('package:GPArotation')
ls('package:psych')
?cor
#Correlation matrix between MCAT wise AOV(Q3) and MCAt wise Price(Q3):

#Loading data_set

df <- readxl::read_excel("E:/Pricing_analytics/mcat_wise_AOV-price_finalV2.xlsx",sheet = 1)
sum(is.na(df$`Product Q3`))
df1 <- df[!is.na(df$`Product Q3`),] # only 57k out of 81k mcats having both AOV and price(Q3) values

colnames(df1)
#variable selection for correlation matrix

var <- c("MCAT_NAME","MCAT Q3","Product Q3")

df1 <- df1[,var]

colnames(df1)[names(df1)=="MCAT Q3"] <- "AOV_Q3"

num_data <- sapply(df1, is.numeric)

class(df1$AOV_Q3)
class(df1$`Product Q3`)

#getting numeric variable from data to create correlation matrix

data_numeric <- df1[,num_data]

coorm <- cor(data_numeric)  #correlation matrix

write.csv(coorm,"E:/Pricing_analytics/correlation_matrix_price.csv")

ncol(data_numeric)
??mdl

cor.plot(data_numeric,numbers = T,colors = T)

install.packages("corrplot")
library(corrplot)
?corrplot
corrplot(coorm,method = "color",is.corr = T)


