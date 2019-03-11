#installing package -- one time operation





# Load packages:

library(xlsx)
library(dplyr)
library(sqldf)
library(stringi)
library(stringr)
library(rapportools)
library(readxl)
options(scipen = 999)

Input_Data <- readxl::read_excel("C:/Users/imart/Desktop/SID_R_Check/SID-Washing Machine (1)-Sample-Input-File.xlsx",sheet = 1,col_names = F)
df1 <- Input_Data[-c(6:15),]

df2 <- df1[-c(1:6),]

#Extracting column names from rowsand seperating data from original file.

#colnames(df2) = df2[1,]

colnames(df2) <- as.character(unlist(df2[1,]))
df2 = df2[-1,]  # data agaist reference for check1 and check2

df3 <- df1[,-c(1:46)]

colnames(df3) <- as.character(unlist(df3[1,]))
#df3 <- df3[-1,] # Key/config data

#check1 Reference should not contain any space , cannot be blank
i <- 1
for (i in 1:nrow(df2)) {
  ifelse(df2$reference[i]=="",df2$reference[i] <- "Error- Invalid Link",df2$reference[i] <- as.character(df2$reference[i]))
}


#?grepl
# check for sapces if avaialble

a <- grepl("\\s", df2$reference)
a <- as.character(a)
a <- as.data.frame(a)
df2a <- cbind(df2,a)
df2a$reference <- as.character(df2a$reference)
j <- 1
for (j in 1:nrow(df2a)) {
  ifelse(df2a$a[j]=="TRUE",df2a$reference[j] <- paste0("Error- Invalid Link with space"),df2a$reference[j] <- as.character(df2$reference[j]))
  
}


#check2 - Brand mcat id Should be only Numeric with no alphabet or space
df2a$brand_mcat_id <- trimws(df2a$brand_mcat_id)
b <- grepl("\\D",df2a$brand_mcat_id)
b <- as.character(b)
b <- as.data.frame(b)
df2a <- cbind(df2a,b)
df2a$brand_mcat_id <- as.character(df2a$brand_mcat_id)
class(df2a$brand_mcat_id)

#df2a$brand_mcat_id[5:8] <- "acd"
df2a$b <- as.character(df2a$b)
#df2a$b[3] <- "TRUE"
k <- 1
for (k in 1:nrow(df2a)) {
  ifelse(df2a$b[k]=="TRUE",df2a$brand_mcat_id[k] <- paste0("Error-MCAT ID non numeric"),df2a$brand_mcat_id[k] <- as.character(df2a$brand_mcat_id[k]))
  
}

#grep("\\D",str)

#class(b)

#b <- df2$brand_mcat_id
#b <- as.character(b)
#b[5:8,] <- "ashutosh"

#check3  brand_mcat_name <- Same as main sheet with no double space

#test <- trimws("Ashutosh s ")


#check4 <- pmcat id Should be only Numeric with no alphabet or space
df2a$pmcat_id <- trimws(df2a$pmcat_id)
c <- grepl("\\D",df2a$pmcat_id)
c <- as.character(c)
c <- as.data.frame(c)
df2a <- cbind(df2a,c)
df2a$pmcat_id <- as.character(df2a$pmcat_id)
class(df2a$pmcat_id)

#df2a$brand_mcat_id[5:8] <- "acd"
df2a$c <- as.character(df2a$c)
#df2a$b[3] <- "TRUE"
k <- 1

for (k in 1:nrow(df2a)) {
  ifelse(df2a$c[k]=="TRUE",df2a$pmcat_id[k] <- paste0("Error-PMCAT ID not numeric"),df2a$pmcat_id[k] <- as.character(df2a$pmcat_id[k]))
  
}


df2a$variant_name <- as.character(df2a$variant_name)
df2a[is.na(df2a$variant_name),c('variant_name')] <- "NA"
for (k in 1:nrow(df2a)) {
  
  ifelse((df2a$`standard/variant`[k]=="Variant" && df2a$variant_name[k]=="NA"),df2a$variant_name[k] <- "Error- Variant name can't be blank",df2a$variant_name[k] <- as.character(df2a$variant_name[k]))
  
}
class(df2a$`standard/variant`)
for (k in 1:nrow(df2a)) {
  
  ifelse((df2a$`standard/variant`[k]=="Standard" && df2a$variant_name[k]!="NA"),df2a$variant_name[k] <- "Error- For standard, Variant name should be blank",df2a$variant_name[k] <- as.character(df2a$variant_name[k]))
  
}


#checks for duplicacy of string- Standard Product Name should not be duplicate. Similar for varinat name

#newdf_var <- newdf4
V1<-c('Standard','Variant','Standard','Standard','Variant','Variant')
V2<-c('LG Mobile 500X','GX 500 Ultra','LG Mobile 500X','Mi Mobile Phone','redmi 5A','redmi 5A')

res1<-as.data.frame(cbind(V1,V2))
data<-res1
#data[which(duplicated(data[,c]))]

index<-which(duplicated(data[,c('V1','V2')])==T)

index_1 <- which(duplicated(df2a[,c('standard_product','variant_name')])==T)

for (i in 1:length(index_1)) {
  df2a$variant_name[index_1[i]] <- "Error-Two variants can't be same for one standard product"
}


resq1 <- df2a[,c('standard/variant','standard_product')] 
class(resq1$`standard/variant`)

for (i in 1:nrow(resq1)) {
  ifelse(resq1$`standard/variant`[i]=="Variant",resq1$`standard/variant`[i] <- i,resq1$`standard/variant`[i] <- resq1$`standard/variant`[i])
}

index_2 <- which(duplicated(resq1[,c('standard/variant','standard_product')]))

for (j in 1:length(index_2)) {
  df2a$standard_product[index_2[j]] <- "Error-Duplicate standard Product Name"
}

# Checks for indiamart image URL on img1 to img5
#class(df2a$`standard/variant`)
grepl(".imimg.com",df2a$img3[2])
fun_imimgURL <- function(x){
  x <- as.data.frame(x)
  index_img <- grepl(".imimg.com",x)
  for (i in 1:nrow(df2a)) {
    ifelse(index_img[i]==TRUE,x[i] <- "Error-Indiamart image URL",x[i] <- as.character(x[i]))
  }
}

index_img1 <- grepl(".imimg.com",df2a$img1)

for (i in 1:nrow(df2a)) {
  if(index_img1[i]==TRUE){
    df2a$img1[i] <-"Error-Indiamart image URL" }
}
which(index_img1==T)
df2a$img1[85]

index_img2 <- grepl(".imimg.com",df2a$img2)

for (i in 1:nrow(df2a)) {
  if(index_img2[i]==TRUE){
    df2a$img2[i] <-"Error-Indiamart image URL" }
}

index_img3 <- grepl(".imimg.com",df2a$img3)

for (i in 1:nrow(df2a)) {
  if(index_img3[i]==TRUE){
    df2a$img3[i] <-"Error-Indiamart image URL" }
}

index_img4 <- grepl(".imimg.com",df2a$img4)

for (i in 1:nrow(df2a)) {
  if(index_img4[i]==TRUE){
    df2a$img4[i] <-"Error-Indiamart image URL" }
}

index_img5 <- grepl(".imimg.com",df2a$img5)

for (i in 1:nrow(df2a)) {
  if(index_img5[i]==TRUE){
    df2a$img5[i] <-"Error-Indiamart image URL" }
}

############

#check5 <- same as sheet

#check6 <- Any word should not be repeated like samsung samsung

#?strsplit
###testing###
rem_dup.w <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = " ")
}

duptest <- "Samsung WA80E5LEC samsung Top Loading with Diamond Drum, 6 kg (Silver)"
nchar(duptest)
duptest <- gsub("\\,","",duptest)
nchar(duptest)
nchar(rem_dup.w(duptest))



###testing end###
######
wordmatch <- df2$standard_product
wordmatch <- as.data.frame(wordmatch)      
wordmatch$wordmatch <- as.character(wordmatch$wordmatch)
#wordmatch$wordmatch <- gsub("\\,","",wordmatch$wordmatch)
wordmatch$wordmatch <- as.character(wordmatch$wordmatch)
wordmatch[is.na(wordmatch$wordmatch),] <- "NA"
wordmatch$nchar <- nchar(wordmatch$wordmatch)

rem_dup.one <- function(x){
  x <- tolower(x)
  paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = " ")
}

#wordmatch$nchar2 <- nchar(trimws(rem_dup.one(wordmatch$wordmatch)))

i <- 1


for (i in 1:nrow(wordmatch)) {
  wordmatch$nchar2[i] <- nchar(trimws(rem_dup.one(wordmatch$wordmatch[i])))
  
}

j <- 1
for (j in 1:nrow(wordmatch)) {
  ifelse(wordmatch$nchar[j]!=wordmatch$nchar2[j],wordmatch$wordmatch2[j] <- paste0("Error-duplicate words"),wordmatch$wordmatch2[j] <- as.character(wordmatch$wordmatch[j]))
}

wordmatch$wordmatch2 <- trimws(wordmatch$wordmatch2)
newdf2 <- cbind(df2a,wordmatch)
newdf2$wordmatch <- NULL
#newdf2$wordmatch2 <- NULL
newdf2$standard_product <- as.character(newdf2$standard_product)
newdf2$wordmatch2 <- as.character(newdf2$wordmatch2)
k <- 1
for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$wordmatch2[k]=="Error-duplicate words",newdf2$standard_product[k] <- paste0("Error-duplicate words - "),newdf2$standard_product[k] <- as.character(newdf2$standard_product[k]))
}


##for variant names:
wordmatch$wordv <- newdf2$variant_name
wordmatch$wordv <- as.character(wordmatch$wordv)
wordmatch[is.na(wordmatch$wordv),"wordv"] <- "NA"
wordmatch$nchar3 <- nchar(wordmatch$wordv)
for (i in 1:nrow(wordmatch)) {
  wordmatch$nchar4[i] <- nchar(trimws(rem_dup.one(wordmatch$wordv[i])))
  
}

j <- 1
for (j in 1:nrow(wordmatch)) {
  ifelse(wordmatch$nchar3[j]!=wordmatch$nchar4[j],wordmatch$wordmatch4[j] <- paste0("Error-duplicate words"),wordmatch$wordmatch4[j] <- as.character(wordmatch$wordv[j]))
}

newdf2 <- newdf2[,c(1:62)]
newdf2 <- cbind(newdf2,wordmatch)
newdf2$variant_name <- as.character(newdf2$variant_name)
for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$wordmatch4[k]=="Error-duplicate words",newdf2$variant_name[k] <- paste0("Error-duplicate words - "),newdf2$variant_name[k] <- as.character(newdf2$variant_name[k]))
}

#check 7 Hindi standard product- english character not allowed


#grepl("[A-z]","1Y")

newdf2$hindi_devnagri <- as.character(newdf2$hindi_devnagri)
#newdf2$hindi_devnagri[4] <- "11452225हिंदी"
d <- grepl("[A-z]",newdf2$hindi_devnagri)
d <- as.character(d)
#d[5] <- "14456221247"
d <- as.data.frame(d)

newdf2 <- cbind(newdf2,d)
newdf2$d <- as.character(newdf2$d)
class(newdf2$hindi_devnagri)

for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$d[k]=="TRUE",newdf2$hindi_devnagri[k] <- paste0("Error-english character -",newdf2$hindi_devnagri[k]),newdf2$hindi_devnagri[k] <- as.character(newdf2$hindi_devnagri[k]))
  
}

#check 8 prime flag-- Should be only Numeric with no alphabet or space, cannot be blank, It has to be 0 or 1


newdf2$prime_flag <- as.character(newdf2$prime_flag)
class(newdf2$prime_flag)
for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$prime_flag[k]=="1"||newdf2$prime_flag[k]=="0",newdf2$prime_flag[k] <- as.character(newdf2$prime_flag[k]),newdf2$prime_flag[k] <- "Error-Prime flag should be 0 or 1")
  
}


#check 9 Variant priority -- Has to be numeric, If standard then it will be blank. If it is variant it can't be null

e <- grepl("\\D",newdf2$variant_priority)
e <- as.character(e)
e <- as.data.frame(e)

newdf2 <- cbind(newdf2,e)
newdf2$e <- as.character(newdf2$e)
newdf2$variant_priority <- as.character(newdf2$variant_priority)

for (k in 1:nrow(newdf2)) {
  ifelse(newdf2[k,"e"]!="FALSE",newdf2$variant_priority <- "Error-Priority wrongly updated",newdf2$variant_priority[k] <-newdf2$variant_priority[k])  
  
}

#check 10 Standard/Variant should be either standard or variant

newdf2$`standard/variant` <- as.character(newdf2$`standard/variant`)
newdf2$`standard/variant` <- trimws(newdf2$`standard/variant`)

for (i in 1:nrow(newdf2)) {
  ifelse(newdf2$`standard/variant`[i]=="Standard"||newdf2$`standard/variant`[i]=="Variant",newdf2$`standard/variant`[i] <- as.character(newdf2$`standard/variant`[i]),newdf2$`standard/variant`[i] <- "Error-Standard/Variant wrongly updated")
  
}



variant_name <- seq(1:1000)
variant_name <- as.data.frame(variant_name)

#for (j in 1:nrow(df3)) {
# variant_name$variant_name[j] <- paste0(df3[j,2],",",df3[j,3])
#}


#++++++ FORMAT TO BE DISCUSSED WITH ANAND

# check 11 english_devnagri -	English Characters not allowed , numeric is allowed


f <- grepl("[A-z]",newdf2$english_devnagri)
f <- as.character(f)
f <- as.data.frame(f)

newdf2 <- cbind(newdf2,f)
newdf2$f <- as.character(newdf2$f)
newdf2$english_devnagri <- as.character(newdf2$english_devnagri)

for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$f[k]=="TRUE",newdf2$english_devnagri[k] <- paste0("Error-english character -",newdf2$english_devnagri[k]),newdf2$english_devnagri[k] <- as.character(newdf2$english_devnagri[k]))
  
}

# check 12 hindi_devnagri	English Characters not allowed, numeric is allowed-- Already covered we should updated a seperate col for standproduct and varient hindi devnagri


# Check13 hindi_roman	Same as main sheet with no double space

# check 14 search_kw	Same as main sheet with no double space

# Check 15 price1_low	should only contain numeric values, Price low should be less than price high

g <- grepl("\\D",newdf2$price1_low)
g <- as.character(g)
g <- as.data.frame(g)

newdf2 <- cbind(newdf2,g)
newdf2$g <- as.character(newdf2$g)
#newdf2$price1_low <- as.character(newdf2$price1_low)
#newdf2$price1_high <- as.character(newdf2$price1_high)
newdf2$price1_low <- as.numeric(newdf2$price1_low)
newdf2$price1_high <- as.numeric(newdf2$price1_high)
#class()
class(newdf2$price1_high)
for (j in 1:nrow(newdf2)) {
  ifelse(newdf2$price1_low[j]>newdf2$price1_high[j],newdf2$price1_low[j] <- "Error-Price_Low > High",newdf2$price1_low[j] <- newdf2$price1_low[j])
}

#as.numeric(as.character(newdf2$price1_high))
#newdf2$price1_low <- as.character(newdf2$price1_low)

for (i in 1:nrow(newdf2)) {
  ifelse(newdf2$g[i]=="TRUE",newdf2$price1_low[i] <- "Error-Price_Low > High",newdf2$price1_low[i] <- as.character(newdf2$price1_low[i]))
  
}

class(newdf2$price1_low)
newdf2$price1_low <- as.numeric(newdf2$price1_low)
#check 16 price1_high should only contain numeric values, Price low should not be less than price low(already checked)
#newdf2$price1_high <- as.character(newdf2$price1_high)
#newdf2$price1_low <- as.character(newdf2$price1_low)
h <- grepl("\\D",newdf2$price1_high)
h <- as.character(h)
h <- as.data.frame(h)

newdf2 <- cbind(newdf2,h)

newdf2$h <- as.character(newdf2$h)

newdf2$price1_high <- as.character(newdf2$price1_high)

for (k in 1:nrow(newdf2)) {
  ifelse(newdf2$h[k]=="TRUE",newdf2$price1_high[k] <- "Error-Price should be numeric",newdf2$price1_high[k] <- as.character(newdf2$price1_high[k]))
  
}

# check 17 unit1	Same as main sheet with no double space

# Check 18 price_reference_link	should not contain any space
# check 19 img1	should not contain any space
#check 20 img2	should not contain any space
#check 21 img3	should not contain any space
#check 22 img4	should not contain any space
#check 23 img5	should not contain any space
# check 24 pdf1	should not contain any space
# Check 25 pdf2	should not contain any space

# check 26 video	should not contain any space

m <- grepl("\\s",newdf2$img5)
m <- as.character(m)
m <- as.data.frame(m)

newdf2 <- cbind(newdf2,m)

newdf2$m <- as.character(newdf2$m)
newdf2$img5 <- df2a$img5
newdf2$img5 <- as.character(newdf2$img5)

for (i in 1:nrow(newdf2)) {
  ifelse(newdf2$m[i]=="TRUE",newdf2$img5[i] <- "Error- url contains space",newdf2$img5[i] <- as.character(newdf2$img5[i]))
}

#check 29 to 34 Feature1 to Feature6 -- Should be less than or equal to 100 chars
newdf2$feature1 <- as.character(newdf2$feature1)
j <- 1
for (j in 1:nrow(newdf2)) {
  ifelse(nchar(newdf2$feature1[j])>100,newdf2$feature1[j] <- "Error-Exceed character length",newdf2$feature1[j] <- as.character(newdf2$feature1[j]))
  
}

#creating an UDF for rest of the columns where we need to check char length..

funcharlen <- function(x){
  ifelse(nchar(x)>100,x <- "Error-Exceed character length",x <- as.character(x))
}


#check <- data.frame(check=numeric())
#check$check <- as.character(check$check)

#check$check[1] <- paste("ajdnjfdvcbncsad sadjbsdbncnc xc cxghjfhfbnvcnz ajdhjfbnzf asghfhggggggnm azdcbhzcbnzzcbncbnz azfgygahf zndghfnbfgnh") 
#check$check <- lapply(check$check,funcharlen)

newdf2$feature1 <- as.character(newdf2$feature1)
newdf2$feature2 <- as.character(newdf2$feature2)
newdf2$feature3 <- as.character(newdf2$feature3)
newdf2$feature4 <- as.character(newdf2$feature4)
newdf2$feature5 <- as.character(newdf2$feature5)
newdf2$feature6 <- as.character(newdf2$feature6)

newdf2$feature1 <- lapply(newdf2$feature1,funcharlen)
newdf2$feature2 <- lapply(newdf2$feature2,funcharlen)
newdf2$feature3 <- lapply(newdf2$feature3,funcharlen)
newdf2$feature4 <- lapply(newdf2$feature4,funcharlen)
newdf2$feature5 <- lapply(newdf2$feature5,funcharlen)
newdf2$feature6 <- lapply(newdf2$feature6,funcharlen)

#check <- newdf2$feature2
#check <- as.data.frame(check)
#check$check <- as.character(check$check)
#check <- sapply(check, funcharlen)
#check$check[9] <- funcharlen(check$check[9])

#class(check$check[9])

#check 35 to 40 Application1 to Application6 -- Should be less than or equal to 100 chars

newdf2$application1 <- as.character(newdf2$application1)
newdf2$application2 <- as.character(newdf2$application2)
newdf2$application3 <- as.character(newdf2$application3)
newdf2$application4 <- as.character(newdf2$application4)
newdf2$application5 <- as.character(newdf2$application5)

newdf2$application1 <- lapply(newdf2$application1,funcharlen)
newdf2$application2 <- lapply(newdf2$application2,funcharlen)
newdf2$application3 <- lapply(newdf2$application3,funcharlen)
newdf2$application4 <- lapply(newdf2$application4,funcharlen)
newdf2$application5 <- lapply(newdf2$application5,funcharlen)


#check 45 Decapitalize text
#creating a user defined function for decapitalizing string
fundecap <- function(x){
  x <- tolower(x)
  x <- tocamel(x,sep = " ",upper = T)
}

#check$check[1] <- "dffh sjdhjf - ajgdf"

#check$check <- lapply(check$check,fundecap)

is.character(colnames((newdf2)))

charcter_vars <- names(newdf2)[sapply(newdf2, is.character)]

#char_df <- newdf2[,charcter_vars]

char_df <- newdf2[,c("pmcat_name","related_mcat1_name","related_mcat2_name","feature1","feature2","feature3","feature4","feature5","feature6","application1","application2","application3","application4","application5")]
char_df <- lapply(char_df,fundecap)




#Applied this function on final sheet

newdf2[,c("pmcat_name","related_mcat1_name","related_mcat2_name","feature1","feature2","feature3","feature4","feature5","feature6","application1","application2","application3","application4","application5")] <- lapply(newdf2[,c("pmcat_name","related_mcat1_name","related_mcat2_name","feature1","feature2","feature3","feature4","feature5","feature6","application1","application2","application3","application4","application5")],fundecap)

newdf3 <- newdf2[,-c(65:76)]

newdf4 <- newdf3[,c(1:46)]

newdf4<-newdf4[,c(1:46)]
#function to replace commas at start and end
fUNCOMMAEND <- function(x){
  x <- as.character(x)
  x <- gsub("^,|,*$", "", x, perl=T)
}

newdf4$standard_product <- lapply(newdf4$standard_product,fUNCOMMAEND)
newdf4[,c(1:46)] <- lapply(newdf4[,c(1:46)],fUNCOMMAEND)

#error if double comma(,,)
#strr <- ",My,dfgfj , fbvhsgd, cvnfkhh fdvid,"

w <- grepl(",,",newdf4$standard_product)
w <- as.data.frame(w)
newdf4 <- cbind(newdf4,w)


#function to check double comma

newdf4$standard_product <- as.character(newdf4$standard_product)
newdf4$w <- as.character(newdf4$w)
for (i in 1:nrow(newdf4)) {
  
  ifelse(newdf4$w[i]=="TRUE",newdf4$standard_product[i] <- "Error- Remove Double comma",newdf4$standard_product[i] <- as.character(newdf4$standard_product[i]))
  
}

newdf4$variant_name <- as.character(newdf4$variant_name)

for (i in 1:nrow(newdf4)) {
  
  ifelse(newdf4$w[i]=="TRUE",newdf4$variant_name[i] <- "Error- Remove Double comma",newdf4$variant_name[i] <- as.character(newdf4$variant_name[i]))
  
}


newdf4 <- newdf4[,1:46]

funcharlen80 <- function(x){
  ifelse(nchar(x)>80,x <- "Error-Exceed character length",x <- as.character(x))
}

newdf4$standard_product <- as.character(newdf4$standard_product)
newdf4$variant_name <- as.character(newdf4$variant_name)

newdf4$standard_product <- lapply(newdf4$standard_product,funcharlen80)
newdf4$variant_name <- lapply(newdf4$variant_name,funcharlen80)

#check for standard and variant names: a. if standard then img1 link cant be blank

newdf4$`standard/variant` <- as.character(newdf4$`standard/variant`)
newdf4$img1 <- as.character(newdf4$img1)
for (j in 1:nrow(newdf4)) {
  ifelse((newdf4$`standard/variant`[j]=="Standard" && newdf4$img1[j]==""),newdf4$img1[j] <- "Error-no img link for standard",newdf4$img1[j] <- as.character(newdf4$img1[j]))
  
}

# if variant then variant name can't be blank
newdf4$`standard/variant` <- as.character(newdf4$`standard/variant`)
newdf4$variant_name <- as.character(newdf4$variant_name)
#newdf4$variant_name <- gsub("NA","",newdf4$variant_name)

for (j in 1:nrow(newdf4)) {
  ifelse((newdf4$`standard/variant`[j]=="Variant" && newdf4$variant_name[j]==""),newdf4$variant_name[j] <- "Error-Variant name cant be blank",newdf4$variant_name[j] <- as.character(newdf4$variant_name[j]))
  
}
#standard product name cant be blank

newdf4$standard_product <- as.character(newdf4$standard_product)
#newdf4$standard_product <- gsub("NA","",newdf4$standard_product)
for (k in 1:nrow(newdf4)) {
  
  ifelse(newdf4$standard_product[k]=="NA",newdf4$standard_product[k] <- "Error- Standard product name cant be blank",newdf4$standard_product[k] <- as.character(newdf4$standard_product[k]))
  
}

newdf4$variant_priority <- as.character(newdf4$variant_priority)
newdf4$variant_priority <- gsub("NA","",newdf4$variant_priority)
for (k in 1:nrow(newdf4)) {
  
  ifelse(newdf4$variant_priority[k]=="",newdf4$variant_priority[k] <- "Error- variant priority name cant be blank",newdf4$variant_priority[k] <- as.character(newdf4$variant_priority[k]))
  
}

newdf4$`standard/variant` <- as.character(newdf4$`standard/variant`)
newdf4$`standard/variant` <- gsub("NA","",newdf4$`standard/variant`)

for (k in 1:nrow(newdf4)) {
  
  ifelse(newdf4$`standard/variant`[k]=="",newdf4$`standard/variant`[k] <- "Error- Mention standard or variant",newdf4$`standard/variant`[k] <- as.character(newdf4$`standard/variant`[k]))
  
}


newdf4$variant_name <- as.character(newdf4$variant_name)
#newdf4$variant_name <- gsub("NA","",newdf4$variant_name)



newdf4$reference <- as.character(newdf4$reference)
newdf4$reference <- gsub("NA","",newdf4$reference)
newdf4$reference[is.na(newdf4$reference)] <- ""
for (k in 1:nrow(newdf4)) {
  
  ifelse(newdf4$reference[k]=="",newdf4$reference[k] <- "Error- reference link cant be blank",newdf4$reference[k] <- as.character(newdf4$reference[k]))
  
}


newdf4$prime_flag <- as.character(newdf4$prime_flag)
newdf4[is.na(newdf4$prime_flag),c('prime_flag')] <- "NA"
for (i in 1:nrow(newdf4)) {
  ifelse(newdf4$prime_flag[i]=="NA",newdf4$prime_flag[i] <- "Error-Prime flag can't be blank",newdf4$prime_flag[i] <- as.character(newdf4$prime_flag[i]))
}


for (j in 1:nrow(newdf4)) {
  ifelse(newdf4$`standard/variant`[j]=="Standard" && newdf4$variant_priority[j]!="0",newdf4$variant_priority[j] <- "Error-For Standard variant priority should be 0",newdf4$variant_priority[j] <- as.character(newdf4$variant_priority[j]))
}
class(newdf4$variant_priority)
newdf4[is.na(newdf4$variant_priority),c('variant_priority')] <- "NA"
for (i in 1:nrow(newdf4)) {
  ifelse(newdf4$variant_priority[i]=="NA",newdf4$variant_priority[i] <- "Error-Variant priority can't be blank",newdf4$variant_priority[i] <- as.character(newdf4$variant_priority[i]))
}



#nchar("LG T72CMG22P Cool Grey/Marine Blue, Punch+3 Pulsator, Turbodrum,6.2kg Marine Blue")
#Now we need to add 7 blanks rows and combined colums of another data
Cols<-colnames(newdf4)
Cols1<-Cols
Cols1[Cols1!='']<-NA
newdf5<-rbind(Cols,newdf4)
newdf6<-rbind(Cols1,newdf5)
for(i in 1:5)
{
  newdf6<-rbind(Cols1,newdf6)
}

final<-cbind(newdf6,df3)
getwd()

#checks for #NA and #REF!


final_check <- final

#fun_ref <- function(x){
  #for (i in 1:nrow(final_check)) {
    #ifelse((x[,i]=="#NA"||x[,i]=="#REF!"),x[,i] <- "Error- #NA in data ",x[,i] <- as.character(x[,i]))
  #}
#} 


#final_check$reference[1] <- "#NA"
#final_check$reference[2] <- "#REF!"
#final_check <- sapply(final_check,fun_ref)

for (i in 1:nrow(final_check)) {
  ifelse(final_check$reference[i]=="#NA"||final_check$reference[i]=="#REF!",final_check$reference[i] <- "Error- Please check this cell",final_check$reference[i] <- as.character(final_check$reference[i]))
  
}

for (i in 1:nrow(final_check)) {
  ifelse(final_check$standard_product[i]=="#NA"||final_check$standard_product[i]=="#REF!",final_check$standard_product[i] <- "Error- Please check this cell",final_check$standard_product[i] <- as.character(final_check$standard_product[i]))
  
}

for (i in 1:nrow(final_check)) {
  ifelse(final_check$variant_name[i]=="#NA"||final_check$variant_name[i]=="#REF!",final_check$variant_name[i] <- "Error- Please check this cell",final_check$variant_name[i] <- as.character(final_check$variant_name[i]))
  
}

for (i in 1:nrow(final_check)) {
  ifelse(final_check$search_kw[i]=="#NA"||final_check$search_kw[i]=="#REF!",final_check$search_kw[i] <- "Error- Please check this cell",final_check$search_kw[i] <- as.character(final_check$search_kw[i]))
  
}



#removing NA values from final sheet


#data <- data.frame(lapply(final_check, function(x) { gsub("Na", "", x)}))
data <- data.frame(final_check)
#data[,c("feature1","feature2","feature3","feature4","feature5","feature6","application1","application2","application3","application4","application5")] <- data.frame(lapply(final_check, function(x) { gsub("Na", "", x)}))
fun_nn <- function(x){
  gsub("Na","",x)
}
data$feature1 <- lapply(data$feature1,fun_nn)
data$feature2 <- lapply(data$feature2,fun_nn)
data$feature3 <- lapply(data$feature3,fun_nn)
data$feature4 <- lapply(data$feature4,fun_nn)
data$feature5 <- lapply(data$feature5,fun_nn)
data$feature6 <- lapply(data$feature6,fun_nn)
data$application1 <- lapply(data$application1,fun_nn)
data$application2 <- lapply(data$application2,fun_nn)
data$application3 <- lapply(data$application3,fun_nn)
data$application4 <- lapply(data$application4,fun_nn)
data$application5 <- lapply(data$application5,fun_nn)

data_chk <- data
data_chk[,49:55] <- lapply(data_chk[,49:55],as.character)
class(data_chk[,49])
class(data_chk$Configurable.1)

data_chk[is.na(data_chk[,49]),49] <- "NA"
data_chk[is.na(data_chk[,50]),50] <- "NA"
data_chk[is.na(data_chk[,51]),51] <- "NA"
data_chk[is.na(data_chk[,52]),52] <- "NA"
data_chk[is.na(data_chk[,53]),53] <- "NA"
data_chk[is.na(data_chk[,54]),54] <- "NA"
data_chk[is.na(data_chk[,55]),55] <- "NA"

#data_chk[,49:55] <- gsub("^$|^ $", NA, data_chk[,49:55])
#data_chk[is.na(data_chk$Configurable),c("Configurable")] <- "NA"
#data_chk[is.na(data_chk$Configurable.1),c("Configurable.1")] <- "NA"
#if(names(data_chk)=="Configurable.1"){
   #data_chk[is.na(data_chk$Configurable.1),c("Configurable.1")] <- "NA"
   #}
#data_chk[is.na(data_chk$Configurable.2),c("Configurable.2")] <- "NA"
#data_chk[is.na(data_chk$Key),c("Key")] <- "NA"
#data_chk[is.na(data_chk$Key.1),c("Key.1")] <- "NA"
#data_chk[is.na(data_chk$Key.2),c("Key.2")] <- "NA"
#data_chk[is.na(data_chk$Key.3),c("Key.3")] <- "NA"
#data_chk[is.na(data_chk$Key.4),c("Key.4")] <- "NA"
#data_chk[is.na(data_chk$Configurable.2),c("Configurable.2")] <- "NA"
z <- 8
for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,49]=="NA",data_chk[z,49] <- "Warning- Key/Config can't be blank",data_chk[z,49] <- as.character(data_chk[z,49]))
  
}
for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,50]=="NA",data_chk[z,50] <- "Warning- Key/Config can't be blank",data_chk[z,50] <- as.character(data_chk[z,50]))
  
}

for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,51]=="NA",data_chk[z,51] <- "Warning- Key/Config can't be blank",data_chk[z,51] <- as.character(data_chk[z,51]))
  
}
for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,52]=="NA",data_chk[z,52] <- "Warning- Key/Config can't be blank",data_chk[z,52] <- as.character(data_chk[z,52]))
  
}

for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,53]=="NA",data_chk[z,53] <- "Warning- Key/Config can't be blank",data_chk[z,53] <- as.character(data_chk[z,53]))
  
}


for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,54]=="NA",data_chk[z,54] <- "Warning- Key/Config can't be blank",data_chk[z,54] <- as.character(data_chk[z,54]))
  
}

#for (z in 8:nrow(data_chk)) {
 # ifelse(data_chk$Key.4[z]=="NA",data_chk$Key.4[z] <- "Warning-Key ISQ is blank",data_chk$Key.4[z] <- as.character(data_chk$Key.4[z]))
  
#}

for (z in 8:nrow(data_chk)) {
  ifelse(data_chk[z,55]=="NA",data_chk[z,55] <- "Warning- Key/Config can't be blank",data_chk[z,55] <- as.character(data_chk[z,55]))
  
}


#data_chk2 <- data_chk
#y <- 49
#for (y in 49:ncol(data_chk2)) {
  #ifelse((data_chk2[1,y]=="Configurable"||"Key"||"Regular"),data_chk2[1,y] <- data_chk2[1,y],data_chk2[1,y] <- "Error-value should be Key/Configurable/Regular")
  
#}
#data_chk2[1,49:ncol(data_chk2)]
data_final <- data.frame(lapply(data_chk, function(x) { gsub("NA", "", x)}))



write.xlsx(data_final,"C:/Users/imart/Desktop/SID_R_Check/Washing_machine.xlsx",row.names = F,col.names = F,showNA = F)


#Samsung WA80E5LEC Top Loading with Diamond Drum, 6 kg (Silver)