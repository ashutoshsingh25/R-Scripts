# Loading the required libraries/ packages that would be required for code execution
library(RJDBC)
library(xlsx)
library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(sqldf)
library(robustbase)
library(tm)
library(zoo)
# Following command will free up the heap space, so as to prevent heap error while dealing with large data
options(java.parameters = "-Xmx8000m")
options(scipen = 999)

df <- read.csv("C:/Users/Imart/Downloads/mcat_bl_aov.csv",header = T)
names(df)
df1 <- df[order(df$FK_GLCAT_MCAT_ID),]

df1 <- df1[c(1:7127265),-6]
df1 <- head(df1,n=7127265)
df1 <- df1[,c(3,1,2,4,5)]
#Count of total MCAT IDs 81479
length(unique(df1$FK_GLCAT_MCAT_ID))

# Breaking data MCAT wise

MCAT_Break <- split(df1,f=df1$FK_GLCAT_MCAT_ID)


#Creating an Empty data frame for statistical values:

Stats1 <- data.frame(MCAT_ID= numeric(),
                     Min=numeric(),
                     Max=numeric(),
                     Mean=numeric(),
                     Median=numeric(),
                     OFR_IDs_Count=numeric(),
                     Q1=numeric(),
                     Q3=numeric(),
                     IQR=numeric(),
                     MC=numeric(),
                     lower=numeric(),
                     upper=numeric(),
                     outlier_cnt=numeric(),
                     outlier_per=numeric())

i<-1
total_ofrIDs<-0



### Function to get the dataframe without desired empty column#################
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#### Function to assign outlier flag as 1 to any observation ######
my_populate<-function(x,lower,upper)
{
  if(x<lower|| x>upper)
  {
    return(0)# Zero Corresponds to outliers
  }
  else
  {
    return(1)
  }
}

##### Function to get mode of data ########
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Loop for each MCAT and calculate all required values


for (i in 1:length(MCAT_Break)) {
  #Assign MCAT
  a <- MCAT_Break[[i]]
  #assigning total BL count
  total_ofrIDs <- total_ofrIDs + length(a$MID_VAL)
  
  # Check if there any BL exist for each MCATs
  length_price<-length(MCAT_Break[[i]]$ETO_OFR_DISPLAY_ID)
  
  if(length_price)
  {
    #Store Price Value
    Price<-MCAT_Break[[i]]$MID_VAL
    # Convert prices to numeric if character any
    Price<-as.numeric(MCAT_Break[[i]]$MID_VAL)
    # Sort the price value
    Price<-sort(Price)
    # Assign 0 to price where price is NA
    Price[is.na(Price)]<-0
    #Remove zero prices
    Price<-Price[Price>0]
    # Calculate MC,Q1,Q3,IQR and lower upper cap
    MC<-mc(Price,na.rm = TRUE)
    Q1 <- quantile(Price, na.rm = TRUE)[[2]]
    Q3 <- quantile(Price, na.rm = TRUE)[[4]]
    IQR <- Q3 - Q1
    if(MC<0)
    {
      
      lower <- as.numeric(Q1 - 1.5*exp(-4*MC)*IQR)
      upper <- as.numeric(Q3 + 1.5*exp(3.5*MC)*IQR)
    }
    if(MC>=0)
    {
      lower <- as.numeric(Q1 - 1.5*exp(-3.5*MC)*IQR)
      upper <- as.numeric(Q3 + 1.5*exp(4*MC)*IQR)
    }
    # Store MCAT data in data frame named a
    a<-MCAT_Break[[i]]
    a$MID_VAL<-as.numeric(a$MID_VAL)
    # Apply completeFun as created above for column Price
    a<-completeFun(a, "MID_VAL")
    a$MID_VAL<-round(a$MID_VAL)
    # Reindexing of rows
    rownames(a) <- 1:nrow(a)
    # Apply my_populate function to assign outlier flag to observation
    a$check<-apply(a,1,function(params)my_populate(as.numeric(params[3]),lower,upper))
    # If Lower value as calculated by Adjusted Box Plot is negative, remove lower two percentile values and assign lowest value to lower value
    if(lower<0)
    {
      a$MID_VAL<-as.numeric(a$MID_VAL)
      lower_data<-min(a[a$MID_VAL>quantile(a$MID_VAL,0.02),c(3)])
      lower<-lower_data
    }
    # Check outlier count and its percentage per subcat
    count_outliers<-sum(a$check==0)
    outliers_per<-(count_outliers/nrow(a))*100
    # Appending values to empty dataframe as created above
    Stats1<-rbind(Stats1,data.frame(MCAT_ID=unique(a$FK_GLCAT_MCAT_ID),min=min(a$MID_VAL,na.rm = T),
                                    max=max(a$MID_VAL,na.rm = T),mean=mean(a$MID_VAL,na.rm = T),median=median(a$MID_VAL,na.rm = T),
                                    OFR_IDs_Count=nrow(a),Q1=Q1,Q3=Q3,IQR=IQR,MC=MC,lower=lower,upper=upper,outlier_cnt=count_outliers,outlier_per=outliers_per))
    
  }
  
}

for (i in 1:nrow(Stats1)) {
  ifelse(is.infinite(Stats1$lower[i])==TRUE,Stats1$lower[i] <- as.numeric(Stats1$mean[i]),Stats1$lower[i] <- as.numeric(Stats1$lower[i]))
}
#Stats1 <- NULL
#a <- NULL
write.csv(Stats1,"E:/Pricing_analytics/mcat_wise_AOV-price_mean2.csv",row.names = F)
