x <- readLines("C:/Users/Imart/Downloads/imageclassificationresults_best.txt")
x <- as.data.frame(x)

#sapply(strsplit("__label__isq material handling", "__label__isq "), "[", 2)
x$x[7]

nrow(x)

Index <- 
  df <- read.table("C:/Users/Imart/Downloads/imageclassificationresults_best.txt",sep = "\n")

find("train","i am having train of blue")
?find
class(df$V1)
df$V1<-as.character(df$V1)
df$V2<-sapply(strsplit(df$V1, "train/"), "[", 2)
df$V3<-sapply(strsplit(df$V2, "/"), "[", 1)

df$index <- seq(1:nrow(df))

v1 <- seq(1,nrow(df),by=7)
v2<-seq(2,nrow(df),by=7)
v3<-seq(3,nrow(df),by=7)

df3$V3 <- sapply((?strsplit()))

v<-c(v1,v2,v3)
v<-sort(v)
#class(df3$V1)
df3<-df[v,]
for (i in 1:nrow(df3)) {
  df3$v4[i] <- word(df3$V1[i],2,sep = "\t")
}

df3$v5 <- word(df3$V1,1,sep = "\t")
index_url <- seq(1,nrow(df3),by=3)
index_mcat1 <- seq(2,nrow(df3),by=3)
index_mcat2 <- seq(3,nrow(df3),by=3)
image_URL <- df3$V1[index_url]
mcat_1 <- df3$v4[index_mcat1]
mcat_2 <- df3$v4[index_mcat2]
prob1 <- df3$v5[index_mcat1]
prob2 <- df3$v5[index_mcat2]

final <- data.frame(cbind(image_URL,mcat_1,prob1,mcat_2,prob2))
