options(scipen = 999)

# Creating pmcat level testing files:

df <- readxl::read_excel("E:/Excavator-PG2/kfold/final_processed_unique_41567.xlsx",sheet = 2)

df <- df[order(df$mcat1),]

write.csv(df,"E:/Excavator-PG2/kfold/final/Processed/compiled_kfold_ordered31670.csv",row.names = F,quote = F)

out <- split(df,f = df$mcat1)
i <- 1
for (i in 1:length(out)) {
  hold <- unique(out[[i]]$mcat1)
  hold <- gsub(" ","_",hold)
  Location <- paste0("E:/Excavator-PG2/PMCAT/Testing Files/",hold,".txt")
  a <- out[[i]]$test_data
  write.table(a,Location,row.names = F,col.names = F,quote = F)
}