#options(scipen = 999)

library(pdftools)
library(stringr)
library(qdapRegex)

#PATH <- "C:/Users/imart/Downloads/Unicheck/"

PATH <- choose.dir()
Files <- list.files(path = PATH,pattern = ".pdf$")

my_df <- data.frame(DOC_FILE =character(),
                    PDF_FILE = character(),
                    PLAGARISM = character())

i <- 8
for (i in 1:length(Files)) {
  LOCATION <- paste0(PATH,"\\",Files[i])
  PDF1 <- pdftools::pdf_text(pdf = LOCATION)
  PDF_CHAR <- PDF1[1]
  PDF_CHAR <- rm_white(PDF_CHAR)
  DOC_FILE <- paste0(word(rm_white(PDF_CHAR),1,sep = " "),".doc")
  PDF_FILE <- Files[i]
  PLAGARISM <- sub(".*Character replacement*(.*?) *..% of Citations *..% of References.*", "\\1", rm_white(PDF_CHAR))
  PLAGARISM <- gsub("^\n","",PLAGARISM)
  PLAGARISM <- rm_white(PLAGARISM)
  TEMP_DF <- data.frame(cbind(DOC_FILE,PDF_FILE,PLAGARISM))
  my_df <- rbind(my_df,TEMP_DF)
}


my_df1 <- my_df


write.csv(my_df1,"plagarism_data.csv",row.names = F,quote = F)
