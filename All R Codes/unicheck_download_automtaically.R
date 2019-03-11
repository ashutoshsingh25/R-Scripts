library(RSelenium)
library(stringi)
library(stringr)

driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]

remDr$navigate("https://unicheck.com/login/business")

#Insert user_name and password

mailid<-remDr$findElement(using = 'css',  "[class = '_1uOq0o']")
mailid$sendKeysToElement(list("category_kp@indiamart.com"))

password<-remDr$findElement(using = 'css',  "[type = 'password']")
password$sendKeysToElement(list("dehradun@224"))

#Login Button

SignINButton <- remDr$findElement(using = 'css',"[class='_3TPErr _2a0HXz rx4SKJ _17AUbL']")
SignINButton$clickElement()
#SignINButton$clickElement()


#Navigate to a URL


remDr$navigate("https://my.unicheck.com/?id=da2821680cd047e4a47373bc2300e3d9")



#After entering mail id and password, we need to click the login button,
#this can be done by identifying the login button element by css selector using selector gadget 
#as shown below and calling the clickElement method.

#Data_id <- File_List[i]
#FILE_ID <- as.numeric(Data_id)
#INSERT_val <- paste0('"[','title =',"'",FILE_ID,"'",']"')
#INSERT_val <- gsub("\",")

remDr$navigate("https://my.unicheck.com/?id=da2821680cd047e4a47373bc2300e3d9")

SEARCH_KEY <- remDr$findElement(using = 'css',"[class='cabinet-routes-library-Search-___index__search__icon___1jtPh']")

#SEARCH_KEY$clickElement()

#SERACH_ICON <- remDr$findElement(using = 'css',"[class='cabinet-routes-library-Search-___index__search__block-with-icons___3X6aD']")
#webElem <- remDr %>% findElement(using = 'css',"[class='cabinet-routes-library-Search-___index__search__block-with-icons___3X6aD']") %>% elementSendKeys("360004933332", key = "enter")


#SERACH_ICON$clickElement()

#SERACH_ICON$sendKeysToElement(list("360004933332"))
#remDr$getCurrentUrl
  
Button1 <- remDr$findElement(using = 'css',"[title = '360016495911']")
#Bbutton1 <- remDr$findElements('name',"360018399272")

Button1$clickElement()

#For download button

Button2 <- remDr$findElement(using = 'css',"[class = 'cabinet-core-Button-___index__button___3Yv6Z cabinet-routes-viewer-viewerButtons-___viewerButtons__button___2EKxB cabinet-core-Button-___index__button--circle___1augV']")

Button2$clickElement()

Button3 <- remDr$findElement(using = 'css',"[data-tooltip-for='viewer-button--download']")
Button3$clickElement()

#Button4 <- remDr$findElement(using = 'css',"[class='cabinet-core-Tooltip-___index__tooltip___1Lp9x cabinet-routes-viewer-viewerButtons-___viewerButtons__button-tooltip___3SstS']")
#Button4$clickElement()

#Stop Server

#close method is used to close the client.

#stop method is used to stop the server.

#process method is used to check the current status of server, let us use it after calling stop method to ensure that the connection is terminated.

remDr$close()
driver$server$stop()
driver$server$process
