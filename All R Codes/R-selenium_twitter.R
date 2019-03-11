library(RSelenium)
driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]
remDr$navigate("https://twitter.com/realDonaldTrump?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor")

#login to twitter

mailid<-remDr$findElement(using = 'css',  "[class = 'text-input email-input js-signin-email']")
mailid$sendKeysToElement(list("singhashutosh124@gmail.com"))


password<-remDr$findElement(using = 'css', ".LoginForm-password .text-input")
password$sendKeysToElement(list("wweisashu"))

#After entering mail id and password, we need to click the login button,
#this can be done by identifying the login button element by css selector using selector gadget 
#as shown below and calling the clickElement method.

login <- remDr$findElement(using = 'css',".js-submit")
login$clickElement()

#Collecting twwets
t <- remDr$findElements(using = 'css', "[class = 'TweetTextSize TweetTextSize--normal js-tweet-text tweet-text']")
tweets=list()
tweets=sapply(t, function(x){x$getElementText()})
for(i in 1:6){
  print(tweets[i])
}


#Stop Server

#close method is used to close the client.

#stop method is used to stop the server.

#process method is used to check the current status of server, let us use it after calling stop method to ensure that the connection is terminated.


remDr$close()
driver$server$stop()
driver$server$process
