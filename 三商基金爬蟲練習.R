# Reference ---------------------------------------------------------------


# https://github.com/SuYenTing 
# Lecturer : SuYenTing 
# Data Base : http://vul.mli.com.tw/main.asp?sUrl=$W$HTML$MLISELECT2]HTM (三商美邦人壽)



# Packages Needed ---------------------------------------------------------

library(tidyverse)
library(rvest)
library(jsonlite)
library(RCurl)
library(XML)
library(dplyr)



# Download the Fund List  -------------------------------------------------


url = "http://vul.mli.com.tw/W/djjson/MLISelectJSJSON.djjson?a=P"
fundList = read_html(url, encoding = "big5") %>%
  html_text() %>%
  fromJSON()

fundList = fundList$ResultSet$Menu %>% as.data.frame()
View(fundList)




# Select the Specific Funds from List and Download ------------------------

Funds = "JVUL"   

url = paste0("http://vul.mli.com.tw/w/djjson/MLISearchJSON.djjson?P=", Funds )
myFunds = read_html(url, encoding = "Big5") %>%
  html_text() %>%
  fromJSON()

myFunds = myFunds$ResultSet$Result %>% as.data.frame()
View(myFunds)



# Find the Codes ----------------------------------------------------------
# A(Domestic):ISIN Code // B(Overseas):ISIN Code // CED : Not Found


PolicyCode = myFunds[,c(40,3,42)]   # Internal Code / Fund Name / Type 
colnames(PolicyCode) = c("IntCode","Name","Type") 
PolicyCode$ISIN_Code= NA
View(PolicyCode)

typeA = function(fundIntCode){
  fundIntCode = fundIntCode %>% tolower()
  url = paste0("http://vul.mli.com.tw/w/wr/wr01.djhtm?a=", fundIntCode)
  html = read_html(url, encoding = "Big-5") %>% html_text()

  whereISIN = regexpr("\\ISIN Code", html)
  start = whereISIN + attr(matchSite, "match.length")
  ISIN_Code = substring(html, start, start+11)   
  return(ISIN_Code)
  
}

typeB = function(fundIntCode){
  url = paste0("http://vul.mli.com.tw/w/wb/wb01.djhtm?a=", fundIntCode)
  html = read_html(url, encoding = "Big-5") %>% html_text()

  whereISIN = regexpr("\\ISIN Code", html)
  start = whereISIN + attr(matchSite, "match.length")
  ISIN_Code = substring(html, start, start+11)   
  return(ISIN_Code)
  
}

typeCDE = function(fundIntCode){
  return(NA)
  
}


# Final output ------------------------------------------------------------

for ( i in 1:dim(PolicyCode)[1] ){
  Code = PolicyCode$IntCode[i]
  Type = PolicyCode$Type[i]
  
  ISIN_Code = ifelse(Type=="A", typeA(Code),
            ifelse(Type=="B", typeB(Code), typeCDE(Code)))
  
  PolicyCode$ISIN_Code[i] = ISIN_Code
}
View(PolicyCode)
