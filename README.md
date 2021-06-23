# agmarknet-data-scrapping R Code :
###############################
rm(list=ls())
library(tidyverse)
library(stringr)
#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
library(xml2)
library(rvest)
library(selectr)


#####Prepapre the list of all commodities 
##extarct the commodity name and commodity code on the website
library(httr)
url2<-"https://agmarknet.gov.in/SearchCmmMkt.aspx?Tx_Commodity=17&Tx_State=0&Tx_District=0&Tx_Market=0&DateFrom=17-June-2019&DateTo=06-May-2020&Fr_Date=06-Apr-2020&To_Date=06-May-2020&Tx_Trend=2&Tx_CommodityHead=Apple&Tx_StateHead=--Select--&Tx_DistrictHead=--Select--&Tx_MarketHead=--Select--"
library(XML)
response<-GET(url2)
doc= htmlParse(response,useInternalNodes = TRUE)
root<-xmlRoot(doc)

xml_names<-xpathSApply(root,"//option",xmlValue)
xml_names
t<-xml_names[7:353]


xml_names1<-xpathSApply(root,"//option",xmlAttrs)
com1<-matrix(unlist(xml_names1), nrow=length(xml_names1), byrow=F)

t2<-com1[9:356,1]
t2<-t2[-which(t2=="selected")]

t<-as.character(t)
t2<-as.numeric(t2)
com2<-as.data.frame(cbind(t,t2))
com2$t[1]

names(com2)<-c("commodity", "commodity_code")
commodity<-gsub(" ", "+", com2$commodity)
com2$commodity<-commodity ###All the commodity with the code is stored in the dataset com2
table(com2$commodity)
library(dplyr)
com2<-as.data.frame(com2)
names(com2)


com2=com2[com2$commodity=="Potato",]

##We can choose or take all the commodities in the list 


############ For all Dates + all commodities ###############
##start date we denote with c and end date with d 
#Here Start date will continuously change using loop, end date will always remained constant
###Prepare the date data set in the reqiure date format 
dateto<-seq(as.Date("2020-03-01"), as.Date("2020-04-30"), by="days") ##Year-MM-Date format

d<-"30-April-2020" ##End date

dateto[1]
nchar(dateto[1])
day<-substring(dateto,9,10)
m<-months(as.Date(dateto)) 
m<-substring(m, 1,3)

y<-format(as.Date(dateto, format="%y-%m-%d"),"%Y")
dateto[1]

dateto<-paste0(day,"-", m, "-", y)
dateto


###extract the url of all the excel of all the commodities and all the date
##if you want to change the commodities list or timeperiod , then change the dateto and com2 
url<-NA ###URL of the page with start date c and end date d and commodities list com2
price=2 ##type 0 for only price, type=2 for both price and arriavl q


for (i in 1:length(dateto)){
  c=dateto[i]
  for (j in 1:nrow(com2)) {
    a<-com2$commodity_code[j]
    b<-as.character(com2$commodity[j])
    url2[j]<-paste0("https://agmarknet.gov.in/SearchCmmMkt.aspx?Tx_Commodity","=",a,"&Tx_State=0&Tx_District=0&Tx_Market=0&DateFrom=",c,"&DateTo=", d, "&Fr_Date=", c, "&To_Date=",d, "&Tx_Trend=",price, "&Tx_CommodityHead=", b, "&Tx_StateHead=--Select--&Tx_DistrictHead=--Select--&Tx_MarketHead=--Select--")
    url<-cbind(url, url2[j])
    
    
  }
  
}

url<-url[-1]

pp<-length(url)


###Data pulling and saving
t3<-NA ##Master Excel 

comm_name<-rep(com2$commodity, length(dateto))




for(i in 1: pp ){
  url2<-url[i]
  
  t<-read_html(url2)
  t1<-html_nodes(t, "table") %>% html_table(fill = T)
  t2<-as.data.frame(t1[1])
  if(nrow(t2)>0 & ncol(t2)>10){t2<-t2[,-c(11:12)]}
  if(nrow(t2)>0){t2$crop<-comm_name[i]}
  if(nrow(t2)>0){
    t3<-as.data.frame(rbind(t2, t3))}
  
  
}

t4<-t3 ##2020
t5<-t3 ##2019
t6<-t3 ##2018


final<-as.data.frame(rbind(t4, t5,t6))


write.csv(t4,"/Users/babu/Dropbox (Personal)/BMGF/data/survey 3_analysis/price_data_potato_2020.csv", row.names = F)
write.csv(t5,"/Users/babu/Dropbox (Personal)/BMGF/data/survey 3_analysis/price_data_potato_2019.csv", row.names = F)
write.csv(t6,"/Users/babu/Dropbox (Personal)/BMGF/data/survey 3_analysis/price_data_potato_2018.csv", row.names = F)
write.csv(final,"/Users/babu/Dropbox (Personal)/BMGF/data/survey 3_analysis/price_data_potato_181920.csv", row.names = F)



###################END#############################################
