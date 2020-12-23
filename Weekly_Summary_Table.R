################################GENERAL TABLE#################################



library(jsonlite)
library(ggplot2)
library(fpp2)
library(tseries)
library(tidyr)
library(tidyverse)
library(anytime)
library(quantmod)
library(techchart)
library(Rfast)
library(corrr)
library(cowplot)
library(corrplot)
library(tidyquant)
library(dplyr)
library(lubridate)
library(fpp2)
library(TTR)
library(rvest)
library(dplyr)
library(fredr)
library(Quandl)
library(padr)
library(httr)
library(gridExtra)





#Getting the data and getting the returns column

#BTC

rm(list=ls())
get_coin_data<- function(coin_name) {
  
  
  #getting the data
  coin_daily=fromJSON(paste("https://min-api.cryptocompare.com/data/v2/histoday?fsym=",coin_name,"&tsym=USD&limit=365",sep=""))
  coin_daily_time<-coin_daily$Data$Data$time
  coin_daily_time<-anytime(coin_daily_time)
  coin_daily_close<-coin_daily$Data$Data$close
  coin_daily_open<-coin_daily$Data$Data$open
  coin_daily_high<-coin_daily$Data$Data$high
  coin_daily_low<-coin_daily$Data$Data$low
  coin_daily_volume<-(coin_daily$Data$Data$volumeto)
  
  #constructing the dataframe
  coin_all_data<-data.frame(coin_daily_time,coin_daily_open,coin_daily_high,coin_daily_low,coin_daily_close,coin_daily_volume)
  coin_all_data%>%rename(Date=coin_daily_time,Open=coin_daily_open,High=coin_daily_high,Low=coin_daily_low,Close=coin_daily_close,Volume=coin_daily_volume)->coin_all_data
  rownames(coin_all_data)<-coin_all_data$Date
  coin_all_data<<-coin_all_data[c("Open","High","Low","Close","Volume")] #<<- IS TO SET A GLOBAL VARIABLE
  coin_all_data_close<-coin_all_data$Close

}

get_coin_data("BTC")

btc_close<-as.data.frame(coin_all_data)
btc_close%>%select(4)->btc_close

#getting monthly returns 
btc_close_monthly<-monthlyReturn(btc_close)

#fixing the date format
btc_close_monthly<-as.data.frame(btc_close_monthly)
btc_close_monthly$Date<-rownames(btc_close_monthly)
btc_close_monthly$Date<-format(as.POSIXct(btc_close_monthly$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
btc_close_monthly$Date<-anytime(btc_close_monthly$Date)
rownames(btc_close_monthly)<-btc_close_monthly$Date
colnames(btc_close_monthly)<-c("BTC","DATE")



#ETH

get_coin_data("ETH")

eth_close<-as.data.frame(coin_all_data)
eth_close%>%select(4)->eth_close

#getting monthly returns 
eth_close_monthly<-monthlyReturn(eth_close)

#fixing the date format
eth_close_monthly<-monthlyReturn(eth_close)
eth_close_monthly<-as.data.frame(eth_close_monthly)
eth_close_monthly$Date<-rownames(eth_close_monthly)
eth_close_monthly$Date<-format(as.POSIXct(eth_close_monthly$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
eth_close_monthly$Date<-anytime(eth_close_monthly$Date)
rownames(eth_close_monthly)<-eth_close_monthly$Date
colnames(eth_close_monthly)<-c("ETH","DATE")







#GOLD


Quandl.api_key("insert_api_key")
gold_data<-Quandl("CHRIS/CME_GC1",start_date=Sys.Date()-365,type="xts",collapse="daily",end_date=Sys.Date())
gold_close<-gold_data$Settle

gold_close<-as.data.frame(gold_close)
gold_close$Date<-rownames(gold_close)
gold_close$Date<-anytime(gold_close$Date)
gold_close%>%pad->gold_close
#rownames(gold_close)<-gold_close$Date
gold_close<-as.xts(gold_close$Settle,order.by=as.POSIXct(gold_close$Date))
gold_close<-na.approx(gold_close)
colnames(gold_close)<-"GOLD"
gold_close_monthly<-monthlyReturn(gold_close)
gold_close_monthly<-as.data.frame(gold_close_monthly)
gold_close_monthly$Date<-rownames(gold_close_monthly)
gold_close_monthly$Date<-anytime(gold_close_monthly$Date)
colnames(gold_close_monthly)<-c("GOLD","DATE")



#UST - 10Y

ten_year<-Quandl("FRED/DGS10",collapse="daily",type="xts",start_date=Sys.Date()-365,end_date=Sys.Date())
UST<-ten_year

ust_close<-as.data.frame(UST)
ust_close$Date<-rownames(ust_close)
ust_close$Date<-anytime(ust_close$Date)
ust_close%>%pad->ust_close
#rownames(ust_close)<-ust_close$Date
ust_close<-as.xts(ust_close$V1,order.by=as.POSIXct(ust_close$Date))
ust_close<-na.approx(ust_close)
colnames(ust_close)<-"UST"
ust_close<-as.data.frame(ust_close)
ust_close$Date<-rownames(ust_close)
ust_close<-as.xts(ust_close)
ust_close_monthly<- ust_close[ endpoints(ust_close$Date, on="months", k=1), ]
ust_close_monthly<-as.data.frame(ust_close_monthly)
ust_close_monthly$Date<-rownames(ust_close_monthly)
ust_close_monthly$Date<-anytime(ust_close_monthly$Date)
ust_close_monthly$UST<-as.numeric(ust_close_monthly$UST)
ust_close_monthly$UST<-round(ust_close_monthly$UST,2)
colnames(ust_close_monthly)<-c("UST","DATE")


#SP500

fredr_set_key("0f06e3b0a22d8ddccb15ec322acfd3f4")
sp500<-fredr(series_id = "SP500",observation_start = Sys.Date()-365,observation_end = Sys.Date())
sp500<-subset(sp500,select = -c(series_id))
#fixing missing dates

sp500%>%pad->sp500
rownames(sp500)<-sp500$date
sp500<-as.xts(sp500$value,order.by=as.POSIXct(sp500$date))
sp500<-na.approx(sp500)
colnames(sp500)<-"SP500"
sp500_close_monthly<-monthlyReturn(sp500)
sp500_close_monthly<-as.data.frame(sp500_close_monthly)
sp500_close_monthly$Date<-rownames(sp500_close_monthly)
sp500_close_monthly$Date<-anytime(sp500_close_monthly$Date)
sp500_close_monthly$Date<-format(as.POSIXct(sp500_close_monthly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
sp500_close_monthly$Date<-anytime(sp500_close_monthly$Date)
rownames(sp500_close_monthly)<-sp500_close_monthly$Date
colnames(sp500_close_monthly)<-c("SP500","DATE")

#USD

usd<-fredr(series_id = "DTWEXBGS",observation_start = Sys.Date()-365,observation_end = Sys.Date())
usd<-subset(usd,select = -c(series_id))

#fixing missing dates

usd%>%pad->usd
rownames(usd)<-usd$date
usd<-as.xts(usd$value,order.by=as.POSIXct(usd$date))
usd<-na.approx(usd)
colnames(usd)<-"USD"
usd_close_monthly<-monthlyReturn(usd)
usd_close_monthly<-as.data.frame(usd_close_monthly)
usd_close_monthly$Date<-rownames(usd_close_monthly)
usd_close_monthly$Date<-anytime(usd_close_monthly$Date)
usd_close_monthly$Date<-format(as.POSIXct(usd_close_monthly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')

usd_close_monthly$Date<-anytime(usd_close_monthly$Date)
rownames(usd_close_monthly)<-usd_close_monthly$Date
colnames(usd_close_monthly)<-c("USD","DATE")



#Corporate Bonds

corp_bond_index<-Quandl("ML/USEY",collapse="daily",type="xts",start_date=Sys.Date()-365,end_date=Sys.Date())
corp<-corp_bond_index
corp<-as.data.frame(corp)
corp$Date<-rownames(corp)
corp$Date<-anytime(corp$Date)
corp%>%pad->corp
#rownames(ust_close)<-ust_close$Date
corp<-as.xts(corp$V1,order.by=as.POSIXct(corp$Date))
corp<-na.approx(corp)
colnames(corp)<-"CORP"
corp_close_monthly<-monthlyReturn(corp)
corp_close_monthly<-as.data.frame(corp_close_monthly)
corp_close_monthly$Date<-rownames(corp_close_monthly)
corp_close_monthly$Date<-anytime(corp_close_monthly$Date)
corp_close_monthly$Date<-format(as.POSIXct(corp_close_monthly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')

corp_close_monthly$Date<-anytime(corp_close_monthly$Date)
rownames(corp_close_monthly)<-corp_close_monthly$Date

colnames(corp_close_monthly)<-c("CORP","DATE")


#Merging the data together 

#merging btc and gold first
merge_1<-merge(merge(btc_close_monthly,eth_close_monthly),gold_close_monthly)
#adding in ust and eth
merge_2<-merge(merge_1,ust_close_monthly)
#adding in sp500
merge_3<-merge(merge_2,sp500_close_monthly)
#adding in usd
merge_4<-merge(merge_3,usd_close_monthly)
#adding in corp
merge_5<-merge(merge_4,corp_close_monthly)

merged_data<-merge_5

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

merged_data$BTC<-percent(merged_data$BTC)
merged_data$ETH<-percent(merged_data$ETH)
merged_data$GOLD<-percent(merged_data$GOLD)
merged_data$SP500<-percent(merged_data$SP500)
merged_data$USD<-percent(merged_data$USD)
merged_data$CORP<-percent(merged_data$CORP)

#table 
merged_data_monthly<-merged_data
merged_data_monthly_table<-grid.table(merged_data_monthly)



#####weekly table##############


#BTC
#getting weekly returns 
btc_close_weekly<-weeklyReturn(btc_close)

#fixing the date format
btc_close_weekly<-as.data.frame(btc_close_weekly)
btc_close_weekly$Date<-rownames(btc_close_weekly)
btc_close_weekly$Date<-format(as.POSIXct(btc_close_weekly$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
btc_close_weekly$Date<-anytime(btc_close_weekly$Date)
rownames(btc_close_weekly)<-btc_close_weekly$Date
colnames(btc_close_weekly)<-c("BTC","DATE")
btc_close_weekly$BTC<-percent(btc_close_weekly$BTC)


#ETH
#getting weekly returns 
eth_close_weekly<-weeklyReturn(eth_close)

#fixing the date format
eth_close_weekly<-as.data.frame(eth_close_weekly)
eth_close_weekly$Date<-rownames(eth_close_weekly)
eth_close_weekly$Date<-format(as.POSIXct(eth_close_weekly$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
eth_close_weekly$Date<-anytime(eth_close_weekly$Date)
rownames(eth_close_weekly)<-eth_close_weekly$Date
colnames(eth_close_weekly)<-c("ETH","DATE")
eth_close_weekly$ETH<-percent(eth_close_weekly$ETH)

#GOLD
gold_close_weekly<-weeklyReturn(gold_close)
gold_close_weekly<-as.data.frame(gold_close_weekly)
gold_close_weekly$Date<-rownames(gold_close_weekly)
gold_close_weekly$Date<-anytime(gold_close_weekly$Date)
colnames(gold_close_weekly)<-c("GOLD","DATE_GOLD")
gold_close_weekly$GOLD<-percent(gold_close_weekly$GOLD)


#UST
ust_close_weekly<- ust_close[ endpoints(ust_close$Date, on="weeks", k=1), ]
ust_close_weekly<-as.data.frame(ust_close_weekly)
ust_close_weekly$Date<-rownames(ust_close_weekly)
ust_close_weekly$Date<-anytime(ust_close_weekly$Date)
ust_close_weekly$UST<-as.numeric(ust_close_weekly$UST)
ust_close_weekly$UST<-round(ust_close_weekly$UST,2)
colnames(ust_close_weekly)<-c("UST","DATE")


#SP500
sp500_close_weekly<-weeklyReturn(sp500)
sp500_close_weekly<-as.data.frame(sp500_close_weekly)
sp500_close_weekly$Date<-rownames(sp500_close_weekly)
sp500_close_weekly$Date<-anytime(sp500_close_weekly$Date)
sp500_close_weekly$Date<-format(as.POSIXct(sp500_close_weekly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
sp500_close_weekly$Date<-anytime(sp500_close_weekly$Date)
rownames(sp500_close_weekly)<-sp500_close_weekly$Date
colnames(sp500_close_weekly)<-c("SP500","DATE")
sp500_close_weekly$SP500<-percent(sp500_close_weekly$SP500)



#USD
usd_close_weekly<-weeklyReturn(usd)
usd_close_weekly<-as.data.frame(usd_close_weekly)
usd_close_weekly$Date<-rownames(usd_close_weekly)
usd_close_weekly$Date<-anytime(usd_close_weekly$Date)
usd_close_weekly$Date<-format(as.POSIXct(usd_close_weekly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')

usd_close_weekly$Date<-anytime(usd_close_weekly$Date)
rownames(usd_close_weekly)<-usd_close_weekly$Date
colnames(usd_close_weekly)<-c("USD","DATE")
usd_close_weekly$USD<-percent(usd_close_weekly$USD)




#CORP
corp_close_weekly<-weeklyReturn(corp)
corp_close_weekly<-as.data.frame(corp_close_weekly)
corp_close_weekly$Date<-rownames(corp_close_weekly)
corp_close_weekly$Date<-anytime(corp_close_weekly$Date)
corp_close_weekly$Date<-format(as.POSIXct(corp_close_weekly$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')

corp_close_weekly$Date<-anytime(corp_close_weekly$Date)
rownames(corp_close_weekly)<-corp_close_weekly$Date

colnames(corp_close_weekly)<-c("CORP","DATE")
corp_close_weekly$CORP<-percent(corp_close_weekly$CORP)

#printing the tail of all 
print(tail(btc_close_weekly,3))
print(tail(eth_close_weekly,3))
print(tail(gold_close_weekly,3))
print(tail(ust_close_weekly,2))
print(tail(sp500_close_weekly,3))
print(tail(usd_close_weekly,2))
print(tail(corp_close_weekly,2))




