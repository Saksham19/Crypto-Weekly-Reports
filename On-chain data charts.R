###on chain charts
##Data from glassnode and blockchain.com
##check python code for data scraping

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
library(ggthemes)
library(fBasics)
library(dygraphs)
library(htmltools)
library(readxl)

#loading in the data

#on-chain data
on_chain_all<- read_excel("Aaro Capital docs/research/weekly reports (R+python codes)/On-chain data/OI_COMBINED/ON_CHAIN_COMBINED.xlsx")

on_chain_all<-as.data.frame(on_chain_all)
rownames(on_chain_all)<-on_chain_all$DATE
on_chain_all<-drop_na(on_chain_all)
#btc-price

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
  coin_all_data<-coin_all_data[c("Open","High","Low","Close","Volume")] #<<- IS TO SET A GLOBAL VARIABLE
  coin_all_data<<-as.xts(coin_all_data)
  
}

get_coin_data("BTC")

btc_close<-as.data.frame(coin_all_data)
btc_close%>%select(4)->btc_close
colnames(btc_close)<-"BTC"
btc_close<-as.data.frame(btc_close)
btc_close$Date<-rownames(btc_close)
btc_close$Date<-anytime(btc_close$Date)
btc_close$Date<-format(as.POSIXct(btc_close$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
btc_close$Date<-anytime(btc_close$Date)
rownames(btc_close)<-btc_close$Date

btc_close_no_trend<-btc_close
rownames(btc_close_no_trend)<-btc_close$Date
btc_close_no_trend<-as.xts(btc_close_no_trend$BTC,order.by=as.POSIXct(btc_close_no_trend$Date))
colnames(btc_close_no_trend)<-"BTC"
btc_close_no_trend<-as.data.frame(btc_close_no_trend)
btc_close_no_trend$DATE<-rownames(btc_close_no_trend)
btc_close_no_trend$DATE<-anytime(btc_close_no_trend$DATE)
btc_close_no_trend$DATE<-format(as.POSIXct(btc_close_no_trend$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
btc_close_no_trend$DATE<-anytime(btc_close_no_trend$DATE)




#1. SOPR

sopr<-subset(on_chain_all,select=c("DATE","SOPR"))
sopr<-as.xts(sopr$SOPR,order.by=as.POSIXct(sopr$DATE))
colnames(sopr)<-"SOPR"
sopr<-as.data.frame(sopr)
sopr$DATE<-rownames(sopr)
sopr$DATE<-anytime(sopr$DATE)
sopr$DATE<-format(as.POSIXct(sopr$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
sopr$DATE<-anytime(sopr$DATE)


#merge with btc
merge_sopr_btc<-merge(btc_close_no_trend,sopr,by="DATE")
merge_sopr_btc$DATE<-format(as.POSIXct(merge_sopr_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_sopr_btc)<-merge_sopr_btc$DATE
merge_sopr_btc<-as.xts(merge_sopr_btc)



#7-d moving average 
merge_sopr_btc$SOPR<-SMA(merge_sopr_btc$SOPR,n=7)

#plot

sopr_graph_7d<-dygraph(tail(merge_sopr_btc,30),main="Spent Output Profit Ratio (7d Moving Average)")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="SOPR VALUE (7DMA)")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("SOPR",axis="y",label="SOPR")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

sopr_graph_7d


#signals:
#trend reversal signal
#if sopr goes above>1 without signaling sell off - bullish trend. Vice versa also true.
merge_sopr_btc<-as.data.frame(merge_sopr_btc)
merge_sopr_btc_final<-drop_na(merge_sopr_btc)
merge_sopr_btc_final$BTC<-as.numeric(merge_sopr_btc_final$BTC)
merge_sopr_btc_final$SOPR<-as.numeric(merge_sopr_btc_final$SOPR)


bullish_sopr<-c()
for (i in (nrow(merge_sopr_btc_final)-30):nrow(merge_sopr_btc_final)) {
  if (merge_sopr_btc_final$SOPR[i]>=1 &&
    merge_sopr_btc_final$BTC[i]>merge_sopr_btc_final$BTC[i-1]) {
    bullish_sopr[i]=1    
  }else{bullish_sopr[i]=0}
}

#getting the signal
for (i in (length(bullish_sopr)-7):length(bullish_sopr)) {
  if (bullish_sopr[i]>=1) {
    print(paste("No trend reversal as per bullish sopr as on ",merge_sopr_btc_final$DATE[i],sep=""))    
  }else{print(paste("Trend reversal to bearish as per bullish sopr on ",merge_sopr_btc_final$DATE[i],sep=""))}
}



#2. Stock-Flow Deflection

stfr<-subset(on_chain_all,select=c("DATE","SF_RATIO"))
stfr<-as.xts(stfr$SF_RATIO,order.by=as.POSIXct(stfr$DATE))
colnames(stfr)<-"SF_RATIO"
stfr<-as.data.frame(stfr)
stfr$DATE<-rownames(stfr)
stfr$DATE<-anytime(stfr$DATE)
stfr$DATE<-format(as.POSIXct(stfr$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
stfr$DATE<-anytime(stfr$DATE)


#merge with btc
merge_stfr_btc<-merge(btc_close_no_trend,stfr,by="DATE")
merge_stfr_btc$DATE<-format(as.POSIXct(merge_stfr_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_stfr_btc)<-merge_stfr_btc$DATE
merge_stfr_btc$SF_D<-merge_stfr_btc$BTC/merge_stfr_btc$SF_RATIO
merge_stfr_btc<-subset(merge_stfr_btc,select=c(-SF_RATIO))
merge_stfr_btc<-as.xts(merge_stfr_btc)





#plot

stfr_graph<-dygraph(tail(merge_stfr_btc,30),main="Stock to Flow Deflection")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="STFD VALUE")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("SF_D",axis="y",label="STFD")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

stfr_graph


#signal
#if stfr>1 overvalued and vice versa

bullish_stfr<-c()
for (i in (nrow(merge_stfr_btc)-30):nrow(merge_stfr_btc)) {
  if (merge_stfr_btc$SF_D[i]>=1) {
    bullish_stfr[i]<-0    
  }else{bullish_stfr[i]<-1}}

for (i in (length(bullish_stfr)-7):length(bullish_stfr)) {
  if (bullish_stfr[i]==1) {
    print(paste("BTC undervalued as per SF Deflection as on ",merge_stfr_btc$DATE[i],sep=""))    
  }else{print(paste("BTC overvalued as per SF Deflection as on ",merge_stfr_btc$DATE[i],sep=""))}
}


#3. Network Values to Transaction Signal (NVTS)

nvts<-subset(on_chain_all,select=c("DATE","NVTS"))
nvts<-as.xts(nvts$NVTS,order.by=as.POSIXct(nvts$DATE))
colnames(nvts)<-"NVTS"
nvts<-as.data.frame(nvts)
nvts$DATE<-rownames(nvts)
nvts$DATE<-anytime(nvts$DATE)
nvts$DATE<-format(as.POSIXct(nvts$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
nvts$DATE<-anytime(nvts$DATE)
nvts<-nvts[endpoints(nvts$DATE,on="months",k=1),]

btc_monthly<-btc_close_no_trend[endpoints(btc_close_no_trend$DATE,on="months",k=1),]

#merge with btc
merge_nvts_btc<-merge(btc_monthly,nvts,by="DATE")
merge_nvts_btc$DATE<-format(as.POSIXct(merge_nvts_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_nvts_btc)<-merge_nvts_btc$DATE
merge_nvts_btc<-as.xts(merge_nvts_btc)



#plot

nvts_graph<-dygraph(merge_nvts_btc,main="Net Value to Transactions Signal")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="NVTS")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("NVTS",axis="y",label="NVTS")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

nvts_graph


#4. Market Value to Realized Value


mvrv<-subset(on_chain_all,select=c("DATE","MVRV"))
mvrv<-as.xts(mvrv$MVRV,order.by=as.POSIXct(mvrv$DATE))
colnames(mvrv)<-"MVRV"
mvrv<-as.data.frame(mvrv)
mvrv$DATE<-rownames(mvrv)
mvrv$DATE<-anytime(mvrv$DATE)
mvrv$DATE<-format(as.POSIXct(mvrv$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
mvrv$DATE<-anytime(mvrv$DATE)


#merge with btc
merge_mvrv_btc<-merge(btc_close_no_trend,mvrv,by="DATE")
merge_mvrv_btc$DATE<-format(as.POSIXct(merge_mvrv_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_mvrv_btc)<-merge_mvrv_btc$DATE
merge_mvrv_btc<-as.xts(merge_mvrv_btc)


#plot

mvrv_graph<-dygraph(tail(merge_mvrv_btc,30),main="Market Value to Realized Value")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="MVRV")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("MVRV",axis="y",label="MVRV")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

mvrv_graph


#5. Exchange Trade Volume

etv<-subset(on_chain_all,select=c("DATE","ETV"))
etv<-as.xts(etv$ETV,order.by=as.POSIXct(etv$DATE))
colnames(etv)<-"ETV"
etv<-as.data.frame(etv)
etv$DATE<-rownames(etv)
etv$DATE<-anytime(etv$DATE)
etv$DATE<-format(as.POSIXct(etv$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
etv$DATE<-anytime(etv$DATE)


#merge with btc
merge_etv_btc<-merge(btc_close_no_trend,etv,by="DATE")
merge_etv_btc$DATE<-format(as.POSIXct(merge_etv_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_etv_btc)<-merge_etv_btc$DATE
merge_etv_btc<-as.xts(merge_etv_btc)


#plot

etv_graph<-dygraph(tail(merge_etv_btc,30),main="Exchange Trade Volume")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="ETV")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("ETV",axis="y",label="ETV")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)



#6. Active Addresses


atv<-subset(on_chain_all,select=c("DATE","Active Address"))
atv<-as.xts(atv$`Active Address`,order.by=as.POSIXct(atv$DATE))
colnames(atv)<-"Active Address"
atv<-as.data.frame(atv)
atv$DATE<-rownames(atv)
atv$DATE<-anytime(atv$DATE)
atv$DATE<-format(as.POSIXct(atv$DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
atv$DATE<-anytime(atv$DATE)


#merge with btc
merge_atv_btc<-merge(btc_close_no_trend,atv,by="DATE")
merge_atv_btc$DATE<-format(as.POSIXct(merge_atv_btc$DATE,format='%d-%m-%Y'),format='%Y-%m-%d')
rownames(merge_atv_btc)<-merge_atv_btc$DATE

merge_atv_btc$`Active Address`<-merge_atv_btc$`Active Address`/1e+06
merge_atv_btc<-as.xts(merge_atv_btc)


#plot

atv_graph<-dygraph(merge_atv_btc,main="Active Address(BTC)")%>%
  #add the rollperiod for smoothing
  #  dyRoller(rollPeriod = 1)%>%
  #create two independent axis
  dyAxis("y",label="Active Address(m)")%>%
  dyAxis("y2",label="USD (BTC)",independentTicks = TRUE)%>%
  #assign each time series to an axis
  dySeries("Active Address",axis="y",label="Active Address")%>%
  dySeries("BTC",axis="y2",label="BTC")%>%
  #  dyRangeSelector()%>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

atv_graph


