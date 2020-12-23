#Correlation - final template

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


rm(list=ls())
###########################Correlation between top 10 crypto (based on vol)




#Using rvest to scrape the top 10 coins (https://www.cryptocompare.com/coins/list/all/USD/1)

col_link="https://coinmarketcap.com/"
col_page=read_html(col_link)

Coins<-col_page%>%html_nodes(".coin-item-symbol")%>%html_text()

print(Coins)
#Getting the historical cl data for the top 10 coins

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

for (i in Coins) {
  coin_data<-get_coin_data(i)
  assign(paste("coin_all_data_",i,sep=""),coin_data)
}

#Static correlations

#First- remove trend Take diff - 2 due to non-linear

for (i in Coins) {
  coin_all_data<-get(paste0("coin_all_data_",i))
  coin_data_no_trend<-diff(coin_all_data$Close,2)
  assign(paste("coin_data_no_trend_",i,sep=""),coin_data_no_trend) 

}

for (i in Coins) {
  coin_merge<-get(paste0("coin_data_no_trend_",i,sep=""))
  coin_data_merged<-coin_merge
}


##will need to change this part everytime new coins added to top 10
#can automate in future

coin_data_merged<-merge(coin_data_no_trend_ADA$Close,coin_data_no_trend_BCH$Close,coin_data_no_trend_BNB$Close,coin_data_no_trend_BTC$Close,coin_data_no_trend_DOT$Close,coin_data_no_trend_ETH$Close,coin_data_no_trend_LINK$Close,coin_data_no_trend_LTC$Close,coin_data_no_trend_USDT$Close,coin_data_no_trend_XRP$Close)
colnames(coin_data_merged)<-c("ADA","BCH","BNB","BTC","DOT","ETH","LINK","LTC","USDT","XRP")


#Setting up the network plot
corr_2<-correlate(coin_data_merged)

corr_net<-corr_2%>%
  network_plot(colours=c(palette_light()[[2]],"white",palette_light()[[4]]),legend=TRUE)+
  labs(title="Static Correlations of Top 10 Coins",subtitle="1 Year(2 LAG)")+
  theme_tq()+
  theme(legend.position = "bottom")

corr_net

#Rolling correlations

#Between BTC and ETH
coin_data_merged=as.data.frame(coin_data_merged)
coin_data_merged$DATE<-rownames(coin_data_merged)
coin_data_merged$DATE<-anytime(coin_data_merged$DATE)
rolling_corr<-coin_data_merged%>%
  tq_mutate_xy(x=BTC,
               y=ETH,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr")

rolling_corr<-drop_na(rolling_corr)
rolling_corr%>%
  ggplot(aes(x=DATE))+
  geom_point(aes(y=rolling_corr),alpha=0.5)+
  theme_tq()+
  xlab("Date")+
  ylab("Correlation")+
  ggtitle("Rolling Correlations BTC-ETH (30 days)")

#rolling corr-zoomed in - last 30 days
rolling_corr<-tail(rolling_corr,30)
rolling_corr%>%
  ggplot(aes(x=DATE))+
  geom_point(aes(y=rolling_corr),alpha=0.5)+
  theme_tq()+
  xlab("Date")+
  ylab("Correlation")+
  ggtitle("Rolling Correlations BTC-ETH(30 days) - zoomed in 1 month")


############################Corr between btc,eth, gold,ust,sp500,usd################################

btc_close<-coin_all_data_BTC$Close
colnames(btc_close)<-"BTC"
btc_close$BTC<-diff(btc_close$BTC,2)
btc_close<-as.data.frame(btc_close)
btc_close$Date<-index(coin_all_data_BTC)
btc_close$Date<-anytime(btc_close$Date)
btc_close$Date<-format(as.POSIXct(btc_close$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
btc_close$Date<-anytime(btc_close$Date)
rownames(btc_close)<-btc_close$Date

#eth 

eth_close<-coin_all_data_ETH$Close
colnames(eth_close)<-"ETH"
eth_close$ETH<-diff(eth_close$ETH,2)
eth_close<-as.data.frame(eth_close)
eth_close$Date<-index(coin_all_data_ETH)
eth_close$Date<-anytime(eth_close$Date)
eth_close$Date<-format(as.POSIXct(eth_close$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
eth_close$Date<-anytime(eth_close$Date)
rownames(eth_close)<-eth_close$Date






#getting the gold data using quandl


#gold_price<-fredr(series_id = "GOLDPMGBD228NLBM",observation_start = as.Date("1990-01-01"),

#gold data -quandl
Quandl.api_key("yvCz2e-G7Gg9rxxiTA-L")
gold_data<-Quandl("CHRIS/CME_GC1",start_date=Sys.Date()-365,type="xts",collapse="daily",end_date=Sys.Date())
gold_close<-gold_data$Settle
gold_close$Settle<-diff(gold_close$Settle,2)
#Fixing missing dates
#Via converting to a dataframe - adding the missing dates - then converting back to xts - then replacing na by linear interpolation
gold_close<-as.data.frame(gold_close)
gold_close$Date<-rownames(gold_close)
gold_close$Date<-anytime(gold_close$Date)
gold_close%>%pad->gold_close
#rownames(gold_close)<-gold_close$Date
gold_close<-as.xts(gold_close$Settle,order.by=as.POSIXct(gold_close$Date))
gold_close<-na.approx(gold_close)
colnames(gold_close)<-"GOLD"
gold_close<-as.data.frame(gold_close)
gold_close$Date<-rownames(gold_close)
gold_close$Date<-anytime(gold_close$Date)
#gold_close<-as.data.frame(gold_close)

#UST-10 YEAR - quandl
ten_year<-Quandl("FRED/DGS10",collapse="daily",type="xts",start_date=Sys.Date()-365,end_date=Sys.Date())

UST<-ten_year
UST<-diff(UST,2)
#Fixing missing dates - similar to gold

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
ust_close$Date<-anytime(ust_close$Date)
#ust_close<-as.data.frame(ust_close)

#sp500

fredr_set_key("0f06e3b0a22d8ddccb15ec322acfd3f4")
sp500<-fredr(series_id = "SP500",observation_start = Sys.Date()-365,observation_end = Sys.Date())
sp500<-subset(sp500,select = -c(series_id))
#fixing missing dates

sp500%>%pad->sp500
rownames(sp500)<-sp500$date
sp500<-as.xts(sp500$value,order.by=as.POSIXct(sp500$date))
sp500<-na.approx(sp500)
colnames(sp500)<-"SP500"
sp500$SP500<-diff(sp500$SP500,2)
sp500<-as.data.frame(sp500)
sp500$Date<-rownames(sp500)
sp500$Date<-anytime(sp500$Date)
sp500$Date<-format(as.POSIXct(sp500$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
sp500$Date<-anytime(sp500$Date)


#sp500<-as.data.frame(sp500)


#usd
usd<-fredr(series_id = "DTWEXBGS",observation_start = Sys.Date()-365,observation_end = Sys.Date())
usd<-subset(usd,select = -c(series_id))

#fixing missing dates

usd%>%pad->usd
rownames(usd)<-usd$date
usd<-as.xts(usd$value,order.by=as.POSIXct(usd$date))
usd<-na.approx(usd)
colnames(usd)<-"USD"
usd$USD<-diff(usd$USD,2)
usd<-as.data.frame(usd)
usd$Date<-rownames(usd)
usd$Date<-anytime(usd$Date)
usd$Date<-format(as.POSIXct(usd$Date,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
usd$Date<-anytime(usd$Date)

#corporate bonds -https://www.quandl.com/data/ML/USEY-US-Corporate-Bond-Index-Yield

corp_bond_index<-Quandl("ML/USEY",collapse="daily",type="xts",start_date=Sys.Date()-365,end_date=Sys.Date())
corp<-corp_bond_index
corp<-diff(corp,2)
corp<-as.data.frame(corp)
corp$Date<-rownames(corp)
corp$Date<-anytime(corp$Date)
corp%>%pad->corp
#rownames(ust_close)<-ust_close$Date
corp<-as.xts(corp$V1,order.by=as.POSIXct(corp$Date))
corp<-na.approx(corp)
colnames(corp)<-"CORP"
corp<-as.data.frame(corp)
corp$Date<-rownames(corp)
corp$Date<-anytime(corp$Date)


#Merging the data together 

#merging btc and gold first
merge_1<-merge(btc_close,gold_close)
#adding in ust
merge_2<-merge(merge_1,ust_close)
#adding in sp500
merge_3<-merge(merge_2,sp500)
#adding in usd
merge_4<-merge(merge_3,usd)
#adding in corp
merge_5<-merge(merge_4,corp)
merge_5<-merge(merge_5,eth_close)

merged_data<-merge_5


#network plot
rownames(merged_data)<-merged_data$Date
merged_data<-as.xts(merged_data)
merged_data<-subset(merged_data,select = -c(Date))

merged_data<-as.data.frame(merged_data)

merged_data$BTC = as.numeric(merged_data$BTC)
merged_data$GOLD = as.numeric(merged_data$GOLD)
merged_data$UST = as.numeric(merged_data$UST)
merged_data$SP500=as.numeric(merged_data$SP500)
merged_data$USD=as.numeric(merged_data$USD)
merged_data$CORP=as.numeric(merged_data$CORP)
merged_data$ETH<-as.numeric(merged_data$ETH)



#corr_net - 7D
corr_static_7<-correlate(tail(merged_data,7))



corr_net_static_7<-corr_static_7%>%
  network_plot(colours=c(palette_light()[[2]],"white",palette_light()[[4]]),legend=TRUE)+
  labs(title="Static Correlations- Asset Classes",subtitle="7 Day(2 LAG)")+
  theme_tq()+
  theme(legend.position = "bottom")

corr_net_static_7


#corr_net - 30D

corr_static_30<-correlate(tail(merged_data,30))



corr_static_30<-corr_static_30%>%
  network_plot(colours=c(palette_light()[[2]],"white",palette_light()[[4]]),legend=TRUE)+
  labs(title="Static Correlations- Asset Classes",subtitle="30 Day(2 LAG)")+
  theme_tq()+
  theme(legend.position = "bottom")

corr_static_30


#corr_net-60D

corr_static_60<-correlate(tail(merged_data,60))



corr_static_60<-corr_static_60%>%
  network_plot(colours=c(palette_light()[[2]],"white",palette_light()[[4]]),legend=TRUE)+
  labs(title="Static Correlations- Asset Classes",subtitle="60 Day(2 LAG)")+
  theme_tq()+
  theme(legend.position = "bottom")

corr_static_60


#Corr_net-90

corr_static_90<-correlate(tail(merged_data,90))



corr_static_90<-corr_static_90%>%
  network_plot(colours=c(palette_light()[[2]],"white",palette_light()[[4]]),legend=TRUE)+
  labs(title="Static Correlations- Asset Classes",subtitle="90 Day(2 LAG)")+
  theme_tq()+
  theme(legend.position = "bottom")

corr_static_90

###############Rolling correlations########################



#between BTC and and other assets


merged_data=as.data.frame(merged_data)
merged_data$DATE<-rownames(merged_data)
merged_data$DATE<-anytime(merged_data$DATE)

par(mfrow=c(2,3))



#30D

rolling_corr_btc_eth<-merged_data%>%
  tq_mutate_xy(x=BTC,
               y=ETH,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr_gold")

plot(rolling_corr_btc_eth$DATE,rolling_corr_btc_eth$rolling_corr_gold,xlab = "DATE",ylab="CORRELATION",main = "BTC-ETH 30D",type="line",col="pink")


rolling_corr_btc_gold<-merged_data%>%
tq_mutate_xy(x=BTC,
  y=GOLD,
  mutate_fun=runCor,
  n=30,
  use="pairwise.complete.obs",
  col_rename="rolling_corr_gold")

plot(rolling_corr_btc_gold$DATE,rolling_corr_btc_gold$rolling_corr_gold,xlab = "DATE",ylab="CORRELATION",main = "BTC-GOLD 30D",type="line",col="yellow")



rolling_corr_btc_ust<-merged_data%>%
  tq_mutate_xy(x=BTC,
               y=UST,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr_ust")

plot(rolling_corr_btc_ust$DATE,rolling_corr_btc_ust$rolling_corr_ust,xlab = "DATE",ylab="CORRELATION",main = "BTC-UST 30D",type="line",col="black")


rolling_corr_btc_sp500<-merged_data%>%
  tq_mutate_xy(x=BTC,
               y=SP500,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr_sp500")

plot(rolling_corr_btc_sp500$DATE,rolling_corr_btc_sp500$rolling_corr_sp500,xlab = "DATE",ylab="CORRELATION",main = "BTC-SP500 30D",type="line",col="green")

rolling_corr_btc_usd<-merged_data%>%
  tq_mutate_xy(x=BTC,
               y=USD,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr_usd")

plot(rolling_corr_btc_usd$DATE,rolling_corr_btc_usd$rolling_corr_usd,xlab = "DATE",ylab="CORRELATION",main = "BTC-USD 30D",type="line",col="red")

rolling_corr_btc_corp<-merged_data%>%
  tq_mutate_xy(x=BTC,
               y=CORP,
               mutate_fun=runCor,
               n=30,
               use="pairwise.complete.obs",
               col_rename="rolling_corr_corp")

plot(rolling_corr_btc_corp$DATE,rolling_corr_btc_corp$rolling_corr_corp,xlab = "DATE",ylab="CORRELATION",main = "BTC-CORP 30D",type="line",col="blue")

mtext("30D ROLLING CORREALTIONS", side = 3, line = -1.3, outer = TRUE)
#might include other time horizons?
#also shorten the code



