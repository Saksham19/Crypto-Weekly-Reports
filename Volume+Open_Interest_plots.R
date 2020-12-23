#########VOLUME+OI###################

####DATA SCRAPED FROM CRYPTOCOMPARE (VOLUME) AND BYBT(OI) - CHECK PYTHON CODE 

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
library(readxl)
library( taRifx )
library(gridExtra)
rm(list=ls())

#loading in the data

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
colnames(btc_close)<-c("BTC","DATE")


Volumes_daily <- read_excel("Aaro Capital docs/research/weekly reports (R+python codes)/Volume_Data/Volumes_Combined/Volumes_Open_Interest_Combined.xlsx",sheet = "VOLUME_DAILY")
#Volumes_monthly <- read_excel("Aaro Capital docs/research/weekly reports (R+python codes)/Volume_Data/Volumes_Combined/Volumes_Open_Interest_Combined.xlsx",sheet = "VOLUME_MONTHLY")
OI_daily <- read_excel("Aaro Capital docs/research/weekly reports (R+python codes)/Volume_Data/Volumes_Combined/Volumes_Open_Interest_Combined.xlsx",sheet = "OPEN_INTEREST_DAILY")
#OI_monthly <- read_excel("Aaro Capital docs/research/weekly reports (R+python codes)/Volume_Data/Volumes_Combined/Volumes_Open_Interest_Combined.xlsx",sheet = "OPEN_INTEREST_MONTHLY")



#weekly volumes + OI


Volumes_daily<-as.data.frame(Volumes_daily)
Volumes_daily<-drop_na(Volumes_daily)
Volumes_daily$DATE<-anytime(Volumes_daily$DATE)
rownames(Volumes_daily)<-Volumes_daily$DATE

OI_daily<-as.data.frame(OI_daily)
OI_daily<-drop_na(OI_daily)  
OI_daily$DATE<-anytime(OI_daily$DATE)
rownames(OI_daily)<-OI_daily$DATE


#Binance 

#Daily vol + oi

Volumes_daily_binance<-subset(Volumes_daily,select=c("DATE","Binance Volume"))
OI_daily_binance<-subset(OI_daily,select=c("DATE","BINANCE OI"))
Volumes_daily_binance$DATE<-rownames(Volumes_daily_binance)
OI_daily_binance$DATE<-rownames(OI_daily_binance)

#merging the two datasets
#binance_weekly<-merge(Volumes_daily_binance,OI_daily_binance,by="DATE")

#OKEX

Volumes_daily_okex<-subset(Volumes_daily,select=c("DATE","okex Volume"))
OI_daily_okex<-subset(OI_daily,select=c("DATE","OKEX OI"))
Volumes_daily_okex$DATE<-rownames(Volumes_daily_okex)
OI_daily_okex$DATE<-rownames(OI_daily_okex)

#Coinbase
#NO OI FOR NOW
Volumes_daily_coinbase<-subset(Volumes_daily,select=c("DATE","Coinbase Volume"))
Volumes_daily_coinbase$DATE<-rownames(Volumes_daily_coinbase)


#BitFinex
#NO OI FOR NOW

Volumes_daily_bitfinex<-subset(Volumes_daily,select=c("DATE","Bitfinex Volume"))
Volumes_daily_bitfinex$DATE<-rownames(Volumes_daily_bitfinex)


#Huobi

Volumes_daily_huobi<-subset(Volumes_daily,select=c("DATE","Huobi Volume"))
OI_daily_huobi<-subset(OI_daily,select=c("DATE","HOUBI OI"))
colnames(OI_daily_huobi)<-c("DATE","HUOBI OI")
Volumes_daily_huobi$DATE<-rownames(Volumes_daily_huobi)
OI_daily_huobi$DATE<-rownames(OI_daily_huobi)

#Liquid
#NO OI DATA FOR NOW
Volumes_daily_liquid<-subset(Volumes_daily,select=c("DATE","Liquid Volume"))
Volumes_daily_liquid$DATE<-rownames(Volumes_daily_liquid)

#Gemini 
#NO OI DATA FOR NOW

Volumes_daily_gemini<-subset(Volumes_daily,select=c("DATE","Gemini Volume"))
Volumes_daily_gemini$DATE<-rownames(Volumes_daily_gemini)

#Kraken
#NO OI DATA FOR NOW

Volumes_daily_kraken<-subset(Volumes_daily,select=c("DATE","Kraken Volume"))
Volumes_daily_kraken$DATE<-rownames(Volumes_daily_kraken)

#CME
#ONLY OI DATA

OI_daily_cme<-subset(OI_daily,select=c("DATE","CME OI"))
OI_daily_cme$DATE<-rownames(OI_daily_cme)


####daily combined chart vol only##################################################


merge_1<-merge(Volumes_daily_binance,Volumes_daily_okex,by="DATE")
merge_2<-merge(merge(merge_1,Volumes_daily_coinbase,by="DATE"),Volumes_daily_bitfinex,by="DATE")
merge_3<-merge(merge(merge_2,Volumes_daily_huobi,by="DATE"),Volumes_daily_liquid,by="DATE")               
merge_4<-merge(merge(merge_3,Volumes_daily_gemini,by="DATE"),Volumes_daily_kraken,by="DATE")
merge_vol_daily<-merge_4


#long format
merged.long_vol_daily<-gather(tail(merge_vol_daily,7),Exchange,Value,-DATE)

merged.long_vol_daily$Value<-as.numeric(merged.long_vol_daily$Value)
merged.long_vol_daily$Value<-merged.long_vol_daily$Value/1e+09
merged.long_vol_daily$Value<-as.numeric(merged.long_vol_daily$Value)


vol_daily_chart<-merged.long_vol_daily%>%
  ggplot(aes(x=DATE,y=Value,fill=Exchange))+#group=Asset
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(limits=unique(merged.long_vol_daily$DATE))+
  xlab("Days")+
  ylab("Volume (in Billions (in USD))")+
  ggtitle("Daily Volume")+
  theme_minimal()+
  labs(caption="Data Source:Cryptocompare. Chart:https://github.com/Saksham19/")+
  geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.25,check_overlap = T)+
  theme(plot.background = element_rect(fill="ivory2"),
        text=element_text(colour="gray17"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="gray17",face="bold",size=12),
        axis.line = element_line(colour="gray17"),
        axis.text = element_text(colour="gray17"))



###########daily combined chart OI ########################################################

merge_1<-merge(OI_daily_binance,OI_daily_cme,by="DATE")
merge_2<-merge(merge_1,OI_daily_huobi,by="DATE")
merge_3<-merge(merge_2,OI_daily_okex,by="DATE")
merge_oi_daily<-merge_3

#long format
merged.long_oi_daily<-gather(tail(merge_oi_daily,7),Exchange,Value,-DATE)

merged.long_oi_daily$Value<-as.numeric(merged.long_oi_daily$Value)
merged.long_oi_daily$Value<-merged.long_oi_daily$Value/1e+09
merged.long_oi_daily$Value<-as.numeric(merged.long_oi_daily$Value)


oi_daily_chart<-merged.long_oi_daily%>%
  ggplot(aes(x=DATE,y=Value,fill=Exchange))+#group=Asset
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(limits=unique(merged.long_oi_daily$DATE))+
  xlab("Days")+
  ylab("Open Interest (in Billions (USD))")+
  ggtitle("Daily Open Interest")+
  theme_minimal()+
  labs(caption="Data Source:Cryptocompare. Chart:https://github.com/Saksham19/")+
  geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.25,check_overlap = T)+
  theme(plot.background = element_rect(fill="ivory2"),
        text=element_text(colour="gray17"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="gray17",face="bold",size=12),
        axis.line = element_line(colour="gray17"),
        axis.text = element_text(colour="gray17"))





#weekly 

#binance

Volumes_daily_binance<-subset(Volumes_daily_binance,select = c(-DATE))
OI_daily_binance<-subset(OI_daily_binance,select=c(-DATE))

Volumes_daily_binance<-apply.weekly(Volumes_daily_binance,sum)
Volumes_daily_binance$DATE<-rownames(Volumes_daily_binance)
OI_daily_binance<-apply.weekly(OI_daily_binance,sum)
OI_daily_binance$DATE<-rownames(OI_daily_binance)
#okex 

Volumes_daily_okex<-subset(Volumes_daily_okex,select = c(-DATE))
OI_daily_okex<-subset(OI_daily_okex,select=c(-DATE))

Volumes_daily_okex<-apply.weekly(Volumes_daily_okex,sum)
Volumes_daily_okex$DATE<-rownames(Volumes_daily_okex)
OI_daily_okex<-apply.weekly(OI_daily_okex,sum)
OI_daily_okex$DATE<-rownames(OI_daily_okex)

#coinbase

Volumes_daily_coinbase<-subset(Volumes_daily_coinbase,select = c(-DATE))
Volumes_daily_coinbase<-apply.weekly(Volumes_daily_coinbase,sum)
Volumes_daily_coinbase$DATE<-rownames(Volumes_daily_coinbase)

#bitfinex

Volumes_daily_bitfinex<-subset(Volumes_daily_bitfinex,select = c(-DATE))
Volumes_daily_bitfinex<-apply.weekly(Volumes_daily_bitfinex,sum)
Volumes_daily_bitfinex$DATE<-rownames(Volumes_daily_bitfinex)

#huobi

Volumes_daily_huobi<-subset(Volumes_daily_huobi,select = c(-DATE))
OI_daily_huobi<-subset(OI_daily_huobi,select=c(-DATE))

Volumes_daily_huobi<-apply.weekly(Volumes_daily_huobi,sum)
Volumes_daily_huobi$DATE<-rownames(Volumes_daily_huobi)
OI_daily_huobi<-apply.weekly(OI_daily_huobi,sum)
OI_daily_huobi$DATE<-rownames(OI_daily_huobi)

#liquid

Volumes_daily_liquid<-subset(Volumes_daily_liquid,select = c(-DATE))
Volumes_daily_liquid<-apply.weekly(Volumes_daily_liquid,sum)
Volumes_daily_liquid$DATE<-rownames(Volumes_daily_liquid)

#gemini

Volumes_daily_gemini<-subset(Volumes_daily_gemini,select = c(-DATE))
Volumes_daily_gemini<-apply.weekly(Volumes_daily_gemini,sum)
Volumes_daily_gemini$DATE<-rownames(Volumes_daily_gemini)

#kraken

Volumes_daily_kraken<-subset(Volumes_daily_kraken,select = c(-DATE))
Volumes_daily_kraken<-apply.weekly(Volumes_daily_kraken,sum)
Volumes_daily_kraken$DATE<-rownames(Volumes_daily_kraken)

#cme

OI_daily_cme<-subset(OI_daily_cme,select=c(-DATE))
OI_daily_cme<-apply.weekly(OI_daily_cme,sum)
OI_daily_cme$DATE<-rownames(OI_daily_cme)

#weekly plot (vol only)


merge_1<-merge(Volumes_daily_binance,Volumes_daily_okex,by="DATE")
merge_2<-merge(merge(merge_1,Volumes_daily_coinbase,by="DATE"),Volumes_daily_bitfinex,by="DATE")
merge_3<-merge(merge(merge_2,Volumes_daily_huobi,by="DATE"),Volumes_daily_liquid,by="DATE")               
merge_4<-merge(merge(merge_3,Volumes_daily_gemini,by="DATE"),Volumes_daily_kraken,by="DATE")
merge_vol_weekly<-merge_4


#long format


merged.long_vol_weekly<-gather(tail(merge_vol_weekly,2),Exchange,Value,-DATE)

merged.long_vol_weekly$Value<-as.numeric(merged.long_vol_weekly$Value)
merged.long_vol_weekly$Value<-merged.long_vol_weekly$Value/1e+09
merged.long_vol_weekly$Value<-as.numeric(merged.long_vol_weekly$Value)



vol_weekly_chart<-merged.long_vol_weekly%>%
  ggplot(aes(x=DATE,y=Value,fill=Exchange))+#group=Asset
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(limits=unique(merged.long_vol_weekly$DATE))+
  xlab("Days")+
  ylab("Volume (in Billions (in USD))")+
  ggtitle("Weekly Volume")+
  theme_minimal()+
  labs(caption="Data Source:Cryptocompare. Chart:https://github.com/Saksham19/")+
  geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.25,check_overlap = T)+
  theme(plot.background = element_rect(fill="ivory2"),
        text=element_text(colour="gray17"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="gray17",face="bold",size=12),
        axis.line = element_line(colour="gray17"),
        axis.text = element_text(colour="gray17"))

#oi weekly


merge_1<-merge(OI_daily_binance,OI_daily_cme,by="DATE")
merge_2<-merge(merge(merge_1,OI_daily_huobi,by="DATE"),OI_daily_okex,by="DATE")
merge_oi_weekly<-merge_2


#long format


merged.long_oi_weekly<-gather(tail(merge_oi_weekly,2),Exchange,Value,-DATE)

merged.long_oi_weekly$Value<-as.numeric(merged.long_oi_weekly$Value)
merged.long_oi_weekly$Value<-merged.long_oi_weekly$Value/1e+09
merged.long_oi_weekly$Value<-as.numeric(merged.long_oi_weekly$Value)



oi_weekly_chart<-merged.long_oi_weekly%>%
  ggplot(aes(x=DATE,y=Value,fill=Exchange))+#group=Asset
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(limits=unique(merged.long_oi_weekly$DATE))+
  xlab("Days")+
  ylab("Open Interest (in Billions (in USD))")+
  ggtitle("Weekly Open Interest")+
  theme_minimal()+
  labs(caption="Data Source:Bybit. Chart:https://github.com/Saksham19/")+
  theme_minimal()+
  geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.25,check_overlap = T)+
  theme(plot.background = element_rect(fill="ivory2"),
        text=element_text(colour="gray17"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="gray17",face="bold",size=12),
        axis.line = element_line(colour="gray17"),
        axis.text = element_text(colour="gray17"))


#vol chart
grid.arrange(vol_daily_chart,vol_weekly_chart)


#oi chart
grid.arrange(oi_daily_chart,oi_weekly_chart)



