#Generates filters by pairs of the times when critical news occured.
#WEB news were a set of 10 news that are consisered economically important... 
library(dplyr)
library(data.table)
library(lubridate)

setwd("D:/Scrips/Trading/seed_camilo_mora_newshistorical/data/")
ForexTimeLine=data.frame(Date=seq(ymd('2021-01-01'),ymd('2023-09-27'), by = '1 day'))
PercentFreqRemoval=10 #remove news that which frequency is below this number..too frequent news reduce trading too much

Pairs= c("AUDCAD", "AUDCHF", "AUDJPY", "AUDNZD", "AUDUSD", "CADCHF", "CADJPY", "CHFJPY", "EURAUD", "EURCAD", "EURCHF" , "EURGBP", "EURJPY", "EURNZD", "EURUSD", "GBPAUD", "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "GBPUSD",  "NZDCAD", "NZDCHF", "NZDJPY","NZDUSD", "USDCAD", "USDCHF", "USDJPY", "XAUUSD")


###############################################################################################################################
########---------------START-------------------NEWS -------------------------------------------------------------------########
###############################################################################################################################
    News=read.csv("D:/Scrips/Trading/News/ForexNewsData.csv")
    News$From=as.Date(News$DateUTC,  format ="%Y-%m-%d")
    TradingDays=abs(as.numeric(difftime(News$DateUTC[nrow(News)],News$DateUTC[1])))
    News=News[, c("currency", "event", "impact","From") ]
    News$To=News$From
    News$Ocurrence="_D_"
    News$Day=lubridate::wday(News$From, week_start=1)
    News$OcurrenceRank=1 #used to rank the different filters (e.g. same day(1)), before (2), after(2), or all beforeduring and after(3), in cases when the results of the filters are the same. Best is 1
    
    
    
    DayAfter=DayBefore=DayBeforeAfter=News
    
    #Range for same day and day after
    DayAfter$Ocurrence="_DA"
    DayAfter$To <- fifelse(DayAfter$Day >= 5, DayAfter$From+(7-DayAfter$Day+1), DayAfter$From+1) #if news annoument done from friday to Sunday, then block Monday trading
    DayAfter$OcurrenceRank=2
    #Range for same day and day after
    DayBefore$Ocurrence="BD_"
    DayBefore$From <- fifelse(DayBefore$Day == 1 , DayBefore$To-3, fifelse(DayBefore$Day == 7 , DayBefore$To-2, DayBefore$To-1))#if news announcement done from Saturday to Monday, then block Friday trading
    DayBefore$OcurrenceRank=2
    #Range for before, same, and after day of news announcement
    DayBeforeAfter$Ocurrence="BDA"
    DayBeforeAfter$From <- DayBefore$From
    DayBeforeAfter$To   <- DayAfter$To
    DayBeforeAfter$OcurrenceRank=3
    

###############################################################################################################################
########---------------end---------------------NEWS for swings---------------------------------------------------------########
###############################################################################################################################


###############################################################################################################################
########---------------START-------------------CriticalNews------------------------------------------------------------########
###############################################################################################################################

   
TypeFilter=c("RETURNS", "SWINGS", "WEB")
    
for (TypeFilterI in TypeFilter){
 
  if(TypeFilterI=="SWINGS")    {CritNews=read.csv("D:/Scrips/Trading/News/News01_CriticalNews_ListByPair_SwingsOnly.csv")}
  if(TypeFilterI=="RETURNS")   {CritNews=read.csv("D:/Scrips/Trading/News/News01_CriticalNews_ListByPair_ReturnsAndSwing.csv")}
  if(TypeFilterI=="WEB")   {CritNews=read.csv("D:/Scrips/Trading/News/News01_CriticalNews_FromWeb.csv")}
  
for (PairI in Pairs){
  Currency1=substr(PairI, 1, 3)
  Currency2=substr(PairI, 4, 6)
  # List of critical news for Pair I from Code News01
  if(TypeFilterI=="WEB"){
    CritNewsPairI =CritNews %>% filter(Currency==Currency1 | Currency==Currency2) #slect uniques
  } else {
    CritNewsPairI=CritNews %>% filter(Pair==PairI) #list of critical news from Code News01
  }
  # List of all news by currency name. Includes times of reports. This is from the web-scrapper code. using the fucntion from Stocks
  News1=DayBeforeAfter %>% filter(currency == Currency1) 
  News2=DayBeforeAfter %>% filter(currency == Currency2 )
  NewsI=rbind(News1,News2) #Concantenate news from both pairs
  
  #Merge list of critical news to all news. This is to select the times when critical news were reported.
  TimesOfCritNews= inner_join(NewsI, CritNewsPairI, by = c("currency" = "Currency", "event" = "Event"))
    

  ##remove frequent news. Frequent news can exclude critical days by their sheer occurence
  FreqNews =TimesOfCritNews %>% group_by(currency,event) %>% summarise(Frequency=n())
  FreqNews$Fraction=FreqNews$Frequency/TradingDays*100
  FreqNews=FreqNews %>% filter(Fraction<=PercentFreqRemoval) 
  
  TimesOfCritNews= inner_join(TimesOfCritNews, FreqNews, by = c("currency" = "currency", "event" = "event"))
  
  if(nrow(TimesOfCritNews)>=1){
  TimesOfCritNews$IsCritical="Yes"
  
  CriticalTimeSeries=left_join(ForexTimeLine,TimesOfCritNews, join_by(between(Date, From, To))) #Merge times of criticsal news to time series for trading.
  
  CriticalTimeSeries=CriticalTimeSeries %>% filter(IsCritical == "Yes")
  CriticalTimeSeries=data.frame(Date=unique(CriticalTimeSeries$Date), IsCritical = "Yes") #Days of critical news
  
  CriticalTimeSeries=merge(ForexTimeLine, CriticalTimeSeries, all.x=TRUE, by="Date")
  
  CriticalTimeSeries$IsCritical[is.na(CriticalTimeSeries$IsCritical)] <- "No"
  } else {CriticalTimeSeries=ForexTimeLine; CriticalTimeSeries$IsCritical="No"}
  CriticalTimeSeries$D1=format(CriticalTimeSeries$Date, "%Y%m%dT")
  CriticalTimeSeries$o=ifelse(CriticalTimeSeries$IsCritical=="Yes",1,0)
  CriticalTimeSeries$h=ifelse(CriticalTimeSeries$IsCritical=="Yes",1,0)
  CriticalTimeSeries$l=ifelse(CriticalTimeSeries$IsCritical=="Yes",1,0)
  CriticalTimeSeries$c=ifelse(CriticalTimeSeries$IsCritical=="Yes",1,0)
  CriticalTimeSeries$v=0
  CriticalTimeSeries$Date=NULL
  CriticalTimeSeries$IsCritical=NULL
  
  Name=paste0(TypeFilterI,"_",PairI, ".csv")
  write.table( CriticalTimeSeries, Name, sep=",",  col.names=FALSE, row.names =FALSE,quote=FALSE)
}
}