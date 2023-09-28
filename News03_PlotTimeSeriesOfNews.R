#Display teime series of candles, while poitning out position of critical zigzags and returns

setwd("D:/Scrips/Trading/OptiMoraR/")
###############################################################################################################################
########-----------------------------------------FUNCTIONS-------------------------------------------------------------########
###############################################################################################################################



#################################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------------###
###----START-------------------------------------Funtion Effect of filtering critical changes---------------------------------###
#-----------------------------------------------------------------------------------------------------------------------------###
#################################################################################################################################

PlotFilteringNews <- function(Ticker, Date,Change,News, CriticalThreshold, YHover, YAxisName){
  
  DataI=data.frame(Date=Date, Change=Change)
  #news for given pair
  News1=News %>% filter(currency == substr(Ticker, 1, 3))
  News2=News %>% filter(currency == substr(Ticker, 4, 6))
  
  NewsI=rbind(News1,News2) #Concantenate news from both pairs
  NewsI$Date=as.Date(NewsI$DateUTC,  format ="%Y-%m-%d")
  
  NewsI=NewsI[,c("event", "Date")]
  #Merge
  ReturnNewsI=merge(DataI,NewsI, by="Date", all.x=TRUE)
  
  
  CriticalDays=ReturnNewsI %>% filter(abs(Change) >=CriticalThreshold) #Select crtical days
  
  NewsEvents=unique(CriticalDays$event)
  
  CritLeftAfterSequaltialRemval=data.frame(Eventname="Total",CriticalLeft=length(unique(CriticalDays$Date)))
  
  #count number of critical entries left after removing days of given news.
  Count=length(NewsEvents)
  while (Count!=0){
    TemLeft=data.frame(Eventname=as.character(),CriticalLeft=as.numeric())
    NewsEvents=unique(CriticalDays$event)
    NewsEvents <- NewsEvents[!is.na(NewsEvents)]
    for (NewsI in NewsEvents){
      CriticalRetLeft = CriticalDays %>% filter(event != NewsI | (is.na(event)))
      NCriticalRetLeft=length(unique(CriticalRetLeft$Date))
      Vals=c(NewsI, NCriticalRetLeft)
      TemLeft=rbind(TemLeft,Vals)
    }
    
    colnames(TemLeft)=c("Eventname", "CriticalLeft")
    LeftCri=TemLeft %>% filter(CriticalLeft ==min(CriticalLeft))
    
    #removve sequentia news that rank even
    EvenNews=unique(LeftCri$Eventname)
    
    for (EvenNewsI in EvenNews){
      CriticalDays = CriticalDays %>% filter(event != EvenNewsI | (is.na(event)))
      FinalLeft=length(unique(CriticalDays$Date))
      FinalVals=c(EvenNewsI, FinalLeft)
      CritLeftAfterSequaltialRemval=rbind(CritLeftAfterSequaltialRemval,FinalVals)
    }
    Count=nrow(CriticalDays)
    NAs=sum(is.na(CriticalDays$event))
    if(NAs==Count){Count=0}
  }
  
  
  NonFilterCriticalReturn=CriticalDays[,c("Date", "Change")] #list of criticl returns that were not filtered
  
  
  #count days and dropdown left for working after news days are removed
  ImportantNews=CritLeftAfterSequaltialRemval$Eventname
  
  
  FinalDaysLeft=c()
  for (ImporantNewsI in ImportantNews){
    ReturnNewsI=ReturnNewsI %>% filter(event !=ImporantNewsI | (is.na(event))) #Exclude dates with the news and include days with no news...to cound days left after removing days with the given news
    MaxDDLeft=round(max(ReturnNewsI$Change,na.rm=TRUE),2)
    nDaysLeft=length(unique(ReturnNewsI$Date))
    ResltDayLeft=c(ImporantNewsI,nDaysLeft, MaxDDLeft)
    FinalDaysLeft=rbind(FinalDaysLeft,ResltDayLeft)
  }
  
  
  colnames(FinalDaysLeft)=c("Eventname", "DaysLeft", "MaximumDDLeft")
  
  
  
  FinalData=merge(CritLeftAfterSequaltialRemval, FinalDaysLeft,by="Eventname")
  
  FinalData$CriticalLeft=as.numeric(FinalData$CriticalLeft)
  FinalData$DaysLeft=as.numeric(FinalData$DaysLeft)
  FinalData$MaximumDDLeft=as.numeric(FinalData$MaximumDDLeft)
  
  TotalReturnsFinalData= FinalData %>% filter(Eventname=="Total")
  RestReturnsFinalData= FinalData %>% filter(Eventname!="Total")
  RestReturnsFinalData=RestReturnsFinalData %>% arrange(desc(CriticalLeft))
  
  FinalData=rbind(TotalReturnsFinalData, RestReturnsFinalData)
  FinalData$Rank=1:nrow(FinalData)
  
  MaxDays=max(FinalData$DaysLeft)
  FinalData$DaysLeft=(FinalData$DaysLeft/MaxDays)*100
  Y3Name=paste0("Trading Days Left (in Percent of ",MaxDays, " days")
  #plot relationship bwetween news and retruns
  CriticalRemovalPlot=Stocks::Plot3YAxis(X=FinalData$Rank,
                                         Y1=FinalData$CriticalLeft,
                                         Y2=FinalData$MaximumDDLeft,
                                         Y3=FinalData$DaysLeft,
                                         
                                         XNames=FinalData$Eventname,
                                         
                                         Y1AxisName=YAxisName,
                                         Y2AxisName="Maximum DropDown Left",
                                         Y3AxisName=Y3Name,
                                         
                                         Y1HoverName =YHover,
                                         Y2HoverName="LeftMaxDD",
                                         Y3HoverName="LeftGoodDays"
  )
  RETURN=list(CriticalRemovalPlot,NonFilterCriticalReturn )
  
  return(RETURN)
}

###----------END---------------------------------Funtion Effect of filtering critical changes---------------------------------###
#################################################################################################################################





#library(sqldf)
library(htmltools)
library(dygraphs)
library(purrr)
library(dplyr)
library(ggplot2)
library(plotly)
library(Stocks)
library(zoo)
library(data.table)

MinDDWanted=2 #percent of account wanted to be at maximum DD for this pair. 
ZigZagDDWated=2 #largest etesion in price used for sqing selection...used smaller than the returns DD wated..
PerCorrection=0.25 #Percent of correction tested. This is used to calcualte zigzag of this size of large. Basically, times when market extended before having a correction of this size


ForexData=fread("D:/Scrips/Trading/ForexPairs2YrsAll.csv") #ForexPairs2YrsAll  ForexPairs2Months.csv


ForexData=ForexData %>% rename("X" = "V1")


###############################################################################################################################
########---------------start---------------------DATA for daily returns------------------------------------------------########
###############################################################################################################################

Daility=ForexData[TF=="1 day", ]
Daility$Return=abs(Daility$h-Daility$l)/Daility$l*100 #Calcaulte returns as

Daility=Daility[,c(  "Date",    "Return","TickerNam")]
Daility$Date=as.Date(Daility$Date,  format ="%Y-%m-%d")

########---------------end-----------------------DATA for daily returns------------------------------------------------########
###############################################################################################################################





###############################################################################################################################
########---------------start---------------------DATA for swings-------------------------------------------------------########
###############################################################################################################################
## Swings are calcualte on 1 minute candle data, then for each day the maximum swings is calcualted. The idea is to create the 
## same data format as in the returns so the same fucntion for ploting can be used

ForexData=ForexData[TF=="1 min", ]
ForexData[,c( "v","TF")]=NULL


PAIRS=unique(Daility$TickerNam)
SwingsData=data.frame()
CriticalSwingsPositions=c()
for ( PairI in PAIRS){
  DataI=ForexData[TickerNam==PairI, ] #Select data for pair I
  DataI$X=1:nrow(DataI)
  
  SizeSwinsI=ZigZag_Frequency.Swings(DataI$c,change=PerCorrection) #Calculate size of swings for a given correction
  
  SwingsI=data.frame(Swings=ZigZag_Swings.Points(DataI$c,change=PerCorrection)) #Price point at which the swing occured. used to find the Candle where the swin happend
  
  SwingsI$X=DataI$X
  SwingsI=na.omit(SwingsI) 
  SwingsI=SwingsI[1:(nrow(SwingsI)-1),] #To select the start position of the  swings, the last row needs to be removed
  
  SwingsI$SizeSwins=SizeSwinsI #Apend the position of swins to their size
  SwingsI$Swings=NULL
  
  SwingsI=merge(DataI, SwingsI, by="X", all.x=TRUE)
  
  SwingsI$Date=as.Date(SwingsI$Date,  format ="%Y-%m-%d")
  SwingsI <- SwingsI %>% mutate(SizeSwins = ifelse(is.na(SizeSwins), 0, SizeSwins)) #candles that did not get any swings are set to cero
  
  #candle positoin of critical swinsgs
  CriticalSwingsTickerI=SwingsI %>% filter(abs(SizeSwins) >=ZigZagDDWated) #Select crtical days
  CriticalSwingsPositions=rbind(CriticalSwingsPositions,CriticalSwingsTickerI[,c("X","SizeSwins", "TickerNam")])
  
  SwingsI= SwingsI %>% group_by(Date) %>% summarise(MaxSwin = max(SizeSwins, na.rm=T)) #Get maximum swins in every day day
  
  SwingsI$TickerNam=PairI
  
  SwingsData=rbind(SwingsData, SwingsI)
}

#Remove data not needed t clear RAM.


SwingsData=as.data.frame(SwingsData)

###############################################################################################################################
########---------------end---------------------DATA for swings---------------------------------------------------------########
###############################################################################################################################





###############################################################################################################################
########---------------START-------------------NEWS for swings---------------------------------------------------------########
###############################################################################################################################
News=read.csv("D:/Scrips/Trading/OptiMoraR/ForexNewsData.csv")
News$X=NULL
###############################################################################################################################
########---------------end---------------------NEWS for swings---------------------------------------------------------########
###############################################################################################################################




###############################################################################################################################
###-------------------------------------------------Create Plot-------------------------------------------------------------###
###############################################################################################################################


for (TickerX in PAIRS){

  Data1MinTickerI=ForexData[TickerNam==TickerX, ]
  Data1MinTickerI$X=1:nrow(Data1MinTickerI)
  
  ZigZagValues=Zig.Zag(Data1MinTickerI$c,PerCorrection)
  
  
  Data1MinTickerI$DAY=as.Date(Data1MinTickerI$Date,  format ="%Y-%m-%d")
  #Mark begening of each day, to identify dates as only the index can be nicely ploted in dygraph
  Data1MinTickerI=Data1MinTickerI %>% mutate (NewDay=ifelse(DAY != dplyr::lag(DAY), 1,""))
  Data1MinTickerI$Y=0.5
  
  NewDays = Data1MinTickerI %>% filter(NewDay == 1 | (is.na(NewDay)))
  NewDays=NewDays[,c("X","DAY")]

  
  
  #Plot returns by news  
  Subset=Daility[TickerNam==TickerX, ]
  Date=Subset$Date
  Change=Subset$Return
  
#Plot effect on returns of filtering news
  YName=paste0("Critical Returns Left at ",MinDDWanted ,"% in ", TickerX)
  Returns=PlotFilteringNews (Ticker=TickerX, Date=Date, Change=Change,News=News, CriticalThreshold=MinDDWanted, YHover="CritReturnsLeft",YAxisName=YName)
  
  
  #Plot effect on swings of filtering news
  SwingsData=as.data.table(SwingsData)
  SubsetSwing=SwingsData[TickerNam==TickerX, ]
  
  DateSwings=SubsetSwing$Date
  ChangeSwings=SubsetSwing$MaxSwin
  
  YName=paste0("Critical Swings Left at ",ZigZagDDWated ,"% in ", TickerX)
  Swings=PlotFilteringNews (Ticker=TickerX, Date=DateSwings, Change=ChangeSwings,News=News, CriticalThreshold=ZigZagDDWated, YHover="CritSwingsLeft",YAxisName=YName)
  
  
#Candlestick plot
  CandlePlotZigzag=data.frame(Data1MinTickerI[,c("X", "o", "h", "l" ,"c")], ZigZagValues=ZigZagValues)
  
  Dygraph=dygraphs::dygraph(CandlePlotZigzag, width = 1800 ,height=600, group="X")%>%    dyCandlestick()   %>% 
    dyAxis("y", valueRange = c(min(CandlePlotZigzag$l), max(CandlePlotZigzag$h)))  %>% 
    dyAxis("x", drawGrid =FALSE) 
  
  
  #Add vertical lines for new days

  Dygraph=Dygraph %>% dyEvent(NewDays$X, NewDays$DAY, labelLoc = "bottom",color ="blue",strokePattern="solid")

  
#Plots critical returns and zigzags
  Axis=rbind(head(Data1MinTickerI,1),tail(Data1MinTickerI,1))
  PlotCriticalReturns <- dygraphs::dygraph(Axis[,c("X", "Y")], group="X", width = 1800, height=150)%>%  dyOptions (strokeWidth= 0 ,drawPoints= TRUE) %>%  dyAxis("x", drawGrid =FALSE) %>%  dyAxis("y", drawGrid =FALSE) %>% dyAxis("y", valueRange = c(0, 1) )
  
  
  CriticalReturns=Returns[[2]]
  if(nrow(CriticalReturns)>=1){
  CriticalReturns=merge(CriticalReturns, NewDays, all.x=TRUE, by.y="DAY", by.x="Date")
  CriticalReturns$Counter=1:nrow(CriticalReturns)
  CriticalReturns$Label=paste0("R ", CriticalReturns$Counter, "of", max(CriticalReturns$Counter), "; ", round(CriticalReturns$Change,1), "%")
  
  PlotCriticalReturns=PlotCriticalReturns %>% dyEvent(CriticalReturns$X, CriticalReturns$Label, labelLoc = "bottom",color ="red")
  
  }
  
  
  CriticalSwings= Swings[[2]]
  if(nrow(CriticalSwings)>=1){
  CriticalSwings=merge(CriticalSwings, NewDays, all.x=TRUE, by.y="DAY", by.x="Date")
  CriticalSwings$Counter=1:nrow(CriticalSwings)
  CriticalSwings$Label=paste0("Z ", CriticalSwings$Counter, "of", max(CriticalSwings$Counter), "; ", round(CriticalSwings$Change,1), "%")
  PlotCriticalReturns=PlotCriticalReturns %>% dyEvent(CriticalSwings$X, CriticalSwings$Label, labelLoc = "bottom",color ="orange")
  }
  

#Position of inidcvudal critical zigzags
  IndividualCriticalSwingsPairI=CriticalSwingsPositions[TickerNam==TickerX, ]
  IndividualCriticalSwingsPairI$SizeSwins=round(IndividualCriticalSwingsPairI$SizeSwins,1)
  if(nrow(IndividualCriticalSwingsPairI)>=1){
    IndividualCriticalSwingsPairI$Label=paste0("Zigzag ", IndividualCriticalSwingsPairI$SizeSwins, "%")
  PlotCriticalReturns=PlotCriticalReturns %>% dyEvent(IndividualCriticalSwingsPairI$X, IndividualCriticalSwingsPairI$Label, labelLoc = "bottom",color ="green")
  }
  
#News
  NewsTickerI1=News %>% filter(currency == substr(TickerX, 1, 3))
  NewsTickerI2=News %>% filter(currency == substr(TickerX, 4, 6))
  NewsTickerI=rbind(NewsTickerI1,NewsTickerI2) #Concantenate news from both pairs
  NewsTickerI=NewsTickerI[,c("event", "DateUTC")]
  
#Get ikcerID for news
  Data1MinTickerI$DateCandles=as.Date(Data1MinTickerI$Date,  format ="%Y-%m-%d")
  Data1MinTickerI$DateUTCCandles=as.character(Data1MinTickerI$Date)
  NewsTickerIDByTime=merge( NewsTickerI, Data1MinTickerI, all.x=TRUE, by.x="DateUTC",by.y="DateUTCCandles")
  NewsTickerIDByTime=NewsTickerIDByTime[,c("X", "event","DateUTC") ]   
  
#Merge by date in case that by time id not match...ege, new avvonced after hours
  NewsTickerIDByTime$DAY=as.Date(NewsTickerIDByTime$DateUTC,  format ="%Y-%m-%d")
  NewsTickerIDByTime$DateUTC=NULL
  NewsTickerIDByDate=merge( NewsTickerIDByTime, NewDays, all.x=TRUE, by="DAY")
#X ids
  NewsTickerIDByDate=NewsTickerIDByDate%>% mutate(X=ifelse(is.na(X.x), X.y,X.x))
  NewsTickerIDByDate=NewsTickerIDByDate[,c("X", "event")]
  NewsTickerIDByDate=NewsTickerIDByDate[complete.cases(NewsTickerIDByDate), ]

  
  Dygraph=Dygraph %>% dyEvent(NewsTickerIDByDate$X, NewsTickerIDByDate$event, labelLoc = "top",color ="grey")
  
  #Merge candles with critical returns and zigzags
  CombinedPlot=list(Dygraph,PlotCriticalReturns)
  
  
 HTML= htmltools::browsable(htmltools::tagList(CombinedPlot))
  
 FileName=paste0("SwingsNews_",TickerX, ".html")
 save_html(HTML, FileName, background = "white", libdir = "lib", lang = "en")
}

