library(Stocks)
library(dplyr)
setwd("D:/Scrips/Trading/News/")
News=GetNewsForexFactory(From = "2021/01/01", To ="2023/12/31")

write.csv(News,"ForexNewsData.csv")

