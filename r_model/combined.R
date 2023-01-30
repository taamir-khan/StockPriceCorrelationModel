library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)

setwd("~/Desktop/senior design/r_model/Data")

combinedData <- read.csv(file="PriceData_2012_2022.csv")

ticker <- combinedData$BB_TICKER_CD
isin <- combinedData$ISIN_CD
date <- combinedData$DATE_DIM_ID
price <- combinedData$PBD_PRICE_AMT

df <- data.frame(ticker, isin, date, price)
df$Month <- format(as.Date(date, format="%Y%m%d"),"%m")
df$Year <- format(as.Date(date, format="%Y%m%d"),"%Y")
df[order(as.Date(df$Date, format="%Y%m%d"))]

df$price <- as.numeric(as.character(df$price))

df %>% group_by(Month, Year) %>% summarize(price = mean(df$price, na.rm = TRUE))

group_mean <- aggregate(price ~ isin+ticker+Month+Year, data = df, mean)
group_mean

  
  