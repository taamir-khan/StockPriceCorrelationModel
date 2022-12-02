library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)

# sets path
setwd("~/Desktop/senior design/r_model/Data")

# MSCI Data
msciData <- read.csv(file="MSCI_SUMMARY_SCORES3_Removed.csv")

msciDate <- msciData$ASOF_DATE
isinID <- msciData$ISSUER_ISIN
industryAdjustedScore <- msciData$INDUSTRY_ADJUSTED_SCORE
weightedAvgScore <- msciData$WEIGHTED_AVERAGE_SCORE
ivaIndustry <- msciData$IVA_INDUSTRY
environmentalPillarScore <- msciData$ENVIRONMENTAL_PILLAR_SCORE
goverancePillarScore <- msciData$GOVERNANCE_PILLAR_SCORE
socialPillarScore <- msciData$SOCIAL_PILLAR_SCORE

df2 <- data.frame(msciDate, isinID, industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore)

df2$Month <- format(as.Date(msciDate, format="%Y-%m-%d"),"%m")
df2$Year <- format(as.Date(msciDate, format="%Y-%m-%d"),"%Y")

df2[order(as.Date(df2$Date, format="%Y-%m-%d"))]
group_mean2 <- aggregate(cbind(industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore) ~ isinID+Month+Year, data = df2, mean)
#group_mean2

# Price Data
priceData <- read.csv(file="Price_Data3_ISIN.csv")

ticker <- priceData$BB_TICKER_CD
price <- priceData$PBD_PRICE_AMT
priceDate <- priceData$DATE_DIM_ID
isinID <- priceData$ISIN_CD

# set-up data frame with name, date, price, y1
df <- data.frame(priceDate, ticker, isinID, price)

df$Month <- format(as.Date(priceDate, format="%Y%m%d"),"%m")
df$Year <- format(as.Date(priceDate, format="%Y%m%d"),"%Y")

df[order(as.Date(df$Date, format="%Y%m%d"))]

group_mean <- aggregate(price ~ isinID+ticker+Month+Year, data = df, mean)
#group_mean

# df3 <- merge(df,df2,by=c("Month","Year"))
# df3
# dplr <- left_join(df, df2, by=c("Month", "Year"))
# dplr
# setDT(group_mean2)[group_mean, c("industryAdjustedScore", "weightedAvgScore", "environmentalPillarScore", "goverancePillarScore", "socialPillarScore") := 
#              .(industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore), on=c("Year", "Month", "price")]
#df3 <- full_join(group_mean, group_mean2, by = c("isinID", "Month", "Year"), all = TRUE)

df3 <- group_mean %>% inner_join( group_mean2, 
                              by=c('isinID'='isinID', 'Year'='Year', 'Month'='Month'))


industry_corr <- df3 %>%
       group_by(ticker)  %>%
       summarize(cor=cor(price, industryAdjustedScore))

environmental_corr <- df3 %>%
  group_by(ticker)  %>%
  summarize(cor=cor(price, environmentalPillarScore))

governance_corr <- df3 %>%
  group_by(ticker)  %>%
  summarize(cor=cor(price, goverancePillarScore))

social_corr <- df3 %>%
  group_by(ticker)  %>%
  summarize(cor=cor(price, socialPillarScore))

df_list <- list(industry_corr, environmental_corr, governance_corr, social_corr)

correlation_table <- df_list %>% reduce(full_join, by='ticker')
colnames(correlation_table) <- c('ticker', 'industryAdjustedScore','environmentalPillarScore', 'governancePillarScore', 'socialPillarScore')

#write.csv(correlation_table,"/Users/sin/Desktop/senior design/r_model/Output/correlation_table.csv", row.names = FALSE)
write.csv(df3,"/Users/sin/Desktop/senior design/r_model/Output/merged_data.csv", row.names = FALSE)



