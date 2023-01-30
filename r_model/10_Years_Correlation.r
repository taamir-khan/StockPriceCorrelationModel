library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)

# reads MSCIdata file
msciData <- read.csv(file="MSCIData_2014_to_2018_2021_to_2022.csv")

# assigning relevant variables from the csv file
msciDate <- msciData$ASOF_DATE
isinID <- msciData$ISSUER_ISIN
industryAdjustedScore <- msciData$INDUSTRY_ADJUSTED_SCORE
weightedAvgScore <- msciData$WEIGHTED_AVERAGE_SCORE
ivaIndustry <- msciData$IVA_INDUSTRY
environmentalPillarScore <- msciData$ENVIRONMENTAL_PILLAR_SCORE
goverancePillarScore <- msciData$GOVERNANCE_PILLAR_SCORE
socialPillarScore <- msciData$SOCIAL_PILLAR_SCORE

# creates the data frame for the MSCI data
df2 <- data.frame(msciDate, isinID, industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore)

# retrieves the Month as a new variable from the given msciDate
df2$Month <- format(as.Date(msciDate, format="%Y-%m-%d"),"%m")

# retrieves the Year as a new variable from the given msciDate
df2$Year <- format(as.Date(msciDate, format="%Y-%m-%d"),"%Y")

df2[order(as.Date(df2$Date, format="%Y-%m-%d"))]
group_mean2 <- aggregate(cbind(industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore) ~ isinID+Month+Year, data = df2, mean)
#group_mean2

#-------------------------------------------
# reads priceData file
priceData <- read.csv(file="PriceData_2012_2022.csv")

# assigning relevant variables from the csv file
ticker <- priceData$BB_TICKER_CD
price_string <- priceData$PBD_PRICE_AMT
priceDate <- priceData$DATE_DIM_ID
isinID <- priceData$ISIN_CD

price <- strtoi(price_string)

# creates a new data frame for the Price Data
df <- data.frame(priceDate, ticker, isinID, price)

# retrieves the Month as a new variable from the given priceData
df$Month <- format(as.Date(priceDate, format="%Y%m%d"),"%m")

# retrieves the Year as a new variable from the given priceData
df$Year <- format(as.Date(priceDate, format="%Y%m%d"),"%Y")

df[order(as.Date(df$Date, format="%Y%m%d"))]

group_mean <- aggregate(price ~ isinID+ticker+Month+Year, data = df, mean)


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

df_list <- list(industry_corr, governance_corr, environmental_corr, social_corr)

correlation_table <- df_list %>% reduce(full_join, by='ticker')
colnames(correlation_table) <- c('ticker', 'industryAdjustedScore', 'governancePillarScore', 'environmentalPillarScore', 'socialPillarScore')

write.csv(correlation_table,"/Users/KatePiotrowski/Correlation_Model/10_Years_Correlation_Table.csv", row.names = FALSE)

