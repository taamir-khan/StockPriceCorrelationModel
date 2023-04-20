# This Correlates ESG (MSCI) to CQ Score

library(data.table)
library(dplyr)
library(tidyverse)
library(plotly)
library(relations)
library(lubridate)
library(reshape2)
library(corrr)

# Reads csv file
cq_data <- read.csv(file="/cq_analysis/cq/Data/Input/CQ_Score_to_ESG_MSCI/cq.csv")

# Assigning Variables 
date <- cq_data$DATE_DIM_ID
ticker <- cq_data$BB_TICKER_CD
isin_ID <- cq_data$ISIN_CD
price <- cq_data$PBD_PRICE
cq_score <- cq_data$INT_CQ_AMT

# Create data frame
df1 <- data.frame(cq_data)

# Create data frame
df1 <- data.frame(date, ticker, isin_ID, price, cq_score)

# Remove instances of column titles in the table 
df1 <- df1[-(507265),]
df1 <- df1[-(1032477),]
df1 <- df1[-c(1032476),]

# Save to a new file that has the removed column titles 
write.csv(df1,"/cq_analysis/cq/Data/Intermediary_Output/CQ_Score_to_ESG_MSCI/Removed_CQ_Data.csv", row.names = FALSE)
#--------------------------------------------------------------------------------------------------------------
# Read the file with the removed data 

removed_cq_data <- read.csv(file="/cq_analysis/cq/Data/Intermediary_Output/CQ_Score_to_ESG_MSCI/Removed_CQ_Data.csv")

# Assigning Variables 
date <- as.character(removed_cq_data$date)
ticker <- removed_cq_data$ticker
isin_ID <- removed_cq_data$isin_ID
price <- as.numeric(removed_cq_data$price)
cq_score <- as.numeric(removed_cq_data$cq_score)

# Create data frame
#df2 <- data.frame(removed_cq_data)
df2 <- data.frame(date, ticker, isin_ID, price, cq_score)

# Formated Date YYYY-MM-DD

df2$Month <- format(as.Date(date, format="%Y%m%d"),"%m")

df2$Year <- format(as.Date(date, format="%Y%m%d"),"%Y")

df2$Month_Year_Together <- format(as.Date(date, format="%Y%m%d"),"%m/%Y")

df2[order(as.Date(df2$Date, format="%Y-%m-%d"))]

group_mean <- aggregate(price ~ ticker+Month+Year+Month_Year_Together, data = df2, mean)

cq_agg <- aggregate(cq_score ~ ticker+Month+Year+Month_Year_Together, data = df2, mean)
#-------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
# joins tables
joined_cq_agg_and_group_mean <- merge(group_mean, cq_agg)

# remove missing
removed_g_mean <- joined_cq_agg_and_group_mean[-which(joined_cq_agg_and_group_mean$ticker == ""), ]
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# Read the file with ESG MSCI Data
ESG_Data <- read.csv(file="/cq_analysis/cq/Data/Input/CQ_Score_to_ESG_MSCI/ESG_Price_Date_Data.csv")

isin_ID <- ESG_Data$isinID
ticker <- ESG_Data$ticker
month <- ESG_Data$Month
year <- ESG_Data$Year
price <- ESG_Data$price
environmental_pillar_score <- ESG_Data$environmentalPillarScore
governance_pillar_score <- ESG_Data$goverancePillarScore
social_pillar_score <- ESG_Data$socialPillarScore

# Create data frame for ESG data
df_esg <- data.frame(ESG_Data)

# Create data frame for ESG data
df_esg <- data.frame(ticker, isin_ID, price, month, year, price, environmental_pillar_score, social_pillar_score, governance_pillar_score)
#--------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------
# Join ESG Data Frame with CQ Data Frame
# (joining df_esg and removed_g_mean)
df3 <- removed_g_mean %>% inner_join( df_esg, 
                                  by=c('price'='price'),
                                  multiple = "all")
# removing extra columns 
df4 <- subset(df3, select = -c(ticker.y, month, year, price))

#--------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------
environmental_corr <- df4 %>%
  group_by(ticker.x)  %>%
  summarize(cor=cor(cq_score, environmental_pillar_score))

governance_corr <- df4 %>%
  group_by(ticker.x)  %>%
  summarize(cor=cor(cq_score, governance_pillar_score))

social_corr <- df4 %>%
  group_by(ticker.x)  %>%
  summarize(cor=cor(cq_score, social_pillar_score))

df_list <- list(governance_corr, environmental_corr, social_corr)

CQ_Score_to_ESG_MSCI_Correlation <- df_list %>% reduce(full_join, by='ticker.x')
colnames(CQ_Score_to_ESG_MSCI_Correlation) <- c('ticker.x', 'governancePillarScore', 'environmentalPillarScore', 'socialPillarScore')

# write correlations to CSV file
write.csv(CQ_Score_to_ESG_MSCI_Correlation,"/cq_analysis/cq/Data/Final_Output/CQ_Score_to_ESG_MSCI/CQ_Score_to_ESG_MSCI_Correlation.csv")


