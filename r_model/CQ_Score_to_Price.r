# This Correlates Price to CQ Score

library(data.table)
library(dplyr)
library(tidyverse)
library(plotly)
library(relations)
library(lubridate)
library(reshape2)
library(corrr)


# Reads csv file
cq_data <- read.csv(file="/Data/Input/CQ_Score_to_Price/cq.csv")

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
#write.csv(df1,"/Data/Output/CQ_Score_to_Price/Removed_CQ_Data.csv", row.names = FALSE)
#--------------------------------------------------------------------------------------------------------------
# Read the file with the removed data 
removed_cq_data <- read.csv(file="/Data/Output/CQ_Score_to_Price/Removed_CQ_Data.csv")

# Assigning Variables 
date <- as.character(removed_cq_data$date)
ticker <- removed_cq_data$ticker
isin_ID <- removed_cq_data$isin_ID
price <- as.numeric(removed_cq_data$price)
cq_score <- as.numeric(removed_cq_data$cq_score)

# Create data frame
df2 <- data.frame(date, ticker, isin_ID, price, cq_score)

# Formated Date YYYY-MM-DD
df2$Month <- format(as.Date(date, format="%Y%m%d"),"%m")

df2$Year <- format(as.Date(date, format="%Y%m%d"),"%Y")

df2$Month_Year_Together <- format(as.Date(date, format="%Y%m%d"),"%m/%Y")

df2[order(as.Date(df2$Date, format="%Y-%m-%d"))]

group_mean <- aggregate(price ~ ticker+Month+Year+Month_Year_Together, data = df2, mean)

cq_agg <- aggregate(cq_score ~ ticker+Month+Year+Month_Year_Together, data = df2, mean)
#--------------------------------------------------------------------------------------------------------------
# joins tables
joined_cq_agg_and_group_mean <- merge(group_mean, cq_agg)

# remove missing
removed_g_mean <- joined_cq_agg_and_group_mean[-which(joined_cq_agg_and_group_mean$ticker == ""), ]

#-----------------------------------------------------------------------------------------------------

cq_to_price_correlation <- removed_g_mean %>%
  group_by(ticker)  %>%
  summarize(cor=cor(price, cq_score))

# Assuming your data frame is named 'dataframe', if not, replace it with the correct name
# Group the data frame by ticker
ticker_groups <- removed_g_mean %>% group_by(ticker)

# Calculate the correlation coefficient between cq_score and price for each ticker
correlations <- ticker_groups %>%
  summarize(correlation_coefficient = cor(cq_score, price, use = "complete.obs")) %>%
  na.omit() # Remove rows with missing values (if any)

# Write to CSV file
write.csv(correlations,"/Users/KatePiotrowski/cq_analysis/cq/Data/Final_Output/CQ_Score_to_Price/CQ_Score_to_Price_Correlations.csv")

# Check Number of Tickers
#tickers_no <- unique(removed_g_mean$ticker)


  

