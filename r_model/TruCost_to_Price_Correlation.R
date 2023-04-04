# This file correlates TruCost Data to Price

library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)

# reads TruCost data file
TruCost_Data <- read.csv(file="/Users/KatePiotrowski/Correlation_Model/Data_Input/truCost.csv")

# assign variables 
trucost_date <- TruCost_Data$ASOF_DATE
isin_ID <- TruCost_Data$ISIN
sector <- TruCost_Data$GICSSECTORNAME
carbon_intensity_direct_and_first_tier_indirect <- TruCost_Data$CARBONINTDIFIRSTTIERINDITCO2EUSDMN
carbon_emissions_scope_1 <- TruCost_Data$CARBONINTSCOPE1TCO2EUSDMN
carbon_emissions_scope_2 <- TruCost_Data$CARBONINTSCOPE2TCO2EUSDMN

# create data frame with variables from file
Intial_TruCost_df <- data.frame(isin_ID, trucost_date, sector, carbon_intensity_direct_and_first_tier_indirect, carbon_emissions_scope_1, carbon_emissions_scope_2)

# retrieves the Month as a new variable from the given date
Intial_TruCost_df$Month <- format(as.Date(trucost_date, format="%Y-%m-%d"),"%m")

# retrieves the Year as a new variable from the given date
Intial_TruCost_df$Year <- format(as.Date(trucost_date, format="%Y-%m-%d"),"%Y")

# puts the Month and Year together
Intial_TruCost_df$Month_Year_Together <- format(as.Date(trucost_date, format="%Y-%m-%d"),"%m/%Y")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1) Get the monthly average of carbon_intensity_direct_and_first_tier_indirect
group_mean_carbon_intensity_direct_and_first_tier_indirect <- aggregate(carbon_intensity_direct_and_first_tier_indirect ~ isin_ID+Month+Year+Month_Year_Together, data = Intial_TruCost_df, mean)

# 2) Get the monthly average of carbon_emissions_scope_1
group_mean_carbon_emissions_scope_1 <- aggregate(carbon_emissions_scope_1 ~ isin_ID+Month+Year+Month_Year_Together, data = Intial_TruCost_df, mean)

# 3) Get the monthly average of carbon_emissions_scope_2
group_mean_carbon_emissions_scope_2 <- aggregate(carbon_emissions_scope_2 ~ isin_ID+Month+Year+Month_Year_Together, data = Intial_TruCost_df, mean)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# join together each avg data frame aggregated above
join_step_1_and_step_2 <- group_mean_carbon_intensity_direct_and_first_tier_indirect %>% inner_join( group_mean_carbon_emissions_scope_1, 
                                  by=c('isin_ID'='isin_ID', 'Year'='Year', 'Month'='Month'))

# This data frame has all the TruCost averages
TruCost_Monthly_Avg <- join_step_1_and_step_2 %>% inner_join( group_mean_carbon_emissions_scope_2, 
                                                   by=c('isin_ID'='isin_ID', 'Year'='Year', 'Month'='Month'))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Now we begin handling the Price Data 

# reads priceData file
priceData <- read.csv(file="/Users/KatePiotrowski/Correlation_Model/Data_Input/PriceData_2012_2022.csv")

# assigning relevant variables from the csv file
ticker <- priceData$BB_TICKER_CD
price <- as.numeric(priceData$PBD_PRICE_AMT)
priceDate <- priceData$DATE_DIM_ID
isin_ID <- priceData$ISIN_CD

# creates a new data frame for the Price Data
df <- data.frame(priceDate, ticker, isin_ID, price)

# retrieves the Month as a new variable from the given priceData
df$Month <- format(as.Date(priceDate, format="%Y%m%d"),"%m")

# retrieves the Year as a new variable from the given priceData
df$Year <- format(as.Date(priceDate, format="%Y%m%d"),"%Y")

df[order(as.Date(df$Date, format="%Y%m%d"))]

group_mean <- aggregate(price ~ isin_ID+ticker+Month+Year, data = df, mean)

df3 <- group_mean %>% inner_join( TruCost_Monthly_Avg, 
                                  by=c('isin_ID'='isin_ID', 'Year'='Year', 'Month'='Month'))
# remove unnecessary columns 
update_df3 <- df3[,-c(6,8)]

# reorder columns 
TruCost_With_Price <- update_df3[, c(1, 2, 3, 4, 8, 5, 6, 7, 9)]
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Determine Correlations
carbon_intensity_direct_and_first_tier_indirect_corr <- TruCost_With_Price %>%
  group_by(isin_ID)  %>%
  summarize(cor=cor(price, carbon_intensity_direct_and_first_tier_indirect))

carbon_emissions_scope_1_corr <- TruCost_With_Price %>%
  group_by(isin_ID)  %>%
  summarize(cor=cor(price, carbon_emissions_scope_1))

carbon_emissions_scope_2_corr <- TruCost_With_Price %>%
  group_by(isin_ID)  %>%
  summarize(cor=cor(price, carbon_emissions_scope_2))

df_list <- list(carbon_intensity_direct_and_first_tier_indirect_corr, carbon_emissions_scope_1_corr, carbon_emissions_scope_2_corr)

trucost_correlation_table <- df_list %>% reduce(full_join, by='isin_ID')
colnames(trucost_correlation_table) <- c('isin_ID', 'carbon_intensity_direct_and_first_tier_indirect_corr', 'carbon_emissions_scope_1_corr', 'carbon_emissions_scope_2_corr')

# filter out 1's and -1's
my_data_filtered <- trucost_correlation_table[!apply(trucost_correlation_table[, c('carbon_intensity_direct_and_first_tier_indirect_corr', 'carbon_emissions_scope_1_corr', 'carbon_emissions_scope_2_corr')] == 1, 1, any), ]

#updated_filter <- my_data_filtered[!apply(my_data_filtered[, c('carbon_intensity_direct_and_first_tier_indirect_corr', 'carbon_emissions_scope_1_corr', 'carbon_emissions_scope_2_corr')] == -1, -1, any), ]


