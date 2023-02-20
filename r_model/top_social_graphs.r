# TOP SOCIAL GRAPHS

library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)
library(plotly)

#set the directory your file reads from
setwd("/Users/sin/Desktop/senior design/StockPriceCorrelationModel/r_model/Data")

# reads MSCIdata file
msciData <- read.csv(file="MSCIData_2014_to_2018_2021_to_2022.csv")

# assigning relevant variables from the csv file
msciDate <- msciData$ASOF_DATE
isinID <- msciData$ISSUER_ISIN
ivaIndustry <- msciData$IVA_INDUSTRY
environmentalPillarScore <- msciData$ENVIRONMENTAL_PILLAR_SCORE
goverancePillarScore <- msciData$GOVERNANCE_PILLAR_SCORE
socialPillarScore <- msciData$SOCIAL_PILLAR_SCORE

# creates the data frame for the MSCI data
df2 <- data.frame(msciDate, isinID, environmentalPillarScore, goverancePillarScore, socialPillarScore)

# retrieves the Month as a new variable from the given msciDate
df2$Month <- format(as.Date(msciDate, format="%Y-%m-%d"),"%m")

# retrieves the Year as a new variable from the given msciDate
df2$Year <- format(as.Date(msciDate, format="%Y-%m-%d"),"%Y")

df2[order(as.Date(df2$Date, format="%Y-%m-%d"))]
group_mean2 <- aggregate(cbind(environmentalPillarScore, goverancePillarScore, socialPillarScore) ~ isinID+Month+Year, data = df2, mean)

#-------------------------------------------
# reads priceData file
priceData <- read.csv(file="PriceData_2012_2022.csv")

# assigning relevant variables from the csv file
ticker <- priceData$BB_TICKER_CD
price <- as.numeric(priceData$PBD_PRICE_AMT)
priceDate <- priceData$DATE_DIM_ID
isinID <- priceData$ISIN_CD

# this gives an error for some reason when converting to a string
# ERROR: prices do not load into df
#price <- strtoi(price_string)

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

# Highest Social Correlation
# 1. US59522J1034  MAA  0.9311474
# 2. US0530151036  ADP  0.9287505
# 3. US0200021014  ALL  0.9203513
#--------------------------------------------------------------------------------------------------------------------------
# Lowest Social Correlation
# 1. US57060D1081  MKTX  -0.9644047
# 2. US0152711091  ARE   -0.9301337
# 3. US45168D1046  IDXX  -0.9105077


#--------------------------------------------------------------------------------------------------------------------------
# copy and paste the code below for each graph 
# just change ticker name for the variables
# and update ISIN_ID
#--------------------------------------------------------------------------------------------------------------------------
# Highest Social Correlation
# 1. US59522J1034  MAA  0.9311474
# Price
MAA_all_info <- df[df$isinID == 'US59522J1034',]
MAA_table <- data.frame(MAA_all_info)

Date <- MAA_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- MAA_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
MAA_Price_Graph <- plot_ly(MAA_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# MAA MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
MAA_msci <- df3[df3$isinID == 'US92345Y1064',]
MAA_msci_scores <- data.frame(MAA_msci)

MAA_Social_Score <- MAA_msci_scores$socialPillarScore

MAA_Social_Graph <- plot_ly(MAA_msci_scores, x= ~YearMonth, y= ~MAA_Social_Score, type= 'scatter')


# save_image(MAA_Price_Graph, "/Users/sin/Desktop/senior design/StockPriceCorrelationModel/r_model/Data/Graphs/Social/MAA_Price.png", ...,  width = NULL, height = NULL, scale = NULL)
# save_image(MAA_Social_Graph, "/Users/sin/Desktop/senior design/StockPriceCorrelationModel/r_model/Data/Graphs/Social/MAA_Social.svg")

#--------------------------------------------------------------------------------------------------------------------------
# Highest Social Correlation
# 2. US0530151036  ADP  0.9287505
# Price
ADP_all_info <- df[df$isinID == 'US0530151036',]
ADP_table <- data.frame(ADP_all_info)

Date <- ADP_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- ADP_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
ADP_Price_Graph <- plot_ly(ADP_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# ADP MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
ADP_msci <- df3[df3$isinID == 'US0530151036',]
ADP_msci_scores <- data.frame(ADP_msci)

ADP_Social_Score <- ADP_msci_scores$socialPillarScore

ADP_Social_Graph <- plot_ly(ADP_msci_scores, x= ~YearMonth, y= ~ADP_Social_Score, type= 'scatter')

#--------------------------------------------------------------------------------------------------------------------------
# Highest Social Correlation
# 3. US0200021014  ALL  0.9203513
# Price
ALL_all_info <- df[df$isinID == 'US0200021014',]
ALL_table <- data.frame(MAA_all_info)

Date <- ALL_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- ALL_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
ALL_Price_Graph <- plot_ly(ALL_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# ALL MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
ALL_msci <- df3[df3$isinID == 'US0200021014',]
ALL_msci_scores <- data.frame(ALL_msci)

ALL_Social_Score <- ALL_msci_scores$socialPillarScore

ALL_Social_Graph <- plot_ly(ALL_msci_scores, x= ~YearMonth, y= ~ALL_Social_Score, type= 'scatter')

#--------------------------------------------------------------------------------------------------------------------------
# Lowest Social Correlation
# 1. US57060D1081  MKTX  -0.9644047
# Price
MKTX_all_info <- df[df$isinID == 'US57060D1081',]
MKTX_table <- data.frame(MAA_all_info)

Date <- MKTX_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- MKTX_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
MKTX_Price_Graph <- plot_ly(MKTX_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# MKTX MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
MKTX_msci <- df3[df3$isinID == 'US57060D1081',]
MKTX_msci_scores <- data.frame(MKTX_msci)

MKTX_Social_Score <- MKTX_msci_scores$socialPillarScore

MKTX_Social_Graph <- plot_ly(MKTX_msci_scores, x= ~YearMonth, y= ~MKTX_Social_Score, type= 'scatter')

#--------------------------------------------------------------------------------------------------------------------------
# Lowest Social Correlation
# 2. US0152711091  ARE   -0.9301337
# Price
ARE_all_info <- df[df$isinID == 'US0152711091',]
ARE_table <- data.frame(ARE_all_info)

Date <- ARE_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- ARE_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
ARE_Price_Graph <- plot_ly(ARE_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# ARE MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
ARE_msci <- df3[df3$isinID == 'US0152711091',]
ARE_msci_scores <- data.frame(ARE_msci)

ARE_Social_Score <- ARE_msci_scores$socialPillarScore

ARE_Social_Graph <- plot_ly(ARE_msci_scores, x= ~YearMonth, y= ~ARE_Social_Score, type= 'scatter')

#--------------------------------------------------------------------------------------------------------------------------
# Lowest Social Correlation
# 3. US45168D1046  IDXX  -0.9105077
# Price
IDXX_all_info <- df[df$isinID == 'US45168D1046',]
IDXX_table <- data.frame(IDXX_all_info)

Date <- IDXX_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- IDXX_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
IDXX_Price_Graph <- plot_ly(IDXX_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

# IDXX MSCI Social Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
IDXX_msci <- df3[df3$isinID == 'US45168D1046',]
IDXX_msci_scores <- data.frame(IDXX_msci)

IDXX_Social_Score <- IDXX_msci_scores$socialPillarScore

IDXX_Social_Graph <- plot_ly(IDXX_msci_scores, x= ~YearMonth, y= ~IDXX_Social_Score, type= 'scatter')
