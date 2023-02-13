library(data.table)
library(dplyr)
library(corrr)
library(tidyverse)
library(plotly)

# Raytheon Technologies Corp (RTX) vs Environmental Pillar Score

# Walmart (WMT) vs Social Pillar Score

# Genworth Financial Inc (GNW) vs  Governance Pillar Score

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
# x is date
# y is msci
#----------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
# RTX Price
#RTX_all_info <- df[df$ticker == 'RTX US',]
RTX_all_info <- df[df$isinID == 'US75513E1010',]
RTX_table <- data.frame(RTX_all_info)

Date <- RTX_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
Price <- RTX_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
RTX_Price <- plot_ly(RTX_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

#write.csv(Price_Frame,"/Users/KatePiotrowski/Correlation_Model/Graphs/RTX_Price_Date.csv", row.names = FALSE)

# RTX MSCI Environmental Score vs Date
df3$YearMonth <- paste(df3$Year, df3$Month)
RTX_msci <- df3[df3$isinID == 'US75513E1010',]
RTX_msci_scores <- data.frame(RTX_msci)

RTX_Enviromental_Score <-RTX_msci_scores$environmentalPillarScore

RTX_Enviromental_Graph <- plot_ly(RTX_msci_scores, x= ~YearMonth, y= ~RTX_Enviromental_Score, type= 'scatter')
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# GNW MSCI Governance Score vs Date
GNW_all_info <- df[df$isinID == 'US37247D1063',]
GNW_table <- data.frame(GNW_all_info)
GNW_msci <- df3[df3$isinID == 'US37247D1063',]
GNW_msci_scores <- data.frame(GNW_msci)

GNW_Governance_Score <- GNW_msci_scores$goverancePillarScore

GNW_Governance_Graph <- plot_ly(GNW_msci_scores, x= ~YearMonth, y= ~GNW_Governance_Score, type= 'scatter')

# GNW Price 
Date_YYYYMMDD <- GNW_table$priceDate
Dates_Formatted <-as.Date(as.character(Date_YYYYMMDD),format="%Y%m%d")
Price <- GNW_table$price

Price_Frame <- data.frame(Price, Dates_Formatted)
#write.csv(Price_Frame,"/Users/KatePiotrowski/Correlation_Model/Graphs/GNW_Price_Date.csv", row.names = FALSE)

GNW_Price_Graph <- plot_ly(GNW_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# WMT MSCI Social Score vs Date
WMT_all_info <- df[df$isinID == 'US9311421039',]
WMT_table <- data.frame(WMT_all_info)
WMT_msci <- df3[df3$isinID == 'US9311421039',]
WMT_msci_scores <- data.frame(WMT_msci)

WMT_Social_Score <- WMT_msci_scores$socialPillarScore

WMT_MSCI_Social_Graph <- plot_ly(WMT_msci_scores, x= ~YearMonth, y= ~WMT_Social_Score, type= 'scatter')

# WMT Price
Date_YYYYMMDD <- WMT_table$priceDate
Dates_Formatted <-as.Date(as.character(Date_YYYYMMDD),format="%Y%m%d")
Price <- WMT_table$price

WMT_Price_Graph <- plot_ly(WMT_table, x = ~Dates_Formatted, y = ~Price, type = 'scatter')

#Price_Frame <- data.frame(Price, Dates_Formatted)
#write.csv(Price_Frame,"/Users/KatePiotrowski/Correlation_Model/Graphs/WMT_Price_Date.csv", row.names = FALSE)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------



