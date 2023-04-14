# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Read stock price data
stock_price_data <- read.csv(file="/Users/KatePiotrowski/Correlation_Model/Data_Input/PriceData_2012_2022.csv")

# Convert DATE_DIM_ID to a date format and extract month and year
stock_price_data$DATE_DIM_ID <- as.Date(as.character(stock_price_data$DATE_DIM_ID), format = "%Y%m%d")
stock_price_data$year_month <- format(stock_price_data$DATE_DIM_ID, "%Y-%m")
stock_price_data$PBD_PRICE_AMT <- as.numeric(stock_price_data$PBD_PRICE_AMT)
ticker <- stock_price_data$BB_TICKER_CD

# data frame of ticker and isin_ID
Ticker_and_isinID <- stock_price_data[, c(2, 3)]

# get distinct values of ticker and isin_ID
distinct_ticker_isinID <- Ticker_and_isinID %>% distinct()

# Calculate the monthly average price
monthly_avg_price <- stock_price_data %>%
  group_by(ISIN_CD, year_month) %>%
  summarise(avg_price = mean(PBD_PRICE_AMT, na.rm = TRUE))

# Remove rows with NaN values in the avg_price column
monthly_avg_price <- monthly_avg_price %>%
  filter(!is.nan(avg_price))

# Remove rows with blank ISIN_CD values
monthly_avg_price <- monthly_avg_price %>%
  filter(ISIN_CD != "")

# Read TruCost data
trucost_data <- read.csv(file="/Users/KatePiotrowski/Correlation_Model/Data_Input/truCost.csv")

# Convert ASOF_DATE to a date format and extract month and year
trucost_data$ASOF_DATE <- as.POSIXct(trucost_data$ASOF_DATE, format = "%Y-%m-%d %H:%M:%S")
trucost_data$year_month <- format(trucost_data$ASOF_DATE, "%Y-%m")

# Calculate the monthly average carbon scores
monthly_avg_trucost <- trucost_data %>%
  group_by(ISIN, GICSSECTORNAME, year_month) %>%
  summarise(
    avg_scope1 = mean(CARBONINTSCOPE1TCO2EUSDMN, na.rm = TRUE),
    avg_scope2 = mean(CARBONINTSCOPE2TCO2EUSDMN, na.rm = TRUE),
    avg_first_tier = mean(CARBONINTDIFIRSTTIERINDITCO2EUSDMN, na.rm = TRUE))

# Join the two data frames on ISIN and year_month
combined_data <- monthly_avg_price %>%
  left_join(monthly_avg_trucost, by = c("ISIN_CD" = "ISIN", "year_month" = "year_month"))

# Fill missing carbon scores with the last available value
combined_data <- combined_data %>%
  group_by(ISIN_CD) %>%
  tidyr::fill(avg_scope1, avg_scope2, avg_first_tier, .direction = "down")

# Remove GICSSECTORNAME column
combined_data <- combined_data %>%
  select(-GICSSECTORNAME)

# Remove rows containing NA values
combined_data <- na.omit(combined_data)

# Calculate correlations
correlations <- combined_data %>%
  group_by(ISIN_CD) %>%
  summarise(
    carbon_intensity_direct_and_first_tier_indirect_correlation = cor(avg_price, avg_first_tier, use = "complete.obs"),
    carbon_emissions_scope_1_correlation = cor(avg_price, avg_scope1, use = "complete.obs"),
    carbon_emissions_scope_2_correlation = cor(avg_price, avg_scope2, use = "complete.obs")
  )

# Add the ticker name into the data frame
correlations_with_ticker <- merge(distinct_ticker_isinID, correlations, by="ISIN_CD")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Top TruCost Correlations carbon_intensity_direct_and_first_tier_indirect_corr
# 1. US0320951017 APH US 0.999238482
# 2. US6541061031 NKE US 0.984401164
# 3. 
#--------------------------------------------------------------------------------------------------------------------------
# Lowest TruCost Correlations carbon_intensity_direct_and_first_tier_indirect_corr
# 1. 
# 2. 
# 3. 

#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
# Top TruCost Correlations carbon_intensity_direct_and_first_tier_indirect_corr
# 1. US0320951017 APH US 0.999238482
APH_all_info <- df[df$isinID == 'US0320951017',]
APH_table <- data.frame(APH_all_info)

Date <- APH_table$priceDate
Dates_Formatted <-as.Date(as.character(Date),format="%Y%m%d")
APH_Price <- APH_table$price

Price_Frame <- data.frame(APH_Price, Dates_Formatted)
APH_Price_Graph <- plot_ly(APH_table, x = ~Dates_Formatted, y = ~APH_Price, type = 'scatter')

# APH carbon_intensity_direct_and_first_tier_indirect_core vs Date
TruCost_With_Price$Month_Year_Together <- paste(TruCost_With_Price$Year, TruCost_With_Price$Month)
APH_TruCost <- TruCost_With_Price[TruCost_With_Price$isin_ID == 'US0320951017',]
APH_TruCost_All <- data.frame(APH_TruCost)

APH_carbon_intensity_direct_and_first_tier_indirect_core <- APH_TruCost_All$carbon_intensity_direct_and_first_tier_indirect

APH_Graph_carbon_intensity_direct_and_first_tier_indirect_core <- plot_ly(APH_TruCost_All, x= ~Month_Year_Together, y= ~APH_carbon_intensity_direct_and_first_tier_indirect_core, type= 'scatter')


# WYNN US9831341071
WYNN_all_info <- df[df$isinID == 'US9831341071',]
WYNN_table <- data.frame(WYNN_all_info)
WYNN_distinct <- unique(WYNN_table)


# KLAC US US4824801009 0.9345321
KLAC_all_info <- combined_data[combined_data$ISIN_CD == 'US4824801009',]
KLAC_table <- data.frame(KLAC_all_info)

KLAC_Date <- KLAC_table$year_month
KLAC_Price <- KLAC_table$avg_price

Price_Frame <- data.frame(KLAC_Price, KLAC_Date)
KLAC_Price_Graph <- plot_ly(KLAC_table, x = ~KLAC_Date, y = ~KLAC_Price, type = 'scatter')

# TruCost
KLAC_TruCost_Graph <- plot_ly() %>%
  add_trace(x = ~KLAC_Date, y = ~KLAC_avg_scope1, type = 'scatter', mode = 'lines', name = 'avg_scope1') %>%
  add_trace(x = ~KLAC_Date, y = ~KLAC_avg_scope2, type = 'scatter', mode = 'lines', name = 'avg_scope2') %>%
  add_trace(x = ~KLAC_Date, y = ~KLAC_avg_first_tier, type = 'scatter', mode = 'lines', name = 'avg_first_tier') %>%
  layout(xaxis = list(title = 'Date'), yaxis = list(title = 'Value'), title = 'KLAC TruCost Graph')



# SNPS US8716071076
SNPS_all_info <- combined_data[combined_data$ISIN_CD == 'US8716071076',]
SNPS_table <- data.frame(SNPS_all_info)

SNPS_Date <- SNPS_table$year_month
SNPS_Price <- SNPS_table$avg_price

Price_Frame <- data.frame(SNPS_Price, SNPS_Date)
SNPS_Price_Graph <- plot_ly(SNPS_table, x = ~SNPS_Date, y = ~SNPS_Price, type = 'scatter')

# TruCost
SNPS_avg_scope1 <- SNPS_all_info$avg_scope1
SNPS_avg_scope2 <- SNPS_all_info$avg_scope2
SNPS_avg_first_tier <- SNPS_all_info$avg_first_tier

SNPS_TruCost_Graph <- plot_ly() %>%
  add_trace(x = ~SNPS_Date, y = ~SNPS_avg_scope1, type = 'scatter', mode = 'lines', name = 'avg_scope1') %>%
  add_trace(x = ~SNPS_Date, y = ~SNPS_avg_scope2, type = 'scatter', mode = 'lines', name = 'avg_scope2') %>%
  add_trace(x = ~SNPS_Date, y = ~SNPS_avg_first_tier, type = 'scatter', mode = 'lines', name = 'avg_first_tier') %>%
  layout(xaxis = list(title = 'Date'), yaxis = list(title = 'Value'), title = 'SNPS TruCost Graph')

SNPS_TruCost_Graph


# FCX US35671D8570
FCX_all_info <- combined_data[combined_data$ISIN_CD == 'US35671D8570',]
FCX_table <- data.frame(FCX_all_info)
FCX_Date <- FCX_table$year_month
FCX_Price <- FCX_table$avg_price

Price_Frame <- data.frame(FCX_Price, FCX_Date)
FCX_Price_Graph <- plot_ly(FCX_table, x = ~FCX_Date, y = ~FCX_Price, type = 'scatter')

FCX_avg_scope1 <- FCX_all_info$avg_scope1
FCX_avg_scope2 <- FCX_all_info$avg_scope2
FCX_avg_first_tier <- FCX_all_info$avg_first_tier

FCX_TruCost_Graph <- plot_ly() %>%
  add_trace(x = ~FCX_Date, y = ~FCX_avg_scope1, type = 'scatter', mode = 'lines', name = 'avg_scope1') %>%
  add_trace(x = ~FCX_Date, y = ~FCX_avg_scope2, type = 'scatter', mode = 'lines', name = 'avg_scope2') %>%
  add_trace(x = ~FCX_Date, y = ~FCX_avg_first_tier, type = 'scatter', mode = 'lines', name = 'avg_first_tier') %>%
  layout(xaxis = list(title = 'Date'), yaxis = list(title = 'Value'), title = 'FCX TruCost Graph')

# TXN
TXN_all_info <- combined_data[combined_data$ISIN_CD == 'US8825081040',]
TXN_table <- data.frame(TXN_all_info)

TXN_Date <- TXN_table$year_month
TXN_Price <- TXN_table$avg_price

Price_Frame <- data.frame(TXN_Price, TXN_Date)
TXN_Price_Graph <- plot_ly(TXN_table, x = ~TXN_Date, y = ~TXN_Price, type = 'scatter')

TXN_avg_scope1 <- TXN_all_info$avg_scope1
TXN_avg_scope2 <- TXN_all_info$avg_scope2
TXN_avg_first_tier <- TXN_all_info$avg_first_tier

TXN_TruCost_Graph <- plot_ly() %>%
  add_trace(x = ~TXN_Date, y = ~TXN_avg_scope1, type = 'scatter', mode = 'lines', name = 'avg_scope1') %>%
  add_trace(x = ~TXN_Date, y = ~TXN_avg_scope2, type = 'scatter', mode = 'lines', name = 'avg_scope2') %>%
  add_trace(x = ~TXN_Date, y = ~TXN_avg_first_tier, type = 'scatter', mode = 'lines', name = 'avg_first_tier') %>%
  layout(xaxis = list(title = 'Date'), yaxis = list(title = 'Value'), title = 'TXN TruCost Graph')


