# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(plotly)

# Read stock price data
stock_price_data <- read.csv("PriceData_2012_2022.csv")

# Convert DATE_DIM_ID to a date format and extract month and year
stock_price_data$DATE_DIM_ID <- as.Date(as.character(stock_price_data$DATE_DIM_ID), format = "%Y%m%d")
stock_price_data$year_month <- format(stock_price_data$DATE_DIM_ID, "%Y-%m")
stock_price_data$PBD_PRICE_AMT <- as.numeric(stock_price_data$PBD_PRICE_AMT)

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

# Read MSCI data
msci_data <- read.csv("MSCIData_2014_to_2018_2021_to_2022.csv")

# Convert ASOF_DATE to Date type
msci_data$ASOF_DATE <- as.Date(msci_data$ASOF_DATE, format = "%Y-%m-%d")

# Create a year_month column for MSCI data
msci_data$year_month <- format(msci_data$ASOF_DATE, "%Y-%m")

# Calculate the monthly average MSCI scores
monthly_avg_msci <- msci_data %>%
  group_by(ISSUER_ISIN, year_month) %>%
  summarise(
    avg_environmental_score = mean(ENVIRONMENTAL_PILLAR_SCORE, na.rm = TRUE),
    avg_social_score = mean(SOCIAL_PILLAR_SCORE, na.rm = TRUE),
    avg_governance_score = mean(GOVERNANCE_PILLAR_SCORE, na.rm = TRUE)
  )

# Calculate the monthly average price (previous steps)

# Join the two data frames on ISIN and year_month
combined_data <- monthly_avg_price %>%
  left_join(monthly_avg_msci, by = c("ISIN_CD" = "ISSUER_ISIN", "year_month" = "year_month"))

# Fill missing MSCI scores with the last available value
combined_data <- combined_data %>%
  group_by(ISIN_CD) %>%
  tidyr::fill(avg_environmental_score, avg_social_score, avg_governance_score, .direction = "down")

# Remove rows containing NA values
combined_data <- na.omit(combined_data)

# Calculate correlations
correlations <- combined_data %>%
  group_by(ISIN_CD) %>%
  summarise(
    environmental_score_correlation = cor(avg_price, avg_environmental_score),
    social_score_correlation = cor(avg_price, avg_social_score),
    governance_score_correlation = cor(avg_price, avg_governance_score)
  )

# Save the final dataframe to a CSV file
write.csv(correlations, "/Users/sin/Desktop/school/Senior Design/StockPriceCorrelationModel/r_model/Output/correlations_msci.csv", row.names = FALSE)
