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
#distinct_ticker_isinID <- Ticker_and_isinID %>% distinct()
distinct_ticker_isinID <- unique(Ticker_and_isinID)

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

#correlations_t <- distinct(correlations_with_ticker)
#unique_tickers <- unique(correlations_with_ticker$BB_TICKER_CD)
unique_tickers <- correlations_with_ticker %>% distinct(BB_TICKER_CD)

# this outputs the correlations with Tickers
my_data_unique <- correlations_with_ticker[correlations_with_ticker$BB_TICKER_CD %in% unique_tickers$BB_TICKER_CD, ]

# Use complete.cases() to identify rows with no missing values
valid_rows <- complete.cases(my_data_unique)

# Subset the original data frame to keep only valid rows
df_clean <- my_data_unique[valid_rows, ]

#df_clean_tickers <- subset(df_clean, complete.cases(df_clean[, "BB_TICKER_CD"]))

# Save the final data frame to a CSV file
#write.csv(df_clean, "TruCost_to_Price_Correlation.csv", row.names = FALSE)

# Plot graphs
for (isin in unique(combined_data$ISIN_CD)) {
  # Filter data for the specific ISIN
  isin_data <- combined_data %>%
    filter(ISIN_CD == isin)
  
  # Price and Carbon Score Plot
  price_carbon_plot <- ggplot(isin_data, aes(x = as.Date(year_month, format = "%Y-%m"))) +
    geom_line(aes(y = avg_price, color = "Price")) +
    geom_line(aes(y = avg_first_tier, color = "Carbon Intensity - Direct & First Tier Indirect")) +
    geom_line(aes(y = avg_scope1, color = "Carbon Emissions - Scope 1")) +
    geom_line(aes(y = avg_scope2, color = "Carbon Emissions - Scope 2")) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Carbon Scores")) +
    labs(
      title = paste("Price and Carbon Scores for", isin),
      x = "Year-Month",
      y = "Price",
      color = "Legend"
    )
  
  # Save plot as a PNG file
  #ggsave(paste0("price_carbon_scores_", isin, ".png"), plot = price_carbon_plot, width = 10, height = 6, units = "in")
}

