# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)

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

# Read TruCost data
trucost_data <- read.csv("truCost.csv")

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

# Count data points before backfilling
count_before <- combined_data %>%
  group_by(ISIN_CD) %>%
  summarise(
    data_points_before = sum(!is.na(avg_scope1))
  )

# Fill missing carbon scores with the last available value
combined_data <- combined_data %>%
  group_by(ISIN_CD) %>%
  tidyr::fill(avg_scope1, avg_scope2, avg_first_tier, .direction = "down")

# Count data points after backfilling
count_after <- combined_data %>%
  group_by(ISIN_CD) %>%
  summarise(
    data_points_after = sum(!is.na(avg_scope1))
  )

# Combine count data
data_point_counts <- count_before %>%
  inner_join(count_after, by = "ISIN_CD")

# Remove rows with 0 values in data_point_counts
data_point_counts <- data_point_counts %>%
  filter(
    data_points_before != 0,
    data_points_after != 0
  )


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

# Save the final dataframe to a CSV file
write.csv(correlations, "/Users/sin/Desktop/school/Senior Design/StockPriceCorrelationModel/r_model/Output/truCostCorr.csv", row.names = FALSE)

# Merge the data_point_counts with the correlations
merged_data <- left_join(correlations, data_point_counts, by = "ISIN_CD")

# Reorder columns to move the data points columns after the ISIN_CD column
merged_data <- merged_data %>%
  select(ISIN_CD,
         data_points_before,
         data_points_after,
         everything())

# Save the merged data to a CSV file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

                  
