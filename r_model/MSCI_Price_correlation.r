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

# Select the top 5 and bottom 5 correlation scores for each ESG factor
top_bottom_environmental <- correlations %>%
  arrange(environmental_score_correlation) %>%
  slice(c(1:5, (n() - 4):n()))

top_bottom_social <- correlations %>%
  arrange(social_score_correlation) %>%
  slice(c(1:5, (n() - 4):n()))

top_bottom_governance <- correlations %>%
  arrange(governance_score_correlation) %>%
  slice(c(1:5, (n() - 4):n()))

top_bottom_correlations <- bind_rows(top_bottom_environmental, top_bottom_social, top_bottom_governance)

# Merge the combined_data with the selected top and bottom correlations
selected_data <- combined_data %>%
  inner_join(top_bottom_correlations, by = c("ISIN_CD"))

# Normalize price and ESG scores
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
selected_data$normalized_price <- normalize(selected_data$avg_price)
selected_data$normalized_environmental_score <- normalize(selected_data$avg_environmental_score)
selected_data$normalized_social_score <- normalize(selected_data$avg_social_score)
selected_data$normalized_governance_score <- normalize(selected_data$avg_governance_score)

# Plot graphs for each stock and ESG factor
esg_factors <- c("normalized_environmental_score", "normalized_social_score", "normalized_governance_score")
esg_factors_original <- c("avg_environmental_score", "avg_social_score", "avg_governance_score")
esg_factor_names <- c("environmental", "social", "governance")

for (isin in unique(selected_data$ISIN_CD)) {
  stock_data <- selected_data[selected_data$ISIN_CD == isin, ]
  for (i in 1:length(esg_factors)) {
    factor <- esg_factors[i]
    factor_original <- esg_factors_original[i]
    factor_name <- esg_factor_names[i]
    
    # Get the correlation score
    correlation_score <- round(
      top_bottom_correlations[top_bottom_correlations$ISIN_CD == isin, paste0(factor_name, "_score_correlation")],
      3
    )
    
    # Create the plot
    plot <- plotly::plot_ly(stock_data) %>%
      plotly::add_lines(x = ~year_month, y = ~normalized_price, name = "Normalized Price", line = list(color = "blue")) %>%
      plotly::add_lines(x = ~year_month, y = ~get(factor), name = paste0("Normalized ", factor_name, " Score"), line = list(color = "red")) %>%
      plotly::layout(
        title = paste0("Stock Price and ", factor_name, " Score Correlation for ", stock_data$BB_TICKER_CD[1], " (", isin, "): ", correlation_score),
        xaxis = list(title = "Year-Month"),
        yaxis = list(title = "Normalized Value")
      )
    
    # Print the plot
    print(plot)
    
  }
}
