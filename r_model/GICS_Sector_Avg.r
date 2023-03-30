library(zoom)
library(plotly)
library(dplyr)


# Reads in file with Price Data 
Price_Data <- read.csv(file="/Users/KatePiotrowski/Correlation_Model/Data_Input/Price_Data3_ISIN.csv")

# assigns variables for ticker and isin_ID
ticker <- Price_Data$BB_TICKER_CD
isin_ID <- Price_Data$ISIN_CD
price_date <- Price_Data$DATE_DIM_ID

# creates data frame with ticker and isin_ID
tickers_with_isin_ID <- data.frame(ticker, isin_ID, price_date)

# retrieves the Month as a new variable from the given date
tickers_with_isin_ID$Month <- format(as.Date(price_date, format="%Y%m%d"),"%m")

# retrieves the Year as a new variable from the given date
tickers_with_isin_ID$Year <- format(as.Date(price_date, format="%Y%m%d"),"%Y")

tickers_with_isin_ID$Full_Date <- as.Date(format(as.Date(price_date, format="%Y%m%d")))

tickers_with_isin_ID[order(as.Date(tickers_with_isin_ID$Date, format="%Y%m%d"))]

# determines only the distinct tickers
tickers_with_isin_ID <- distinct(tickers_with_isin_ID, .keep_all = FALSE)

# Gets the Date Range of Price Per Ticker
date_range <- tickers_with_isin_ID %>%
  group_by(ticker) %>%
  summarize(start_date = min(Full_Date), end_date = max(Full_Date))

date_range <- date_range[-1,]

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# This portion of code begins calculating the GICS Sector Average Roll-Up

# reads in resultant MSCI Correlation Data
MSCI_Correlation_Data <- read.csv(file= "/Users/KatePiotrowski/Correlation_Model/Data_Output/fixed_10_Years_Correlation_Table.csv")

# assign variables
isin_ID <- MSCI_Correlation_Data$isinID
environmental_correlation <- MSCI_Correlation_Data$environmentalPillarScore
social_correlation <- MSCI_Correlation_Data$socialPillarScore
goverance_correlation <- MSCI_Correlation_Data$governancePillarScore
ticker <- MSCI_Correlation_Data$ticker

# creates a data frame with MSCI correlations
msci_correlation <- data.frame(isin_ID, environmental_correlation, social_correlation, goverance_correlation)
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# reads in GICS Sectors csv file 
GICS_Sectors <- read.csv(file = "/Users/KatePiotrowski/Correlation_Model/Data_Input/GICS_Sectors.csv")

# assign variables
isin_ID <- GICS_Sectors$ISIN_CD
sector_level_1 <- GICS_Sectors$GICS_INDUSTRY_LVL1_NM

# creates data frame with GICS Sector information by isin_ID
gics_sector_df <- data.frame(isin_ID, sector_level_1)

# get the number of stocks per sector
number_of_stocks_per_sector <- gics_sector_df %>%
  group_by(sector_level_1) %>%
  summarise(Count = n()) %>%
  rename(String = sector_level_1)

# join together the MSCI Correlation Data and the GICS Sector data frame
joined <- merge(msci_correlation, gics_sector_df, by="isin_ID")
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# STANDARD DEVIATION
# Calculating the standard deviation by ESG Correlation Mean per ESG Factor
std_dev_by_sector <- joined %>%
  group_by(sector_level_1) %>%
  summarise(
    env_score_std_dev = sd(environmental_correlation, na.rm = TRUE),
    soc_score_std_dev = sd(social_correlation, na.rm = TRUE),
    gov_score_std_dev = sd(goverance_correlation, na.rm = TRUE)
  )

# Calculate the standard deviation for each factor (ESG)
std_dev_env <- sd(joined$environmental_correlation, na.rm = TRUE)
std_dev_soc <- sd(joined$social_correlation, na.rm = TRUE)
std_dev_gov <- sd(joined$goverance_correlation, na.rm = TRUE)

# Calculate the mean for each factor
mean_env <- mean(joined$environmental_correlation, na.rm = TRUE)
mean_soc <- mean(joined$social_correlation, na.rm = TRUE)
mean_gov <- mean(joined$goverance_correlation, na.rm = TRUE)

# Filter out correlation coefficients greater than one standard deviation for each factor
outliers_env <- joined %>% filter(abs(environmental_correlation - mean_env) > std_dev_env)
outliers_soc <- joined %>% filter(abs(social_correlation - mean_soc) > std_dev_soc)
outliers_gov <- joined %>% filter(abs(goverance_correlation - mean_gov) > std_dev_gov)

# Combine outliers into a single data frame
outliers <- bind_rows(outliers_env, outliers_soc, outliers_gov)

# Remove duplicates if any
outliers <- distinct(outliers)

outliers_with_tickers <- outliers %>% merge(tickers_with_isin_ID, by= 'isin_ID')

ordered <- c("ticker", "isin_ID", "environmental_correlation",
               "social_correlation", "goverance_correlation")

# Final Output of the List of Outliers
ordered <- outliers_with_tickers[, ordered]
ordered <- distinct(ordered, .keep_all = FALSE)

# Write the outliers to csv file
#write.csv(ordered,"/Users/KatePiotrowski/Correlation_Model/Data_Output/Outliers_GICS_Sector_Avg.csv", row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# Getting the Sector Avg WITHOUT outlier data (use this to create data mining graph)

# Getting the Environmental Sector Avg with aggregation 
no_outliers_environmental_sector_avg <- aggregate(environmental_correlation ~ sector_level_1, data = outliers, mean)

# Getting the Social Sector Avg with aggregation
no_outliers_social_sector_avg <- aggregate(social_correlation ~ sector_level_1, data = outliers, mean)

# Getting the Governance Sector Avg with aggregation
no_outliers_goverance_sector_avg <- aggregate(goverance_correlation ~ sector_level_1, data = outliers, mean)

#------------------------------------------------------------------------------------------------------------------------------
# Getting the Sector Avg WITH outliers (ALL OF THE DATA)

# Getting the Environmental Sector Avg with aggregation
environmental_sector_avg <- aggregate(environmental_correlation ~ sector_level_1, data = joined, mean)

# Getting the Social Sector Avg with aggregation
social_sector_avg <- aggregate(social_correlation ~ sector_level_1, data = joined, mean)

# Getting the Governance Sector Avg with aggregation
goverance_sector_avg <- aggregate(goverance_correlation ~ sector_level_1, data = joined, mean)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# WITH OUTLIERS (ALL OF THE DATA)
# merging each E, S, G sector avg table into one table

# first merge Environmental and Social Averages
joined_enviro_and_social <- merge(environmental_sector_avg, social_sector_avg, by="sector_level_1")

# then merge the first merge with Governance 
entire_joined_sectors_avg <- merge(joined_enviro_and_social, goverance_sector_avg, by="sector_level_1")

# write the merged data to a csv file
#write.csv(entire_joined_sectors_avg,"/Users/KatePiotrowski/Correlation_Model/Data_Output/GICS_Sector_MSCI_Correlation_Avg.csv", row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# WITHOUT OUTLIERS
# merging each E, S, G sector avg table into one table

# first merge Environmental and Social Averages without outliers
no_outliers_joined_enviro_and_social <- merge(no_outliers_environmental_sector_avg, no_outliers_social_sector_avg, by="sector_level_1")

# then merge the first merge with Governance without outliers
no_outliers_entire_joined_sectors_avg <- merge(no_outliers_joined_enviro_and_social, no_outliers_goverance_sector_avg, by="sector_level_1")
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# Join together joined and entire_joined_sectors_avg to display each Correlation by SECTOR

display_sector_avg_with_esg_correlation_per_stock <- entire_joined_sectors_avg %>% inner_join(joined, join_by(sector_level_1 == sector_level_1), multiple = "all")

# merge standard deviation value by sector into the data frame
with_std_sector_avg_with_esg_correlation_per_stock <- display_sector_avg_with_esg_correlation_per_stock %>% inner_join(std_dev_by_sector, join_by(sector_level_1 == sector_level_1), multiple = "all")


#display_sector_avg_with_esg_correlation_per_stock <- display_sector_avg_with_esg_correlation_per_stock[, c(1, 5, 2, 3, 4, 6, 7, 8)]

rename_display_sector_avg_with_esg_correlation_per_stock <- with_std_sector_avg_with_esg_correlation_per_stock %>% rename("Sector" = "sector_level_1",
                                                                     "Enviro_Corr_Sector_Avg" = "environmental_correlation.x",
                                                                     "Social_Corr_Sector_Avg" = "social_correlation.x",
                                                                     "Gov_Corr_Sector_Avg" = "goverance_correlation.x",
                                                                     "Enviro_Corr" = "environmental_correlation.y", 
                                                                     "Social_Corr" = "social_correlation.y",
                                                                     "Gov_Corr" = "goverance_correlation.y",
                                                                     "Env_Sector_Sd" = "env_score_std_dev",
                                                                     "Social_Sector_Sd" = "soc_score_std_dev",
                                                                     "Gov_Sector_Sd" = "gov_score_std_dev")

rename_display_sector_avg_with_esg_correlation_per_stock <- rename_display_sector_avg_with_esg_correlation_per_stock[, c(1, 5, 2, 3, 4, 6, 7, 8, 9, 10, 11)]
#------------------------------------------------------------------------------------------------------------------------------
# Filter out correlation coefficients greater than one standard deviation for each factor
outliers_env_by_sector <- rename_display_sector_avg_with_esg_correlation_per_stock %>% filter(abs(Enviro_Corr_Sector_Avg - Enviro_Corr) > Env_Sector_Sd)
outliers_soc_by_sector <- rename_display_sector_avg_with_esg_correlation_per_stock %>% filter(abs(Social_Corr_Sector_Avg - Social_Corr) > Social_Sector_Sd)
outliers_gov_by_sector <- rename_display_sector_avg_with_esg_correlation_per_stock %>% filter(abs(Gov_Corr_Sector_Avg - Gov_Corr) > Gov_Sector_Sd)

# Combine outliers into a single data frame
outliers_by_sector <- bind_rows(outliers_env_by_sector, outliers_soc_by_sector, outliers_gov_by_sector)

# Remove duplicates if any
outliers_by_sector <- distinct(outliers_by_sector)

# add Ticker to data frame
outliers_by_sector_with_tickers <- outliers_by_sector %>% merge(tickers_with_isin_ID, by= 'isin_ID')

# Reorder the columns
final_outliers_by_sector <- c("ticker", 
                     "isin_ID", 
                     "Sector",
                     "Enviro_Corr_Sector_Avg",
                     "Enviro_Corr", 
                     "Env_Sector_Sd",
                     "Social_Corr_Sector_Avg",
                     "Social_Corr", 
                     "Social_Sector_Sd",
                     "Gov_Corr_Sector_Avg",
                     "Gov_Corr", 
                     "Gov_Sector_Sd")

# Final Output of the List of Outliers By Sector
final_outliers_by_sector <- outliers_by_sector_with_tickers[, final_outliers_by_sector]

final_outliers_by_sector <- distinct(final_outliers_by_sector, .keep_all = FALSE)

# Merge Date Range with final_outliers_by_sector
final_outliers_by_sector_with_date_range <- merge(date_range, final_outliers_by_sector, by="ticker")

# Write to CSV File
#write.csv(final_outliers_by_sector_with_date_range,"/Users/KatePiotrowski/Correlation_Model/Data_Output/Outliers_By_GICS_Sector.csv", row.names = FALSE)






