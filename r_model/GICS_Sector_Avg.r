library(zoom)
library(plotly)
library(dplyr)

# GICS AVERAGE

# reads in resultant MSCI Correlation Data
MSCI_Correlation_Data <- read.csv(file= "/Users/KatePiotrowski/Correlation_Model/Data_Output/fixed_10_Years_Correlation_Table.csv")

# assign variables
isin_ID <- MSCI_Correlation_Data$isinID
environmental_correlation <- MSCI_Correlation_Data$environmentalPillarScore
social_correlation <- MSCI_Correlation_Data$socialPillarScore
goverance_correlation <- MSCI_Correlation_Data$governancePillarScore

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


#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# join "msci_correlation" and "gics_sector_df" by "gics_isin_ID" 

joined <- merge(msci_correlation, gics_sector_df, by="isin_ID")

#------------------------------------------------------------------------------------------------------------------------------
# STANDARD DEVIATION
# Calculating the standard deviation by sector for each score
std_dev_by_sector <- joined %>%
  group_by(sector_level_1) %>%
  summarise(
    env_score_std_dev = sd(environmental_correlation, na.rm = TRUE),
    soc_score_std_dev = sd(social_correlation, na.rm = TRUE),
    gov_score_std_dev = sd(goverance_correlation, na.rm = TRUE)
  )


# Calculate mean and standard deviation by sector for each score
score_stats_by_sector <- joined %>%
  group_by(sector_level_1) %>%
  summarise(
    env_score_mean = mean(environmental_correlation, na.rm = TRUE),
    env_score_std_dev = sd(environmental_correlation, na.rm = TRUE),
    soc_score_mean = mean(social_correlation, na.rm = TRUE),
    soc_score_std_dev = sd(social_correlation, na.rm = TRUE),
    gov_score_mean = mean(goverance_correlation, na.rm = TRUE),
    gov_score_std_dev = sd(goverance_correlation, na.rm = TRUE)
  )

# Calculate z-scores for each score and filter outliers
outliers <- joined %>%
  left_join(score_stats_by_sector, by = "sector_level_1") %>%
  mutate(
    env_score_z = (environmental_correlation - env_score_mean) / env_score_std_dev,
    soc_score_z = (social_correlation - soc_score_mean) / soc_score_std_dev,
    gov_score_z = (goverance_correlation - gov_score_mean) / gov_score_std_dev
  ) %>%
  filter(
    (env_score_z >= -1 & env_score_z <= 1) &
      (soc_score_z >= -1 & soc_score_z <= 1) &
      (gov_score_z >= -1 & gov_score_z <= 1)
  ) %>%
  select(-contains("mean"), -contains("std_dev"), -contains("z"))
#------------------------------------------------------------------------------------------------------------------------------
# Getting the Sector Avg WITHOUT outlier data

# Getting the Environmental Sector Avg with aggregation 
no_outliers_environmental_sector_avg <- aggregate(environmental_correlation ~ sector_level_1, data = outliers, mean)

# Getting the Social Sector Avg with aggregation
no_outliers_social_sector_avg <- aggregate(social_correlation ~ sector_level_1, data = outliers, mean)

# Getting the Governance Sector Avg with aggregation
no_outliers_goverance_sector_avg <- aggregate(goverance_correlation ~ sector_level_1, data = outliers, mean)

#------------------------------------------------------------------------------------------------------------------------------

# Getting the Environmental Sector Avg with aggregation
environmental_sector_avg <- aggregate(environmental_correlation ~ sector_level_1, data = joined, mean)

# Getting the Social Sector Avg with aggregation
social_sector_avg <- aggregate(social_correlation ~ sector_level_1, data = joined, mean)

# Getting the Governance Sector Avg with aggregation
goverance_sector_avg <- aggregate(goverance_correlation ~ sector_level_1, data = joined, mean)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
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
# create bar graph

# ENVIRO GRAPH
jpeg(file="testing_E_graph.jpeg")


#g <- ggplot(mpg, aes(class))  
#p <-  g + geom_bar()

#ggplotly(p)

g <- ggplot(environmental_sector_avg, aes(sector_level_1))  
p <-  g + geom_bar()

ggplotly(p)

e_graph <- plot_ly(environmental_sector_avg$environmental_correlation,
        main = "ENVIRONMENTAL: GICS Sector Avg of MSCI to Price Correlations",
        xlab = "Sector",
        ylab = "Average Correlation",
        names.arg = c("Communication Services", 
                      "Consumer Discretionary", 
                      "Consumer Staples", 
                      "Energy", 
                      "Financials", 
                      "Health Care",
                      "Industrials",
                      "Information Technology",
                      "Materials",
                      "Real Estate",
                      "Utilities"),
        col = "light green")

dev.off()


# create bar graph

fig <- plot_ly(entire_joined_sectors_avg, x = ~sector_level_1, y = ~environmental_correlation, type = 'bar', name = 'environmental', color = 'light green')
fig <- fig %>% add_trace(y = ~social_correlation, name = 'Social', color = 'light yellow')
fig <- fig %>% add_trace(y = ~social_correlation, name = 'Governance', color = 'light blue')
fig <- fig %>% layout(yaxis = list(title = 'Correlation Coefficient'), barmode = 'stack')

fig


