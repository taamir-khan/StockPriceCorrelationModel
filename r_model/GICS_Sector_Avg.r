library(zoom)
library(plotly)

# GICS AVERAGE

# reads in resultant MSCI Correlation Data
MSCI_Correlation_Data <- read.csv(file= "/Users/sin/Desktop/school/Senior Design/StockPriceCorrelationModel/r_model/Output/fixed_10_Years_Correlation_Table.csv")

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
GICS_Sectors <- read.csv(file = "/Users/sin/Desktop/school/Senior Design/StockPriceCorrelationModel/r_model/Data/GICS_Sectors.csv")

# assign variables
isin_ID <- GICS_Sectors$ISIN_CD
sector_level_1 <- GICS_Sectors$GICS_INDUSTRY_LVL1_NM

# creates data frame with GICS Sector information by isin_ID
gics_sector_df <- data.frame(isin_ID, sector_level_1)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# join "msci_correlation" and "gics_sector_df" by "gics_isin_ID" 

joined <- merge(msci_correlation, gics_sector_df, by="isin_ID")

# Getting the Environmental Sector Avg with aggregation
environmental_sector_avg <- aggregate(environmental_correlation ~ sector_level_1, data = joined, mean)

# Getting the Social Sector Avg with aggregation
social_sector_avg <- aggregate(social_correlation ~ sector_level_1, data = joined, mean)

# Getting the Governance Sector Avg with aggregation
goverance_sector_avg <- aggregate(goverance_correlation ~ sector_level_1, data = joined, mean)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# merging each E, S, G sector avg table into one table

# first merge Enviromental and Social Averages
joined_enviro_and_social <- merge(environmental_sector_avg, social_sector_avg, by="sector_level_1")

# then merge the first merge with Governance 
entire_joined_sectors_avg <- merge(joined_enviro_and_social, goverance_sector_avg, by="sector_level_1")

# write the merged data to a csv file
#write.csv(entire_joined_sectors_avg,"/Users/KatePiotrowski/Correlation_Model/Data_Output/GICS_Sector_MSCI_Correlation_Avg.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
# create bar graph

fig <- plot_ly(entire_joined_sectors_avg, x = ~sector_level_1, y = ~environmental_correlation, type = 'bar', name = 'environmental', color = 'light green')
fig <- fig %>% add_trace(y = ~social_correlation, name = 'Social', color = 'light yellow')
fig <- fig %>% add_trace(y = ~social_correlation, name = 'Governance', color = 'light blue')
fig <- fig %>% layout(yaxis = list(title = 'Correlation Coefficient'), barmode = 'stack')

fig




