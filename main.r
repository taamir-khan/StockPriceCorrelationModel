# reads csv file
data <- read.csv("/Users/KatePiotrowski/Height_Model_R/JPM_Practice.csv")

Name <- data$Name
Date <- data$Date2
Price <- data$Price
y1 <- data$y1

# set-up data frame with name, date, price, y1
df <- data.frame(Name, Date, Price, y1)

df$Month <- format(as.Date(Date, format="%Y-%m-%d"),"%m")
df$Year <- format(as.Date(Date, format="%Y-%m-%d"),"%Y")

df[order(as.Date(df$date, format="%Y-%m-%d"))]
#df

aggregate(y1~Month+Year,df,mean)

cor(df$Price, df$y1,
    method = "spearman"
)
