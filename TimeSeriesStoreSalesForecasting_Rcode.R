
#-------------------------------------------------------------------------------------------

##1.PACKAGES
# BASE
# ------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(data.table)
library(stringr)

# PACF - ACF
# ------------------------------------------------------
library(stats)

# DATA VISUALIZATION
# ------------------------------------------------------
library(ggpubr)
library(ggplot2)
library(plotly)


##2.IMPORTING DATA
# Import
setwd("C:/Users/User/OneDrive - Nanyang Technological University/NTU Academic/NBS 2223 S2/BR2211 Financial & Risk Analytics I S01/Project/StoreSales Project")
train <- read_csv("train.csv")
transactions <- read_csv('transactions.csv')
stores <- read_csv('stores.csv')
oil <- read_csv('oil.csv')
holidays <- read_csv('holidays_events.csv')
sub <- read_csv('sample_submission.csv')
test <- read_csv('test.csv')

# Sort transactions by store_nbr and date
transactions <- transactions %>% 
  arrange(store_nbr, date)

# Convert date to Date type
train$date <- as.Date(train$date)
test$date <- as.Date(test$date)
transactions$date <- as.Date(transactions$date)

# Convert data types
train$onpromotion <- as.numeric(train$onpromotion)
train$sales <- as.numeric(train$sales)
stores$cluster <- as.integer(stores$cluster)

#-------------------------------------------------------------------------------
##3.TRANSACTIONS
head(train)

# This feature is highly correlated with sales but first, you are supposed to sum the sales feature to find relationship. 
# Transactions means how many people came to the store or how many invoices created in a day.
# Sales gives the total sales for a product family at a particular store at a given date. 
# Fractional values are possible since products can be sold in fractional units (1.5 kg of cheese, for instance, as opposed to 1 bag of chips).
# That's why, transactions will be one of the relevant features in the model. 
# Generate new features by using transactions.

temp <- train %>%
  group_by(date, store_nbr) %>%
  summarise(total_sales = sum(sales)) %>%
  left_join(transactions, by = c("date", "store_nbr"))

cat(sprintf("Spearman Correlation between Total Sales and Transactions: %.4f", cor(temp$total_sales, temp$transactions, method = "spearman")))

transactions %>%
  arrange(store_nbr, date) %>%
  ggplot(aes(x = date, y = transactions, color = factor(store_nbr))) +
  geom_line() +
  labs(title = "Transactions")

# calculate Spearman correlation
spearman_corr <- cor.test(temp$total_sales, temp$transactions, method = "spearman")

# print the correlation coefficients
print(paste("Spearman Correlation:", spearman_corr$estimate))

##Spearman Correlation between Total Sales and Transactions: 0.8175

# There is a stable pattern in Transaction. All months are similar except December from 2013 to 2017 by boxplot. 
# In addition, we've just seen same pattern for each store in previous plot. 
# Store sales had always increased at the end of the year.

#--------------------------------------------------------------------------------

# create year_month column
transactions$year_month <- format(transactions$date, "%Y-%m")

# create a list of colors for each month
month_colors <- c("#F8766D", "#DE8B00", "#E9AB17", "#7CAE00", "#00BFC4", "#C77CFF",
                           "#F564E3", "#00A08A", "#F1C500", "#0051FF", "#8DAF94", "#AF7AC5")
                           
# create box plots for each month in each year
p <- plot_ly(transactions, x = ~year_month, y = ~transactions, color = ~format(date, "%m"),
             colors = month_colors, type = "box") %>%
  layout(title = "Transactions", xaxis = list(title = "Month and Year"), 
         yaxis = list(title = "Transactions"))

# show plot
p

# Let's take a look at transactions by using monthly average sales
# We've just learned a pattern what increases sales. It was the end of the year. 
# We can see that transactions increase in spring and decrease after spring.

#--------------------------------------------------------------------------
# aggregate monthly average transactions
transactions_monthly <- transactions %>%
  group_by(year = format(date, "%Y"), month = format(date, "%m")) %>%
  summarize(avg_transactions = mean(transactions)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# plot line chart
p <- plot_ly(transactions_monthly, x = ~date, y = ~avg_transactions, color = ~year,
             type = "scatter", mode = "lines", line = list(width = 1)) %>%
  layout(title = "Monthly Average Transactions",
         xaxis = list(title = "Date"), yaxis = list(title = "Average Transactions"))

# show plot
p

#When we look at their relationship, we can see that there is a highly correlation between total sales and transactions also.

#--------------------------------------------------------------------------------------------------------------------------------------

a <- transactions %>%
  mutate(year = lubridate::year(date),
         dayofweek = factor(lubridate::wday(date, label = TRUE), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(year, dayofweek) %>%
  summarise(transactions = mean(transactions), .groups = "drop")

ggplot(a, aes(x = dayofweek, y = transactions, group=year, color = factor(year))) +
  geom_line() +
  labs(title = "Transactions")

# The days of week is very important for shopping. It shows us a great pattern. Stores make more transactions at weekends. 
# Almost, the patterns are same from 2013 to 2017 and Saturday is the most important day for shopping.

#-------------------------------------------------------------------------------------------------------------------------------------

# The economy is one of the biggest problem for the governments and people. It affects all of things in a good or bad way. 
# In our case, Ecuador is an oil-dependent country. Changing oil prices in Ecuador will cause a variance in the model. 
# I researched Ecuador's economy to be able to understand much better and I found an article from IMF. 

# Convert the date column to a date format
oil$date <- as.Date(oil$date)

# Resample data
oil_resampled <- oil %>%
  select(date, dcoilwtico) %>%
  group_by(date) %>%
  summarise(daily_oil_price = sum(dcoilwtico, na.rm = TRUE)) %>%
  ungroup()

# Interpolate missing values
oil_resampled$daily_oil_price_interpolated <- na.approx(oil_resampled$daily_oil_price, 
                                                        na.rm = FALSE)

# Plot data
oil_plot <- oil_resampled %>%
  select(date, daily_oil_price, daily_oil_price_interpolated) %>%
  pivot_longer(cols = c("daily_oil_price", "daily_oil_price_interpolated"), 
               names_to = "Legend", 
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = Legend)) + 
  geom_line(data = . %>% filter(value != 0)) + # Exclude points with value = 0
  labs(title = "Daily Oil Price")

# Display plot
print(oil_plot)

# "Ecuador is a oil-dependent country" but is it true? Can we really see that from the data by looking at?
# First of all, let's look at the correlations for sales and transactions. The correlation values are not strong but the sign of sales is negative. 
# Maybe, we can catch a clue. Logically, if daily oil price is high, we expect that the Ecuador's economy is bad and it means the price of product increases and sales decreases. 
# There is a negative relationship here.

# Merge transactions and oil data on date column
merged_data <- merge(transactions, oil, by = "date", all.x = TRUE)

# Calculate correlation between daily oil price and transactions
correlation <- cor.test(merged_data$dcoilwtico, merged_data$transactions, method = "spearman")

# Print correlation coefficient and p-value
cat("Correlation between daily oil price and transactions:", correlation$estimate, "\n")
cat("P-value:", correlation$p.value, "\n")

# Merge train and oil data on date column
merged_data <- merge(train, oil, by = "date", all.x = TRUE)

# Calculate correlation between daily oil price and sales
correlation <- cor.test(merged_data$dcoilwtico, merged_data$sales, method = "spearman")

# Print correlation coefficient and p-value
cat("Correlation between daily oil price and sales:", correlation$estimate, "\n")
cat("P-value:", correlation$p.value, "\n")

####Not much correlation

#------------------------------------------------------------------------------------
#Next, examine daily total sales graph by family

# Calculate daily total sales for each store
a <- train %>%
  mutate(date = ymd(date)) %>%
  group_by(date, family) %>%
  summarise(sales = sum(sales), .groups = "drop")

# Create an interactive line chart
fig <- a %>%
  ggplot(aes(x = date, y = sales, color = as.factor(family))) +
  geom_line() +
  labs(title = "Daily total sales of the stores",
       x = "Date", y = "Sales", color = "Family")

# Convert the ggplot to a plotly interactive plot
fig_plotly <- ggplotly(fig)

# Show the plot
fig_plotly
#--------------------------------------------------------------
#Next, examine daily total sales graph by store

# Calculate daily total sales for each store
a <- train %>%
  mutate(date = ymd(date)) %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales), .groups = "drop")

# Create an interactive line chart
fig <- a %>%
  ggplot(aes(x = date, y = sales, color = as.factor(store_nbr))) +
  geom_line() +
  labs(title = "Daily total sales of the stores",
       x = "Date", y = "Sales", color = "Store_nbr")

# Convert the ggplot to a plotly interactive plot
fig_plotly <- ggplotly(fig)

# Show the plot
fig_plotly

# I realized some unnecessary rows in the data while I was looking at the time serie of the stores one by one. 
# If you select the stores from above, some of them have no sales at the beginning of 2013. 
# You can see them, if you look at the those stores 20, 21, 22, 29, 36, 42, 52 and 53. 
# I decided to remove those rows before the stores opened. 
# In the following codes, we will get rid of them.

# Convert the date column to a date format
train$date <- ymd(train$date)

# Print the shape of the train data frame
cat("Shape of the train data frame before removing rows:", dim(train), "\n")

# Remove unnecessary rows
train <- train[!((train$store_nbr == 52) & (train$date < "2017-04-20")), ]
train <- train[!((train$store_nbr == 22) & (train$date < "2015-10-09")), ]
train <- train[!((train$store_nbr == 42) & (train$date < "2015-08-21")), ]
train <- train[!((train$store_nbr == 21) & (train$date < "2015-07-24")), ]
train <- train[!((train$store_nbr == 29) & (train$date < "2015-03-20")), ]
train <- train[!((train$store_nbr == 20) & (train$date < "2015-02-13")), ]
train <- train[!((train$store_nbr == 53) & (train$date < "2014-05-29")), ]
train <- train[!((train$store_nbr == 36) & (train$date < "2013-05-09")), ]

# Print the shape of the train data frame after removing rows
cat("Shape of the train data frame after removing rows:", dim(train), "\n")


# Some stores don't sell some product families. 
# In the following code, can see which products aren't sold in which stores. 
# It isn't difficult to forecast them next 15 days. Their forecasts must be 0 next 15 days.
# I will remove them from the data and create a new data frame for product families which never sell. 
# Then, when we are at submission part, I will combine that data frame with our predictions.

# Group by store_nbr and family, then calculate the sum of sales
c <- train %>%
  group_by(store_nbr, family) %>%
  summarise(sales = sum(sales)) %>%
  arrange(family, store_nbr) %>%
  filter(sales == 0)

# Print the shape of the train data frame
cat("Shape of the train data frame before removing rows:", dim(train), "\n")

# Anti Join
train <- train %>%
  anti_join(c, by = c("store_nbr", "family"))

# Print the shape of the train data frame after removing rows
cat("Shape of the train data frame after removing rows:", dim(train), "\n")

# Create zero_prediction data frame
zero_prediction <- map_df(seq_len(nrow(c)), function(i) {
  data.frame(
    date = seq(ymd("2017-08-16"), ymd("2017-08-31"), by = "days"),
    store_nbr = c$store_nbr[i],
    family = c$family[i],
    sales = 0
  )
})

# Print the zero_prediction data frame
zero_prediction

#-----------------------------------------------------------------------------------------------

#We can catch the trends, seasonality and anomalies for families.

a <- train %>%
  group_by(family, date) %>%
  summarise(sales = sum(sales), .groups = 'drop') %>%
  arrange(date)

ggplot(a, aes(x = date, y = sales, color = family)) +
  geom_line() +
  labs(title = "Daily total sales of the family", x = "Date", y = "Sales", color = "Family") +
  theme_minimal()

#We are working with the stores. 
# Well, there are plenty of products in the stores and we need to know which product family sells much more? 
# Let's make a barplot to see that.
# The graph shows us GROCERY I and BEVERAGES are the top selling families.

library(ggplot2)
library(dplyr)

a <- train %>%
  group_by(family) %>%
  summarise(sales = mean(sales), .groups = 'drop') %>%
  arrange(desc(sales))

ggplot(a, aes(x = sales, y = reorder(family, sales), fill = family)) +
  geom_bar(stat = "identity") +
  labs(title = "Which product family preferred more?", x = "Sales", y = "Family", fill = "Family") +
  theme_minimal()

#Does onpromotion column cause a data leakage problem?
#It is really a good question. The Data Leakage is one of the biggest problem when we will fit a model. 

spearman_correlation <- cor(train$sales, train$onpromotion, method = "spearman")
cat("Spearman Correlation between Sales and Onpromotion:", format(spearman_correlation, digits = 4), "\n")

# How different can stores be from each other? 
# I couldn't find a major pattern among the stores actually. 
# But I only looked at a single plot. There may be some latent patterns.

d <- merge(train, stores, by = "store_nbr")
d$store_nbr <- as.integer(d$store_nbr)
d$date <- ymd(d$date) # Convert date to date format
d$year <- year(d$date)

d_by_city_year <- d %>%
  group_by(city, year) %>%
  summarize(sales = mean(sales)) %>%
  ungroup()

ggplot(data = d_by_city_year, aes(x = year, y = sales, group = city, color = city)) +
  geom_line() +
  labs(title = "Average Sales by City and Year") +
  theme_minimal()

#--------------------------------------------------------------------------------------------------
# 6. Holidays and Events
# What a mess! Probably, you are confused due to the holidays and events data. It contains a lot of information inside but, don't worry. You just need to take a breathe and think! It is a meta-data so you have to split it logically and make the data useful.
# 
# What are our problems?
# 
# Some national holidays have been transferred.
# There might be a few holidays in one day. When we merged all of data, number of rows might increase. We don't want duplicates.
# What is the scope of holidays? It can be regional or national or local. You need to split them by the scope.
# Work day issue
# Some specific events
# Creating new features etc.
# End of the section, they won't be a problem anymore!

#Transferred Holidays: Combine the transferred holidays (tr1) and their corresponding transfer days (tr2) into a single dataframe (tr).
# Read the dataset
holidays$date <- ymd(holidays$date)

# Transferred Holidays
tr1 <- holidays %>% filter(type == "Holiday" & transferred == TRUE) %>% select(-transferred)
tr2 <- holidays %>% filter(type == "Transfer") %>% select(-transferred)
tr <- bind_rows(tr1, tr2)

#Update the holidays dataframe by removing transferred holidays and transfer days, 
#then append the combined dataframe (tr).

holidays <- holidays %>%
  filter(!(transferred == FALSE & type != "Transfer")) %>%
  select(-transferred)
holidays <- bind_rows(holidays, tr)

#Further process the holidays dataframe to handle additional, bridge, and workday holidays, and then split it into events, regional, national, and local dataframes based on the locale.
# Additional Holidays
holidays$description <- str_replace_all(holidays$description, "[-+]|[0-9]+", "")
holidays$type <- ifelse(holidays$type == "Additional", "Holiday", holidays$type)

# Bridge Holidays
holidays$description <- str_replace(holidays$description, "Puente ", "")
holidays$type <- ifelse(holidays$type == "Bridge", "Holiday", holidays$type)

# Work Day Holidays
work_day <- holidays[holidays$type == "Work Day", ]
holidays <- holidays[holidays$type != "Work Day", ]

# Split
events <- holidays[holidays$type == "Event", ] %>%
  select(-type, -locale, -locale_name) %>%
  rename(events = description)
holidays <- holidays[holidays$type != "Event", ] %>% select(-type)
regional <- holidays[holidays$locale == "Regional", ] %>%
  rename(state = locale_name, holiday_regional = description) %>%
  select(-locale) %>%
  distinct()
national <- holidays[holidays$locale == "National", ] %>%
  rename(holiday_national = description) %>%
  select(-locale, -locale_name) %>%
  distinct()
local <- holidays[holidays$locale == "Local", ] %>%
  rename(holiday_local = description, city = locale_name) %>%
  select(-locale) %>%
  distinct()

#Merge the events, regional, national, and local dataframes with the main dataframe d.
d <- left_join(d, national)
d <- left_join(d, regional, by = c("date", "state"))
d <- left_join(d, local, by = c("date", "city"))

#Print the first 10 rows of the resulting dataframe d.
head(d, 10)

# Performing these preprocessing steps on holidays and events data can improve your time series forecasting by adding valuable information to your dataset. This can lead to better model performance compared to simply removing all the holidays and events.
# 
# Here's how these steps can help:
# 
# Transferred Holidays: By handling transferred holidays, you ensure that the model has the correct information on when the actual holiday took place.
# 
# Additional, Bridge, and Work Day Holidays: Processing these holidays allows the model to learn the effect of holidays that compensate for missed workdays, which might have an impact on sales.
# 
# Splitting Holidays by Locale: Splitting holidays into regional, national, and local events helps the model understand how each type of holiday might have a different effect on sales. For instance, local holidays might have a greater impact on a specific store, while national holidays would affect all stores.
# 
# Merging Holidays and Events with Main Data: By merging the processed holidays and events data with the main dataset, you provide more information for the model to learn from. This way, the model can better capture the relationship between holidays, events, and sales.
# 
# If you just remove all the holidays and events, the model may fail to capture the impact of these factors on the sales data. This could lead to a less accurate forecast, as the model wouldn't be able to account for changes in sales patterns during holidays and events. By including these features in your dataset, you give the model a better chance of understanding and predicting the impact of holidays and events on sales.

#--

  
#-------------------------------------------------------------------------------------------------  
##9. ACF & PACF for each family
# # The lag features means,shifting a time serie forward one step or more than one. So, a lag feature can use in the model to improve it. However, how many lag features should be inside the model? For understanding that, we can use ACF and PACF. The PACF is very useful to decide which features should select.
# #
# # In our problem, we have multiple time series and each time series have different pattern of course. You know that those time series consists of store-product family combinations and we have 54 stores and 33 product families. We can't examine all of them one by one. For this reason, I will look at average sales for each product but it will be store independent.
# #
# # In addition, the test data contains 15 days for each family. We should be careful when selecting lag features. We can't create new lag features from 1 lag to 15 lag. It must be starting 16.
#
library(dplyr)
library(ggplot2)
library(ggfortify)
library(forecast)

# Calculate average sales for each product family, store-independent
a <- d %>%
  filter(!is.na(sales)) %>%
  group_by(date, family) %>%
  summarize(mean_sales = mean(sales)) %>%
  ungroup()

# Get all unique product families
unique_families <- unique(a$family)

# Get the first 5 product families for ACF and PACF plots
unique_families_plot <- unique(a$family)[1:5]

# Plot ACF and PACF for the first 5 product families
for (i in unique_families_plot) {
  temp <- a %>%
    filter(family == i) %>%
    select(date, mean_sales)
  
  temp_ts <- ts(temp$mean_sales, start = c(as.numeric(format(min(temp$date), "%Y")), as.numeric(format(min(temp$date), "%j"))), frequency = 365)
  
  # ACF plot
  acf_plot <- autoplot(Acf(temp_ts, lag.max = 365), main = paste("AUTOCORRELATION -", i))
  
  # PACF plot
  pacf_plot <- autoplot(Pacf(temp_ts, lag.max = 365), main = paste("PARTIAL AUTOCORRELATION -", i))
  
  # Display plots
  gridExtra::grid.arrange(acf_plot, pacf_plot, ncol = 2)
}

# Print the summary of the auto.arima model for all families
for (i in unique_families) {
  temp <- a %>%
    filter(family == i) %>%
    select(date, mean_sales)
  
  temp_ts <- ts(temp$mean_sales, start = c(as.numeric(format(min(temp$date), "%Y")), as.numeric(format(min(temp$date), "%j"))), frequency = 365)
  
  # Fit auto.arima model
  model <- auto.arima(temp_ts)
  
  # Print the model summary
  print(paste("ARIMA Model for", i))
  print(summary(model))
}

# p=4: There are 5 non-seasonal autoregressive terms.
# d=1: There is 1 non-seasonal differencing term.
# q=1: There are 1 non-seasonal moving average terms.
#
#-----------------------------------------------------------------------------------------------


#------- FORECAST SEPARATELY --- HOLIDAYS--WEEKDAYS--WEEKENDS-------------------------------------

# Function to check if a date is a holiday
is_holiday <- function(row_index, data) {
  row <- data[row_index, ]
  return(!is.na(row$holiday_national) | !is.na(row$holiday_regional) | !is.na(row$holiday_local))
}

# Add a new column 'is_holiday' to the data frame
d$is_holiday <- sapply(1:nrow(d), is_holiday, data = d)

# Separate the data into weekdays, weekends, and holidays
weekday_df <- d %>%
  filter(!is.na(sales), !is_holiday, wday(date) %in% 2:6)  # Weekdays are from Monday (2) to Friday (6)

weekend_df <- d %>%
  filter(!is.na(sales), !is_holiday, wday(date) %in% c(1, 7))  # Weekends are Sunday (1) and Saturday (7)

holiday_df <- d %>%
  filter(!is.na(sales), is_holiday)  # Holidays

# Check the data frames
head(weekday_df)
head(weekend_df)
head(holiday_df)

# Add this line to define the 'dates' object
dates <- unique(d$date)

weekday_df<-read_csv('weekday_data.csv')
weekend_df<-read.csv('weekend_data.csv')
holiday_df<-read.csv('holiday_data.csv')

#-----------TRAINTEST SPLIT-------------------------------------

forecast_sales <- function(train_data, test_data, arima_order = c(4, 1, 2)) {
  unique_stores <- unique(train_data$store_nbr)
  unique_families <- unique(train_data$family)
  
  forecast_df <- data.frame()
  residuals_df <- data.frame()
  
  for (store in unique_stores) {
    for (family in unique_families) {
      temp_train <- train_data %>%
        filter(store_nbr == store, family == family) %>%
        select(date, sales)
      
      temp_test <- test_data %>%
        filter(store_nbr == store, family == family) %>%
        select(date, sales)
      
      temp_ts <- ts(temp_train$sales, start = c(year(min(temp_train$date)), yday(min(temp_train$date))), frequency = 365)
      
      model <- arima(temp_ts, order = arima_order)
      
      forecasts <- forecast(model, h = nrow(temp_test))
      
      temp_forecast_df <- data.frame(
        store_nbr = store,
        family = family,
        date = temp_test$date,
        forecast = as.vector(forecasts$mean)
      )
      
      temp_residuals_df <- data.frame(
        store_nbr = store,
        family = family,
        date = temp_train$date,
        residuals = as.vector(model$residuals)
      )
      
      forecast_df <- rbind(forecast_df, temp_forecast_df)
      residuals_df <- rbind(residuals_df, temp_residuals_df)
    }
  }
  
  return(list(forecast = forecast_df, residuals = residuals_df))
}


# Split the data into train and test sets
train_start_date <- as.Date("2013-01-01")
train_end_date <- as.Date("2017-07-31")
test_start_date <- as.Date("2017-08-01")
test_end_date <- as.Date("2017-08-15")

weekday_train <- weekday_df %>%
  filter(date >= train_start_date, date <= train_end_date)

weekday_test <- weekday_df %>%
  filter(date >= test_start_date, date <= test_end_date)

weekend_train <- weekend_df %>%
  filter(date >= train_start_date, date <= train_end_date)

weekend_test <- weekend_df %>%
  filter(date >= test_start_date, date <= test_end_date)

holiday_train <- holiday_df %>%
  filter(date >= train_start_date, date <= train_end_date)

holiday_test <- holiday_df %>%
  filter(date >= test_start_date, date <= test_end_date)

# Use the function to get the forecasts and residuals
weekend_result <- forecast_sales(weekend_train, weekend_test, arima_order = c(4, 1, 2))
holiday_result <- forecast_sales(holiday_train, holiday_test, arima_order = c(2, 1, 1))

# Combine the forecasts and residuals
combined_forecast <- rbind(weekday_result$forecast, weekend_result$forecast, holiday_result$forecast)
combined_residual <- rbind(weekday_result$residuals, weekend_result$residuals, holiday_result$residuals)

head(combined_forecast)
head(combined_residuals)

#------------USING-------------------START FORECASTING + GENERATE RESIDUALS----------------------------------------------------------------------------

# Function to fit and forecast sales with a specified ARIMA order
forecast_sales <- function(data, forecast_dates, arima_order = c(4, 1, 2)) {
  unique_stores <- unique(data$store_nbr)
  unique_families <- unique(data$family)
  
  forecast_df <- data.frame()
  residuals_df <- data.frame()
  
  for (store in unique_stores) {
    for (family in unique_families) {
      temp <- data %>%
        filter(store_nbr == store, family == family) %>%
        select(date, sales)
      
      temp_ts <- ts(temp$sales, start = c(as.numeric(format(min(temp$date), "%Y")), as.numeric(format(min(temp$date), "%j"))), frequency = 365)
      
      model <- arima(temp_ts, order = arima_order)
      
      forecasts <- forecast(model, h = length(forecast_dates))
      
      temp_residuals_df <- data.frame(
        store_nbr = store,
        family = family,
        date = temp$date,
        residuals = as.vector(model$residuals)
      )
      
      residuals_df <- rbind(residuals_df, temp_residuals_df)
    }
  }
  
  return(list(forecast = forecast_df, residuals = residuals_df))
}

# Create vectors for weekday, weekend, and holiday_local forecasts
forecast_start_date <- as.Date("2017-08-16")
forecast_end_date <- as.Date("2017-08-31")

all_dates <- seq(forecast_start_date, forecast_end_date, by = "day")
weekday_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday") & !(all_dates %in% as.Date("2017-08-24"))]
weekend_dates <- all_dates[weekdays(all_dates) %in% c("Saturday", "Sunday")]
holiday_local_date <- as.Date("2017-08-24")

# Use the function to get the forecasts and residuals
weekday_result <- forecast_sales(weekday_df, weekday_dates, arima_order = c(4, 1, 2))
weekend_result <- forecast_sales(weekend_df, weekend_dates, arima_order = c(4, 1, 2))
holiday_result <- forecast_sales(holiday_df, holiday_local_date, arima_order = c(2, 1, 1))

# Combine the forecasts and residuals
combined_forecast <- rbind(weekday_result$forecast, weekend_result$forecast, holiday_result$forecast)
combined_residuals <- rbind(weekday_result$residuals, weekend_result$residuals, holiday_result$residuals)

head(combined_forecast)
head(combined_residuals)

#FIND RESIDUALS-------------------------
library(forecast)

# Function to fit ARIMA model, forecast sales, and return residuals
get_residuals_for_forecasted_dates <- function(data, order, forecast_dates) {
  unique_stores <- unique(data$store_nbr)
  unique_families <- unique(data$family)
  
  all_residuals <- c()
  
  for (store in unique_stores) {
    for (family in unique_families) {
      temp <- data %>%
        filter(store_nbr == store, family == family) %>%
        select(date, sales)
      
      temp_ts <- ts(temp$sales, start = c(as.numeric(format(min(temp$date), "%Y")), as.numeric(format(min(temp$date), "%j"))), frequency = 365)
      
      model <- arima(temp_ts, order = order)
      
      # Forecast the sales for the specified dates
      h <- length(forecast_dates)
      forecasts <- forecast(model, h = h)
      
      # Calculate the residuals for the forecasted dates
      residuals <- temp_ts - fitted(model)
      all_residuals <- c(all_residuals, residuals)
    }
  }
  
  return(all_residuals)
}

forecast_start_date <- as.Date("2017-08-16")
forecast_end_date <- as.Date("2017-08-31")

all_dates <- seq(forecast_start_date, forecast_end_date, by = "day")
weekday_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday") & !(all_dates %in% as.Date("2017-08-24"))]
weekend_dates <- all_dates[weekdays(all_dates) %in% c("Saturday", "Sunday")]
holiday_date <- as.Date("2017-08-24")

# Get residuals for forecasted weekdays, weekends, and holidays
forecasted_weekday_residuals <- get_residuals_for_forecasted_dates(weekday_df, c(4, 1, 2), weekday_dates)
forecasted_weekend_residuals <- get_residuals_for_forecasted_dates(weekend_df, c(4, 1, 2), weekend_dates)
forecasted_holiday_residuals <- get_residuals_for_forecasted_dates(holiday_df, c(2, 1, 1), holiday_date)

# Combine residuals
combined_residuals <- c(forecasted_weekday_residuals, forecasted_weekend_residuals, forecasted_holiday_residuals)

write.csv(combined_residuals, "initialresiduals_data.csv", row.names = FALSE)

# Create a data frame from combined_residuals
combined_residuals_df <- data.frame(residuals = combined_residuals)

# Assuming combined_residuals is already available

# Create a data frame from combined_residuals
combined_residuals_df <- data.frame(residuals = combined_residuals)

combined_residuals_df<-read_csv('initialresiduals1_data.csv')

# Take a random sample of 10000 residuals
subset_combined_residuals <- combined_residuals_df %>%
  sample_n(size = 10000, replace = FALSE)

# ACF plot
acf(subset_combined_residuals$residuals, main = "Autocorrelation of Residuals")

# PACF plot
pacf(subset_combined_residuals$residuals, main = "Partial Autocorrelation of Residuals")

# Calculate the mean of the residuals to check if they are centered around zero:
mean(combined_residuals_df$x)

# Perform the Ljung-Box test to check for autocorrelation in the residuals:
library(lmtest)
Box.test(combined_residuals_df, lag = 10, type = "Ljung-Box")

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Select 10000 random data points from combined_residuals_df
sampled_residuals_df <- combined_residuals_df %>% 
  sample_n(10000, replace = FALSE)

# Calculate the mean and standard deviation of the residuals
mean_residuals <- mean(sampled_residuals_df$x)
sd_residuals <- sd(sampled_residuals_df$x)

# Filter the residuals that are more than 3 standard deviations away from the mean
filtered_residuals_df <- sampled_residuals_df %>% 
  filter(x >= (mean_residuals - 1 * sd_residuals) & x <= (mean_residuals + 0.5 * sd_residuals))

# Plot the QQ-plot of the filtered residuals
ggplot(filtered_residuals_df, aes(sample = x)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-Plot of Initial Model Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")




#-------------EXAMINE THE FORECASTED RESULTS BY STORE NBR -------------------------------
combined_forecast<-read_csv('separatelyWKENDHOLSforecast.csv')
library(dplyr)
library(ggplot2)

# Aggregate daily total sales by store_nbr
agg_forecast_store <- combined_forecast %>%
  group_by(date, store_nbr) %>%
  summarize(total_sales = sum(forecast)) %>%
  ungroup()

# Plot daily total sales by store_nbr
ggplot(agg_forecast_store, aes(x = date, y = total_sales, fill = as.factor(store_nbr))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Daily Total Sales by Store Number", x = "Date", y = "Total Sales") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Store Number")

#TRY SUM OF SALES 

library(dplyr)
library(ggplot2)

# Aggregate daily total sales by date
agg_forecast_date <- combined_forecast %>%
  group_by(date) %>%
  summarize(total_sales = sum(forecast)) %>%
  ungroup()

# Plot daily total sales by date
ggplot(agg_forecast_date, aes(x = date, y = total_sales)) +
  geom_line() +
  labs(title = "Daily Total Sales in Combined Forecast", x = "Date", y = "Total Sales") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#---------------------------------------------------------------------------------------------------------------------------

#----------------- COMBINE EVERYTHING AND SEE -----------------------------------
library(dplyr)
library(ggplot2)

# Aggregate daily sales in the d data frame
agg_d <- d %>%
  group_by(family, date) %>%
  summarise(sales = sum(sales), .groups = 'drop') %>%
  arrange(date)

# Aggregate daily sales in the combined_forecast data frame
agg_combined_forecast <- combined_forecast %>%
  group_by(family, date) %>%
  summarise(sales = sum(forecast), .groups = 'drop') %>%
  arrange(date)

# Combine the aggregated data frames
combined_data <- rbind(agg_d, agg_combined_forecast)

# Create the line plot for combined_data
ggplot(combined_data, aes(x = date, y = sales, color = family)) +
  geom_line() +
  labs(title = "Daily Total Sales of the Family: Actual and Forecast", x = "Date", y = "Sales", color = "Family") +
  theme_minimal()

#can't really see anything-----------so just sum of sales-------------------

library(dplyr)
library(ggplot2)

# Aggregate daily sales in the d data frame
agg_d <- d %>%
  group_by(date) %>%
  summarise(sales = sum(sales), .groups = 'drop') %>%
  arrange(date)

# Add a new column 'type' to indicate the data is actual
agg_d$type <- "Actual"

# Aggregate daily sales in the combined_forecast data frame
agg_combined_forecast <- combined_forecast %>%
  group_by(date) %>%
  summarise(sales = sum(forecast), .groups = 'drop') %>%
  arrange(date)

# Add a new column 'type' to indicate the data is forecast
agg_combined_forecast$type <- "Forecast"

# Combine the aggregated data frames
combined_data <- rbind(agg_d, agg_combined_forecast)

# Create the line plot for combined_data with different colors for Actual and Forecast
ggplot(combined_data, aes(x = date, y = sales, color = type)) +
  geom_line() +
  labs(title = "Daily Total Sales: Actual and Forecast", x = "Date", y = "Sales", color = "Type") +
  theme_minimal()

#-------APPLY LOG TRANSFORMATION----------------------------------------------------
# Log-transform the sales in the weekday, weekend, and holiday data frames
weekday_df$log_sales <- log(weekday_df$sales)
weekday_df[is.infinite(weekday_df$log_sales), "log_sales"] <- NA

weekend_df$log_sales <- log(weekend_df$sales)
weekend_df[is.infinite(weekend_df$log_sales), "log_sales"] <- NA

holiday_df$log_sales <- log(holiday_df$sales)
holiday_df[is.infinite(holiday_df$log_sales), "log_sales"] <- NA

# Function to forecast log-transformed sales and return both forecasts and residuals
forecast_log_sales <- function(data, order, forecast_dates) {
  unique_stores <- unique(data$store_nbr)
  unique_families <- unique(data$family)
  
  all_forecasts <- list()
  
  for (store in unique_stores) {
    for (family in unique_families) {
      temp <- data %>%
        filter(store_nbr == store, family == family) %>%
        dplyr::select(date, log_sales) %>%
        na.omit()
      
      temp_ts <- ts(temp$log_sales, start = c(as.numeric(format(min(temp$date), "%Y")), as.numeric(format(min(temp$date), "%j"))), frequency = 365)
      
      model <- arima(temp_ts, order = order)
      
      # Forecast the log-transformed sales for the specified dates
      h <- length(forecast_dates)
      forecasts <- forecast(model, h = h)
      
      # Store the forecasts in the all_forecasts list
      store_family_name <- paste0("store_", store, "_family_", family)
      forecasts_df <- data.frame(date = forecast_dates, store_nbr = store, family = family, sales_forecast = exp(forecasts$mean))
      all_forecasts[[store_family_name]] <- forecasts_df
    }
  }
  
  return(do.call(rbind, all_forecasts))
}

# Get the forecasts for log-transformed weekdays, weekends, and holidays
forecasted_weekday_log_results <- forecast_log_sales(weekday_df, c(2, 1, 1), weekday_dates)
forecasted_weekend_log_results <- forecast_log_sales(weekend_df, c(2, 1, 1), weekend_dates)
forecasted_holiday_log_results <- forecast_log_sales(holiday_df, c(1, 1, 1), holiday_date)

# Combine the forecasts into one data frame
all_forecasts_df <- do.call(rbind, list(forecasted_weekday_log_results, forecasted_weekend_log_results, forecasted_holiday_log_results))

# Save the combined forecasts data frame to a CSV file
write.csv(all_forecasts_df, "all_forecasts_data.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------
combined_new_residuals <- c(forecasted_weekday_log_results$residuals, forecasted_weekend_log_results$residuals, forecasted_holiday_log_results$residuals)

# Save combined forecasts and residuals to CSV files
write.csv(combined_new_forecasts, "combined_new_forecasts.csv", row.names = FALSE)
write.csv(combined_new_residuals, "combined_new_residuals.csv", row.names = FALSE)

combined_new_residuals<-read_csv('logtransformed_residuals_data.csv')         
combined_new_forecasts<-read_csv('combined_new_forecasts.csv')

#----------------CHECK RESIDUALS----------------------------------------------------------------------------
# Plot ACF and PACF of residuals
acf(combined_new_residuals, main = "ACF of Combined Residuals")
pacf(combined_new_residuals, main = "PACF of Combined Residuals")


# QQ plot of residuals
# Set the seed for reproducibility
set.seed(123)

# Check the number of residuals in combined_new_residuals
num_residuals <- nrow(combined_new_residuals)
cat("Number of residuals:", num_residuals, "\n")

# Select a random sample of residuals
sample_size <- min(100000, num_residuals)
sample_residuals <- combined_new_residuals$x[sample(1:num_residuals, sample_size, replace = TRUE)]

# Generate QQ plot for the sample residuals
qqnorm(sample_residuals, main = "QQ Plot of Sample Residuals")
qqline(sample_residuals, col = "blue")

###### Create a histogram for the sample residuals
hist(sample_residuals, main = "Histogram of Sample Residuals", xlab = "Residuals", col = "lightblue", border = "black")

#Calculate the mean of the residuals to check if they are centered around zero:
mean(combined_new_residuals$x)
#-0.008462

# Perform Ljung-Box test on the residuals
lag <- 8  # Choose a suitable lag value based on your dataset
ljung_box_test <- Box.test(combined_new_residuals$x, lag = lag, type = "Ljung-Box")

# Display the results
print(ljung_box_test)

#To do: Generate graphs after log transformation
#Go back to initial forecast to form QQPlot 
