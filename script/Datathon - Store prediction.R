# Load libraries ####
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
# require() gives a warning message and returns FALSE if the requested package is not found
# load al the necessary libraries with pacman
pacman::p_load(tidyverse,
               lubridate,
               ggplot2,
               ggfortify,
               scales,
               forecast,
               plotly,
               reshape,
               prophet,
               xts,
               data.table,
               imputeTS # used to replace na's in time series data
)



# Load data ####
df_train <- read.csv('data/train.csv')
df_results <- read.csv('data/test.csv')
df_sub <- read.csv('data/sample_submission.csv')

# Make a copy of df_train to work with
df <- df_train



# Data preprocessing and exploration ####

# Change the date to a POSIXct format
df$date <- as.POSIXct(df$date, "%Y-%m-%d")
df_results$date <- as.POSIXct(df_results$date, "%Y-%m-%d")

# Group data monthly and weekly by store and by item
df$year <- year(df$date)
df$month <- month(df$date)
df$week <- week(df$date)
df_month_store <- df %>%
  group_by(year, month, store) %>%
  summarize(date = min(date), sales = sum(sales))
df_week_store <- df %>%
  group_by(year, week, store) %>%
  summarize(date = min(date), sales = sum(sales))
df_month_item <- df %>%
  group_by(year, month, item) %>%
  summarize(date = min(date), sales = sum(sales))
df_week_item <- df %>%
  group_by(year, week, item) %>%
  summarize(date = min(date), sales = sum(sales))

# Plot the grouped data
ggplot(df_month_store, aes(x=date,y=sales)) + geom_line(aes(col=as.factor(store)))
ggplot(df_week_store, aes(x=date,y=sales)) + geom_line(aes(col=as.factor(store)))
ggplot(df_month_item, aes(x=date,y=sales)) + geom_line(aes(col=as.factor(item)))
ggplot(df_week_item, aes(x=date,y=sales)) + geom_line(aes(col=as.factor(item)))

# The items and the stores seem to have the same behavior in time, thus a single model can be found and applied to every combination of item and store



# Searching for a model ####

# Create daily, weekly and monthly data ignoring the items and stores
df_daily <- df %>%
  group_by(date) %>%
  summarize(sales = sum(sales))
df_weekly <- df %>%
  group_by(year,week) %>%
  summarize(date=min(date), sales = sum(sales))
df_monthly <- df %>%
  group_by(year,month) %>%
  summarize(date=min(date), sales = sum(sales))

# Plot the data
ggplot(df_daily, aes(x=date,y=sales)) + geom_line()
ggplot(df_weekly, aes(x=date,y=sales)) + geom_line()
ggplot(df_monthly, aes(x=date,y=sales)) + geom_line()



# Forecasting with prophet ####

# Daily analysis train-test
df_prophet_dayly_train <- df_daily %>% filter (date < "2017-01-01")
df_prophet_dayly_test <- df_daily %>% filter (date >= "2017-01-01")
colnames(df_prophet_dayly_train) <- c('ds','y')
colnames(df_prophet_dayly_test) <- c('ds','y')
m <- prophet(df_prophet_dayly_train,daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 365,freq = 'day')
forecast <- predict(m, future)
#plot(forecast$yhat)
plot(m, forecast) + 
  ggtitle('Daily prediction') +
  xlab('Date') +
  ylab('Sales')
prophet_plot_components(m, forecast)

MAPE_prophet_daily <- mean(abs(forecast$yhat[forecast$ds >= "2017-01-01"] - df_prophet_dayly_test$y) / df_prophet_dayly_test$y * 100)
MAPE_prophet_daily

# Daily forecasting for 2018




df_prophet_to_split <- df_train
df_prophet_to_split$date <- as.POSIXct(df_prophet_to_split$date, "%Y-%m-%d")
names(df_prophet_to_split) <- c('ds','store','item','y')
# LOG
df_prophet_to_split$y <- log10(1+df_prophet_to_split$y)
# LOG
df_split <- split(as.data.table(df_prophet_to_split),by=c('store','item'),keep.by = FALSE)

makepredictions <- function(df_prophet)
{
  m <- prophet(df_prophet, daily.seasonality=TRUE)
  future <- make_future_dataframe(m, periods=90)
  forecast <- predict(m, future)
  print(i)
  i <<- i + 1
  xts::last(forecast[, c("yhat")],90)
}

start_time <- Sys.time()
i <- 1
logprediction <- sapply(df_split,makepredictions)
end_time <- Sys.time()
end_time - start_time

# LOG
logprediction <- 10^logprediction - 1
df_sub$sales <- as.numeric(logprediction)
write.csv(df_sub, "data/submission2.csv", row.names=FALSE)
# LOG
