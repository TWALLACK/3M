# load libraries
library(tidyverse)
library(janitor)
library(vtable)
library(readxl)
library(texreg)
library(lubridate)
library(zoo)

# load data sets

# import assets
assets <- read_csv('data/Macrotrends - assets.csv',skip=1) %>% clean_names()
names(assets) <- c('quarter','assets_millions')
assets$assets_millions = as.numeric(gsub("[\\$,]", "",assets$assets_millions))
assets$assets_millions <- as.numeric(assets$assets_millions)

# import current_ratio
current_ratio <- read_csv('data/Macrotrends - current_ratio.csv',skip=1) %>% clean_names()
current_ratio <- rename(current_ratio, quarter=date)
current_ratio$current_assets = as.numeric(gsub("[\\$B]","",current_ratio$current_assets))
current_ratio$current_liabilities = as.numeric(gsub("[\\$B]","",current_ratio$current_liabilities))
current_ratio <- current_ratio %>% select(quarter,current_ratio)

#import revenue
revenue <- read_csv('data/Macrotrends - revenue.csv') %>% clean_names()
names(revenue) <- c('quarter','revenue_millions')
revenue$revenue_millions = as.numeric(gsub("[\\$,]", "",revenue$revenue_millions))

# combine macrotrend data
comp_data <- left_join(assets,current_ratio,by='quarter')
comp_data <- left_join(comp_data,revenue,by='quarter')
comp_data$quarter<- ymd(comp_data$quarter)

# import gdp data 
gdp <- read_csv('data/gdp.csv') %>% clean_names()
gdp$quarter <- ymd(gdp$date)-1  # in order to match other quarters
gdp <- gdp %>% select(quarter,us_gdp=gdp)
comp_data <- left_join(comp_data,gdp)

# import inflation data
inflation <- read_csv('data/inflation.csv') %>% clean_names()
inflation$quarter <- mdy(inflation$date)-1  # in order to match other quarters
inflation <- inflation %>% select(quarter,cpi)
comp_data <- left_join(comp_data,inflation)


# import s&P 500
spx <- read_csv('data/spx.csv') %>% clean_names()
spx$date<- mdy(spx$date)
spx <- spx %>% select(date,spx_close = close)

#filter quarterly numbers
spx <- spx %>%
  filter(month(spx$date) %in% c(3,6,9,12))
spx$mon_yr = format(spx$date, "%Y-%m") 
spx <- spx %>% group_by(mon_yr) %>% filter(date == max(date)) %>% ungroup()
spx <- spx %>% select(quarter=date,spx_close)
#comp_data <- left_join(comp_data,spx,by='quarter')

# import stock price
stock_price <- read_csv('data/3m_stock_price.csv') %>% clean_names()
stock_price$date <- mdy(stock_price$date)
#filter quarterly numbers
stock_price <- stock_price %>%
  filter(month(stock_price$date) %in% c(3,6,9,12))
stock_price$mon_yr = format(stock_price$date, "%Y-%m") 
stock_price <- stock_price %>% group_by(mon_yr) %>% filter(date == max(date)) %>% ungroup()
stock_price <- stock_price %>% select(quarter=date,stock_price_close=adj_close)

stock_prices <- left_join(spx,stock_price,by='quarter')
View(stock_prices)

# reformat quarter
comp_data$quarter <- as.yearqtr(comp_data$quarter, format = '%Y-%m-%d')
stock_prices$quarter <- as.yearqtr(stock_prices$quarter, format = '%Y-%m-%d')

# join stock prices and comp_data
comp_data <- left_join(comp_data,stock_prices)

# filter for time after 2010
comp_data <- comp_data %>% filter(year(quarter)>=2010)

comp_data <- comp_data %>% arrange(comp_data$quarter)
# import additional variables
more_variables <- read_csv('data/3m_additional_variables.csv') %>% clean_names()
names(more_variables)
more_variables <- more_variables %>% select(quarter,net_income=net_income_attributable_to_3m,cost_of_sales,r_d)
comp_data$quarter = as.character(comp_data$quarter)
comp_data <- left_join(comp_data,more_variables,by='quarter')
View(comp_data)
# export data
write_csv(comp_data, "data/comp_data.csv")

View(comp_data)
