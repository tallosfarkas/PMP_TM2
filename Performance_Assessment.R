library(pmp.engine)
library(lubridate)
library(Rblpapi)
library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(imputeTS)
library(zoo)

blpConnect()

######################
### Jensen´s Alpha ###
######################

### Creating the dataset
port_ret = readxl::read_xlsx("C:/Users/Student2.AzureAD/ZZ Vermögensverwaltung GmbH/ISK-Wien - General/ZZ Gruppe/2024/Personal/Farkas/PMP_TM2/portfolio_returns.xlsx", sheet = 1)
port_ret_f <- port_ret[port_ret$date >= as.Date("2024-01-02"), ]
port_ret_f <- port_ret_f[port_ret_f$date <= as.Date("2024-12-31"), ]
port_ret_f$date = as.Date(port_ret_f$date)

daily_factors_f = dailyFactors[dailyFactors$date >= as.Date("2024-01-02"), ] 
daily_factors_f = daily_factors_f[daily_factors_f$date <= as.Date("2024-12-31"), ] 
str(port_ret_f)
str(daily_factors_f)
#rename daily_factor_f date column to small d date
names(daily_factors_f)[1] = "date"


Jensen_data <- merge(port_ret_f, daily_factors_f, by = "date")
summary(Jensen_data)
str(Jensen_data)
Jensen_data$Portfolio_return = as.numeric(Jensen_data$Portfolio_return)
Jensen_data$DTB3 = NULL

### Dropping NAs
Jensen_data = na.omit(Jensen_data)

### Calculating excess returns
Jensen_data$port_ex_ret = Jensen_data$Portfolio_return - Jensen_data$RF
Jensen_data$port_ex_ret = Jensen_data$port_ex_ret * 100 

### Calculating the portfolio beta
model_1 = lm(port_ex_ret ~ Mkt.RF + SMB + HML + WML, data = Jensen_data)
summary(model_1)

portfolio_beta = model_1$coefficients[2]
Jensen_data$Portfolio_beta = portfolio_beta 


### Calculating Jensen´s Alpha
Jensen_data$Jensen_Alpha = Jensen_data$Portfolio_return - Jensen_data$RF - Jensen_data$Portfolio_beta*Jensen_data$Mkt.RF
plot(Jensen_data$Jensen_Alpha, type = "l", col = "blue", lwd = 2, xlab = "Date", ylab = "Jensen Alpha", main = "Jensen Alpha for 2024")


### Fama French "4" Alpha
portfolio_alpha = model_1$coefficients[1]
portfolio_alpha


### Calculating the Sharpe Ratio
US_1_Y_RF = 4.784

portfolio_ret_1_Y = (Jensen_data$Portfolio_value[250]/Jensen_data$Portfolio_value[1] - 1)*100
portfolio_ret_1_Y

portfolio_ex_ret_1_Y = portfolio_ret_1_Y - US_1_Y_RF
portfolio_ex_ret_1_Y

st_dev_ex_ret = sd(Jensen_data$port_ex_ret) * sqrt(250) 
st_dev_ex_ret

Sharpe_ratio = portfolio_ex_ret_1_Y/st_dev_ex_ret
Sharpe_ratio


### Calculating the Treynor ratio
Treynor_ratio = portfolio_ex_ret_1_Y/portfolio_beta
Treynor_ratio








### Calculating the tracking error
#use quantmod to get the MSCI world dauily rerturns over the period of 2024-01-01 and 2024-12-31
#assign MSCI
getSymbols("MSCI", src = "yahoo", from = "2024-01-01", to = "2025-01-01")
# calculate normal return
MSCI_daily_return = dailyReturn(MSCI)
# multiply by 100 to get percentage
MSCI_daily_return = MSCI_daily_return * 100
# drop the last day of jensen data
# use the dates of jensen data bc MSCI is longer, so they dopnt mastch for st dev
MSCI_daily_return = MSCI_daily_return[1:dim(Jensen_data)[1]]
# MSCI overal 1Y return from xts object 
MSCI_ret_1_Y <- (598.1229/547.6862 - 1)*100
MSCI_ret_1_Y
# calculate tracking error
tracking_error = sd(Jensen_data$Portfolio_return - MSCI_daily_return) * sqrt(250)
tracking_error

### Calculating the Information Ratio
information_ratio = (portfolio_ret_1_Y - MSCI_ret_1_Y)/tracking_error
information_ratio


# export the daily poprtfolio return to a .rda file
save(Jensen_data, file = "C:/Users/Student2.AzureAD/ZZ Vermögensverwaltung GmbH/ISK-Wien - General/ZZ Gruppe/2024/Personal/Farkas/PMP_TM2/Jensen_data.rda")

cleanPortfolioDT
head(cleanPortfolioDT)
#colnames of cleanPortfolioDT
colnames(cleanPortfolioDT)
# > colnames(cleanPortfolioDT)
# [1] "date"      "aqr.TRI"   "aqr.PX"    "brk.TRI"   "brk.PX"    "spdr.TRI"  "spdr.PX"   "t30.TRI"   "t30.PX"   
# [10] "t25.TRI"   "t25.PX"    "t50.TRI"   "t50.PX"    "wexus.TRI" "wexus.PX"  "usgvt.TRI" "usgvt.PX" 



# --- Additional Code for cleanPortfolioDT ------------------------------

library(quantmod)
library(dplyr)
library(lubridate)
library(xts)

# Merge cleanPortfolioDT with daily factor data (assumed in dailyFactors)
data_merged <- merge(cleanPortfolioDT, dailyFactors, by = "date")

# clean the NAs
data_merged <- data_merged[complete.cases(data_merged), ]

# Function to compute daily return (percentage change)
calc_daily_return <- function(index_series) {
  c(NA, diff(index_series) / head(index_series, -1) * 100)
}

# Identify TRI columns (e.g., ending in ".TRI")
tri_cols <- names(data_merged)[ grepl("\\.TRI$", names(data_merged)) ]

# Retrieve benchmark (MSCI) returns for tracking error / info ratio.
# Adjust symbol and dates as needed.
getSymbols("MSCI", src = "yahoo", from = min(data_merged$date), to = max(data_merged$date))
MSCI_index <- MSCI
MSCI_daily_ret <- dailyReturn(MSCI_index) * 100
# Align benchmark dates with our merged data
bench_dates <- index(MSCI_daily_ret)
MSCI_daily_ret <- MSCI_daily_ret[bench_dates %in% data_merged$date]
MSCI_close <- Cl(MSCI_index)
MSCI_annual_ret <- (as.numeric(last(MSCI_close)) / as.numeric(first(MSCI_close)) - 1) * 100

# Risk-free rate for annual metrics (as used previously)
US_1_Y_RF <- 4.784

# Loop over each TRI column to calculate performance metrics
results_list <- lapply(tri_cols, function(fund) {
  
  # Calculate daily returns for the fund
  fund_return <- calc_daily_return(data_merged[[fund]])
  
  temp_df <<- data_merged %>%
    mutate(fund_return = fund_return) %>%
    filter(!is.na(fund_return)) %>%
    filter(date %in% bench_dates)  # Align with benchmark dates
  
  # Excess return: fund return minus risk-free rate
  temp_df <<- temp_df %>% mutate(excess_return = fund_return - RF)
  
  # Multifactor regression (Fama-French + momentum)
  model <- lm(excess_return ~ `Mkt.RF` + SMB + HML + WML, data = temp_df)
  beta <- coef(model)["Mkt.RF"]
  multifactor_alpha <- coef(model)[1]
  
  # Daily Jensen's alpha = fund_return - RF - beta * Mkt.RF
  temp_df <<- temp_df %>% mutate(Jensen_Alpha = fund_return - RF - beta * `Mkt.RF`)
  avg_jensen_alpha <- mean(temp_df$Jensen_Alpha)
  
  # Annualized return from TRI index (using first and last values on matching dates)
  fund_series <- data_merged[[fund]][data_merged$date %in% temp_df$date]
  ann_return <- ( tail(fund_series, 1) / head(fund_series, 1) - 1 ) * 100
  ann_excess_return <- ann_return - US_1_Y_RF
  
  # Annualized standard deviation of daily excess returns
  ann_stdev_excess <- sd(temp_df$excess_return) * sqrt(250)
  
  Sharpe_ratio <- ann_excess_return / ann_stdev_excess
  Treynor_ratio <- ann_excess_return / beta
  
  # Align benchmark returns for tracking error calculation
  bench_ret <- MSCI_daily_ret[as.character(temp_df$date)]
  bench_ret_num <- as.numeric(bench_ret)
  tracking_error <- sd(temp_df$fund_return - bench_ret_num) * sqrt(250)
  
  information_ratio <- (ann_return - MSCI_annual_ret) / tracking_error
  
  # Return metrics for the current fund
  data.frame(
    Fund = fund,
    Beta = beta,
    JensenAlpha = avg_jensen_alpha,
    MultifactorAlpha = multifactor_alpha,
    SharpeRatio = Sharpe_ratio,
    TreynorRatio = Treynor_ratio,
    TrackingError = tracking_error,
    InformationRatio = information_ratio,
    AnnualReturn = ann_return,
    stringsAsFactors = FALSE
  )
})

# Combine results into one data frame
results_df <- do.call(rbind, results_list)
print(results_df)

