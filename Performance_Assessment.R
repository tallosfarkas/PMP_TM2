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

