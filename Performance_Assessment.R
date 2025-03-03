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

daily_factors_f = daily_factors[daily_factors$date >= as.Date("2024-01-02"), ] 
daily_factors_f = daily_factors_f[daily_factors_f$date <= as.Date("2024-12-31"), ] 
str(port_ret_f)
str(daily_factors_f)

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




### Calculating the Information Ratio


### Calculating the tracking error
#use quantmod to get the MSCI world dauily rerturns over the period of 2024-01-01 and 2024-12-31
#assign MSCI
getSymbols("MSCI", src = "yahoo", from = "2024-01-01", to = "2025-01-01")
# calculate normal return
MSCI_daily_return = dailyReturn(MSCI)
# multiply by 100 to get percentage
MSCI_daily_return = MSCI_daily_return * 100
# drop the last day of jensen data

# calculate tracking error
tracking_error = sd(Jensen_data$port_ex_ret - MSCI_daily_return) * sqrt(250)


