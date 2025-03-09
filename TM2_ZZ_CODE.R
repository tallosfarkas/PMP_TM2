#### DATA PREPARATION ####




# ---- 0. Preliminaries: Load Libraries & Connect to Bloomberg ----

# Load required libraries
library(pmp.engine)
library(lubridate)
library(Rblpapi)
library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(imputeTS)
library(zoo)

# Connect to Bloomberg (ensure the Bloomberg Terminal is open)
Rblpapi::blpConnect()


# ---- 1. Load Saved Workspace & Portfolio Data ----

# Load saved workspace data containing:
#   - assetdes: Asset descriptions
#   - DT: Portfolio-related data
load("tm2.RData")  

# Inspect the loaded objects
print(assetdes)
head(DT)
tail(DT)
summary(DT)

# Download additional portfolio data from Bloomberg using a custom function.
# The function returns a list with portfolio prices and asset identifiers.
portfolioData    <- download_port_bbg(start_date = Sys.Date() - years(11))
portfolioPrices  <- portfolioData$prices  # Price time series for portfolio assets
portfolioIDs     <- portfolioData$id      # Asset identifiers


# ---- 2. Download Benchmark Data from Bloomberg ----

# Define date range for benchmark data
benchmarkStartDate <- as.Date("2014-12-31")
benchmarkEndDate   <- Sys.Date()

# Define benchmark tickers and fields (closing prices)
benchmarkTickers <- c("SPX Index", "MXWOU Index", "LUATTRUU Index")
benchmarkFields  <- c("PX_LAST")  

# Download benchmark data for each ticker using bdh()
benchmarkData <- lapply(benchmarkTickers, function(ticker) {
  bdh(ticker, benchmarkFields, benchmarkStartDate, benchmarkEndDate)
})
names(benchmarkData) <- benchmarkTickers

# Inspect a sample of the SPX Index data
head(benchmarkData[["SPX Index"]])


# ---- 3. Download U.S. T-Bill Rates from FRED ----

# Get 3-month Treasury bill rates ("DTB3") from FRED
getSymbols("DTB3", src = "FRED", from = benchmarkStartDate, to = benchmarkEndDate)

# Convert the DTB3 xts object to a data frame and rename the column
tbillData <- data.frame(Date = index(DTB3), TBillRate = coredata(DTB3))
head(tbillData)


# ---- 4. Download and Clean Fama-French Factor Data ----

## 4a. Fama-French Factors (Market Excess Return, SMB, HML)

# Define URL for the Fama-French factors dataset
ffFactorsURL <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
download.file(ffFactorsURL, destfile = "F-F_Research_Data_Factors_CSV.zip", mode = "wb")
unzip("F-F_Research_Data_Factors_CSV.zip", exdir = "FF_factors")

# List files to verify extraction (optional)
list.files("FF_factors")

# Read the CSV file (skip first 3 rows of header text)
ffFactors <- read.csv("FF_factors/F-F_Research_Data_Factors.CSV", skip = 3)

# Convert the date column (in YYYYMM format) to Date type
ffFactors$Date <- as.Date(paste0(substr(ffFactors$X, 1, 4), "-", 
                                 substr(ffFactors$X, 5, 6), "-01"))
ffFactors$X <- NULL  # Remove old column
ffFactors <- ffFactors[, c("Date", setdiff(names(ffFactors), "Date"))]
head(ffFactors)


## 4b. Momentum Factor (WML)

# Define URL for the momentum factor dataset
wmlURL <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip"
download.file(wmlURL, destfile = "F-F_Momentum_Factor_CSV.zip", mode = "wb")
unzip("F-F_Momentum_Factor_CSV.zip", exdir = "FF_momentum")

# Read the momentum factor CSV file (adjust 'skip' if necessary)
wmlFactor <- read.csv("FF_momentum/F-F_Momentum_Factor.CSV",
                      skip = 14,
                      header = FALSE,
                      na.strings = c("-99.99", "-999"))
colnames(wmlFactor) <- c("YYYYMM", "WML")

# Convert the date column to Date type (assume day = 01)
wmlFactor$Date <- as.Date(paste0(substr(wmlFactor$YYYYMM, 1, 4), "-",
                                 substr(wmlFactor$YYYYMM, 5, 6), "-01"))
wmlFactor$YYYYMM <- NULL
wmlFactor <- wmlFactor[, c("Date", setdiff(names(wmlFactor), "Date"))]
head(wmlFactor)


## Daily Factor Data: Fama-French & Momentum

# Download daily Fama-French factors
ffDailyURL <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
download.file(ffDailyURL, destfile = "F-F_Research_Data_Factors_daily_CSV.zip", mode = "wb")
unzip("F-F_Research_Data_Factors_daily_CSV.zip", exdir = "FF_factors_daily")
list.files("FF_factors_daily")

# Read daily Fama-French factors CSV (skip header rows if necessary)
ffDaily <- read.csv("FF_factors_daily/F-F_Research_Data_Factors_daily.CSV", skip = 3)
ffDaily$Date <- as.Date(as.character(ffDaily$X), format = "%Y%m%d")
ffDaily$X <- NULL
ffDaily <- ffDaily[, c("Date", setdiff(names(ffDaily), "Date"))]
head(ffDaily)

# Download daily Momentum Factor (WML)
wmlDailyURL <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_daily_CSV.zip"
download.file(wmlDailyURL, destfile = "F-F_Momentum_Factor_daily_CSV.zip", mode = "wb")
unzip("F-F_Momentum_Factor_daily_CSV.zip", exdir = "FF_momentum_daily")
list.files("FF_momentum_daily")

# Read daily momentum factor CSV (adjust 'skip' if necessary)
wmlDaily <- read.csv("FF_momentum_daily/F-F_Momentum_Factor_daily.CSV",
                     skip = 14,
                     header = FALSE,
                     na.strings = c("-99.99", "-999"))
colnames(wmlDaily) <- c("YYYYMMDD", "WML")
wmlDaily$Date <- as.Date(as.character(wmlDaily$YYYYMMDD), format = "%Y%m%d")
wmlDaily$YYYYMMDD <- NULL
wmlDaily <- wmlDaily[, c("Date", setdiff(names(wmlDaily), "Date"))]
head(wmlDaily)


# ---- 5. Merge Daily Factor Data and T-Bill Rates ----

# Merge daily Fama-French factors with daily Momentum factor by Date
dailyFactors <- merge(ffDaily, wmlDaily, by = "Date", all = TRUE)

# Merge the result with the T-Bill rate data
dailyFactors <- merge(dailyFactors, tbillData, by = "Date", all = TRUE)
head(dailyFactors)

# Filter to include only dates from 2023-12-31 onward
dailyFactors <- dailyFactors[dailyFactors$Date >= "2023-12-31", ]


# ---- 6. Clean and Reshape Portfolio Price Data ----

# Remove specific assets ("TT333904     Corp" and "USD Curncy")
cleanPortfolioPrices <- portfolioPrices[portfolioPrices$id != "TT333904     Corp", ]
cleanPortfolioPrices <- cleanPortfolioPrices[cleanPortfolioPrices$id != "USD Curncy", ]

# Restrict portfolio price data to dates from 2023-12-31 onward
cleanPortfolioPrices <- cleanPortfolioPrices[cleanPortfolioPrices$date >= "2023-12-31", ]

# Plot missing value patterns (assumes plot_price_NAs() is defined)
plot_price_NAs(cleanPortfolioPrices)

# Convert portfolio price data to wide format: one column per asset
widePortfolioPrices <- cleanPortfolioPrices %>%
  pivot_wider(names_from = id, values_from = px)
head(widePortfolioPrices)

# Plot each asset using quantmod's chartSeries()
for (i in 2:ncol(widePortfolioPrices)) {
  chartSeries(widePortfolioPrices[, c(1, i)],
              theme = chartTheme("white"),
              name = colnames(widePortfolioPrices)[i])
}

# Clean portfolio data DT to include dates from 2023-12-31 until 2024-12-31  and remove incomplete rows
cleanPortfolioDT <- DT[DT$date > "2023-12-31" & DT$date <= "2024-12-31", ]
cleanPortfolioDT <- cleanPortfolioDT[complete.cases(cleanPortfolioDT), ]


# ---- 7. Merge All Datasets for Regression Analysis ----

# Ensure consistent date column naming across datasets
colnames(widePortfolioPrices)[colnames(widePortfolioPrices) == "Date"] <- "date"
colnames(dailyFactors)[colnames(dailyFactors) == "Date"] <- "date"
colnames(cleanPortfolioDT)[colnames(cleanPortfolioDT) == "Date"] <- "date"

# Merge portfolio prices (wide format) with daily factor data by date
mergedData <- merge(widePortfolioPrices, dailyFactors, by = "date", all = TRUE)

# Limit the merged dataset to dates up to 2024-12-31
mergedData <- mergedData[mergedData$date <= "2024-12-31", ]

# Optionally, count the number of missing values per column
naCounts <- sapply(mergedData, function(x) sum(is.na(x)))
print(naCounts)


# ---- 8. Impute Missing Values in Merged Data ----

# Create a copy of merged data for imputation
imputedData <- mergedData

# Identify numeric columns (excluding the date column)
numericCols <- setdiff(names(imputedData), "date")

# Loop over each numeric column and impute missing values using a 7-day centered moving average
for (col in numericCols) {
  series <- imputedData[[col]]
  
  # Compute 7-day moving average (centered)
  movingAvg <- rollapply(series, width = 7, FUN = function(x) mean(x, na.rm = TRUE),
                         fill = NA, align = "center")
  
  # Replace NA values with the moving average where available
  imputedSeries <- ifelse(is.na(series) & !is.na(movingAvg), movingAvg, series)
  
  # For remaining NAs (typically at boundaries), perform forward-fill then backward-fill
  imputedSeries <- na_locf(imputedSeries, na.rm = FALSE)
  imputedSeries <- rev(na_locf(rev(imputedSeries), na.rm = FALSE))
  
  imputedData[[col]] <- imputedSeries
}

# Optionally, drop the last row if it contains incomplete data
imputedData <- imputedData[1:(nrow(imputedData) - 1), ]


# ---- 9. Convert to Time Series & Plot the Data ----

# Convert imputed data (excluding the date column) to an xts object for time series analysis
imputedTS <- xts(imputedData[,-1], order.by = as.Date(imputedData$date))

# Plot each time series using chartSeries() from quantmod
for (i in 2:ncol(imputedData)) {
  chartSeries(imputedData[, c(1, i)],
              theme = chartTheme("white"),
              name = colnames(imputedData)[i])
}



#### END OF DATA PREP ####


#### PERFORMANCE MEASUREMENT ####


#### load libraries ####

# install_github("Jaekoeb/pmp.engine")
library(pmp.engine)
library(lubridate)
library(Rblpapi)
library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(imputeTS)
library(zoo)
library(ggplot2)
library(scales)
library(forcats)
library(gridExtra)

# blpConnect()


#### Jensen´s Alpha ####


### Creating the dataset
# port_ret = readxl::read_xlsx("C:/Users/Student2.AzureAD/ZZ Vermögensverwaltung GmbH/ISK-Wien - General/ZZ Gruppe/2024/Personal/Farkas/PMP_TM2/portfolio_returns.xlsx", sheet = 1)
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


# # export the daily poprtfolio return to a .rda file
# save(Jensen_data, file = "C:/Users/Student2.AzureAD/ZZ Vermögensverwaltung GmbH/ISK-Wien - General/ZZ Gruppe/2024/Personal/Farkas/PMP_TM2/Jensen_data.rda")

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

# Nice table
# install.packages("stargazer") # if not already installed
library(stargazer)

# Ensure results_df is in the column order you want:
results_df <- results_df[, c(
  "Fund", "Beta", "JensenAlpha", "MultifactorAlpha",
  "SharpeRatio", "TreynorRatio", "TrackingError",
  "InformationRatio", "AnnualReturn"
)]
#to results_df add a new line for Fund Portfolio
results_df <- rbind(results_df, c("Portfolio", portfolio_beta, mean(Jensen_data$Jensen_Alpha), portfolio_alpha, Sharpe_ratio, Treynor_ratio, tracking_error, information_ratio, portfolio_ret_1_Y))

# Rounding
results_df_rounded <- results_df 
str(results_df_rounded)
results_df_rounded[,2:9] <- lapply(results_df_rounded[,2:9], function(x) as.numeric(as.character(x)))
results_df_rounded <- results_df_rounded %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Create a stargazer table (plain text)
stargazer(
  results_df_rounded,
  type    = "text",       # for console/text output. Use "latex" or "html" as needed.
  summary = FALSE,        # don't print summary stats
  rownames = FALSE,       # don't show row names
  digits = 4,             # decimal places
  title = "Performance Metrics",
  label = "tab:performance_metrics"
)


####  Visualizing Funds Holdings ####
#import Fund_Holdings.xlsx
brk_indices_holdings <- readxl::read_xlsx("Fund_Holdings.xlsx", sheet = 1)
aqr_port <- readxl::read_xlsx("Fund_Holdings.xlsx", sheet = 2)
target_funds <- readxl::read_xlsx("Fund_Holdings.xlsx", sheet = 3)
str(brk_indices_holdings)
str(aqr_port)
str(target_funds)


############ Trying to sort the plots ########



# Define your custom palette (8 provided colors plus 3 additional ones)
custom_palette <- c("#d5c4b4","#c6b7a1", "#b7aa8e", "#afa98d",
                    "#a99177", "#a27968", "#966160", "#844d5f",
                    "#6a3d60","#45325f","#002a5a")

# Pivot the data to long format (asset classes in `...1`, funds in 'Fund')
long_data <- pivot_longer(
  brk_indices_holdings, 
  cols = -`...1`, 
  names_to = "Fund", 
  values_to = "Weight"
)

# (Optional) Reorder funds on the x-axis by total weight if desired:
long_data <- long_data %>%
  group_by(Fund) %>%
  mutate(total_weight = sum(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Fund = fct_reorder(Fund, total_weight, .desc = FALSE))

# Choose three funds to plot.
# (Here we take the first three funds in the sorted order; adjust as needed)
fund_list <- unique(long_data$Fund)[1:3]

# Create a list of plots—each plot will be a 100% stacked bar for one fund.
plots <- lapply(fund_list, function(fund_name) {
  data_fund <- filter(long_data, Fund == fund_name)
  
  # For each fund, reorder asset classes by Weight (ascending) individually:
  data_fund$`...1` <- factor(data_fund$`...1`, 
                             levels = data_fund$`...1`[order(data_fund$Weight)])
  
  ggplot(data_fund, aes(x = Fund, y = Weight, fill = `...1`)) +
    # Use position = "fill" for 100% stacked bars
    geom_bar(stat = "identity", width = 0.7, color = "black", position = "fill") +
    labs(title = fund_name, x = "Fund", y = "Proportion", fill = "Sector") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = custom_palette) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", size = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold")
    )
})

# Arrange the three plots in a 1 x 3 layout
grid.arrange(grobs = plots, nrow = 1)






##### AQR and ZZ portfolio #####


# Define your custom palette (5 colors)
custom_palette2 <- c( "#d5c4b4", "#b7aa8e","#a27968", "#844d5f","#002a5a")

# Pivot the data to long format (asset classes in `...1`, funds in 'Fund')
long_data <- pivot_longer(
  aqr_port, 
  cols = -`...1`, 
  names_to = "Fund", 
  values_to = "Weight"
)

# (Optional) Reorder funds on the x-axis by total weight if desired:
long_data <- long_data %>%
  group_by(Fund) %>%
  mutate(total_weight = sum(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Fund = fct_reorder(Fund, total_weight, .desc = FALSE))

# Choose three funds to plot.
# (Here we take the first three funds in the sorted order; adjust as needed)
fund_list <- unique(long_data$Fund)[1:2]

# Create a list of plots—each plot will be a 100% stacked bar for one fund.
plots <- lapply(fund_list, function(fund_name) {
  data_fund <- filter(long_data, Fund == fund_name)
  
  # For each fund, reorder asset classes by Weight (ascending) individually:
  data_fund$`...1` <- factor(data_fund$`...1`, 
                             levels = data_fund$`...1`[order(data_fund$Weight)])
  
  ggplot(data_fund, aes(x = Fund, y = Weight, fill = `...1`)) +
    # Use position = "fill" for 100% stacked bars
    geom_bar(stat = "identity", width = 0.7, color = "black", position = "fill") +
    labs(title = fund_name, x = "Fund", y = "Proportion", fill = "Sector") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = custom_palette2) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", size = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold")
    )
})

# Arrange the three plots in a 1 x 3 layout
grid.arrange(grobs = plots, nrow = 1)





##### Target Funds #####  
# changes the order of the columns in target_funds
str(target_funds)
# str(target_funds)
# tibble [5 × 4] (S3: tbl_df/tbl/data.frame)
# $ Asset_Class               : chr [1:5] "U.S. Equities" "U.S. Bonds" "Cash" "Non-U.S. Equities" ...
# $ American_Funds_2030_Target: num [1:5] 0.439 0.342 0.04 0.139 0.04
# $ American_Funds_2025_Target: num [1:5] 0.355 0.44 0.044 0.111 0.05
# $ American_Funds_2050_Target: num [1:5] 0.631 0.078 0.036 0.238 0.017
# the order should be: 2025, 2030, 2050
target_funds <- target_funds %>%
  select(Asset_Class, American_Funds_2025_Target, American_Funds_2030_Target, American_Funds_2050_Target)



# Pivot the data to long format (asset classes in `...1`, funds in 'Fund')
long_data <- pivot_longer(
  target_funds, 
  cols = -`Asset_Class`, 
  names_to = "Fund", 
  values_to = "Weight"
)

# (Optional) Reorder funds on the x-axis by total weight if desired:
long_data <- long_data %>%
  group_by(Fund) %>%
  mutate(total_weight = sum(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Fund = fct_reorder(Fund, total_weight, .desc = FALSE))

# Choose three funds to plot.
# (Here we take the first three funds in the sorted order; adjust as needed)
fund_list <- unique(long_data$Fund)[1:3]

# Create a list of plots—each plot will be a 100% stacked bar for one fund.
plots <- lapply(fund_list, function(fund_name) {
  data_fund <- filter(long_data, Fund == fund_name)
  
  # For each fund, reorder asset classes by Weight (ascending) individually:
  data_fund$`Asset_Class` <- factor(data_fund$`Asset_Class`, 
                                    levels = data_fund$`Asset_Class`[order(data_fund$Weight)])
  
  ggplot(data_fund, aes(x = Fund, y = Weight, fill = `Asset_Class`)) +
    # Use position = "fill" for 100% stacked bars
    geom_bar(stat = "identity", width = 0.7, color = "black", position = "fill") +
    labs(title = fund_name, x = "Fund", y = "Proportion", fill = "Sector") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = custom_palette2) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", size = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold")
    )
})

# Arrange the three plots in a 1 x 3 layout
grid.arrange(grobs = plots, nrow = 1)


########## finished trying











library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)

# Define a color-blind friendly base palette (Okabe-Ito)
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

n_sectors <- length(unique(brk_indices_holdings$`...1`))
cb_colors <- colorRampPalette(cb_palette)(n_sectors)

ggplot(
  tidyr::pivot_longer(brk_indices_holdings, cols = -`...1`, 
                      names_to = "Fund", values_to = "Weight"),
  aes(x = Fund, y = Weight*100, fill = `...1`)
) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(
    title = "Fund Holdings Breakdown",
    x = "Fund",
    y = "Weight (%)",
    fill = "Sector"
  ) +
  scale_y_continuous(expand = c(0, 0), labels = percent_format(scale = 1)) +
  scale_fill_manual(values = cb_colors) +
  theme_minimal(base_size = 12) +
  theme(
    # IBCS style adjustments:
    panel.grid.major.x = element_blank(),          # Remove vertical gridlines
    panel.grid.minor = element_blank(),              # Remove minor gridlines
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )



ggplot(
  tidyr::pivot_longer(aqr_port, cols = -`...1`, 
                      names_to = "Fund", values_to = "Weight"),
  aes(x = Fund, y = Weight*100, fill = `...1`)
) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(
    title = "Fund Holdings Breakdown",
    x = "Fund",
    y = "Weight (%)",
    fill = "Asset Class"
  ) +
  scale_y_continuous(expand = c(0, 0), labels = percent_format(scale = 1)) +
  scale_fill_manual(values = cb_colors) +
  theme_minimal(base_size = 12) +
  theme(
    # IBCS style adjustments:
    panel.grid.major.x = element_blank(),          # Remove vertical gridlines
    panel.grid.minor = element_blank(),              # Remove minor gridlines
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )



ggplot(
  tidyr::pivot_longer(target_funds, cols = -`Asset_Class`, 
                      names_to = "Fund", values_to = "Weight"),
  aes(x = Fund, y = Weight*100, fill = `Asset_Class`)
) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(
    title = "Fund Holdings Breakdown",
    x = "Fund",
    y = "Weight (%)",
    fill = "Asset Class"
  ) +
  scale_y_continuous(expand = c(0, 0), labels = percent_format(scale = 1)) +
  scale_fill_manual(values = cb_colors) +
  theme_minimal(base_size = 12) +
  theme(
    # IBCS style adjustments:
    panel.grid.major.x = element_blank(),          # Remove vertical gridlines
    panel.grid.minor = element_blank(),              # Remove minor gridlines
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

### calculate accumulated returns
acc_returns <- data_merged %>%
  # Keep only date and the .TRI columns
  select(date, ends_with(".TRI")) %>%
  # For each .TRI column, divide by its first value, and rename columns
  mutate(
    across(
      ends_with(".TRI"), 
      ~ .x / first(.x), 
      .names = "{sub('.TRI','',.col)}.acc"  # rename aqr.TRI -> aqr.acc, etc.
    )
  )

acc_returns <- acc_returns %>%
  # Join the Portfolio_value by matching date
  left_join(
    select(Jensen_data, date, Portfolio_value),
    by = "date"
  ) %>%
  # Calculate accumulated returns: ratio to the first row’s Portfolio_value
  mutate(portfolio = Portfolio_value / first(Portfolio_value)) %>%
  # (Optional) drop the Portfolio_value column if you only need 'portfolio'
  select(-Portfolio_value) 
acc_returns <- acc_returns[, -c(2:9)]



library(ggplot2)
library(scales)

ggplot(
  tidyr::pivot_longer(acc_returns, cols = -date, 
                      names_to = "Fund", values_to = "Accumulated_Return"),
  aes(x = date, y = Accumulated_Return, color = Fund)
) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Returns 2024",
    x = "Date",
    y = "Cumulative Returns"
  ) +
  scale_color_manual(
    values = cb_colors,
    breaks = c(
      "aqr.acc", "brk.acc", "spdr.acc", 
      "t30.acc", "i25.acc", "t50.acc", 
      "wexus.acc", "usgyt.acc", "portfolio"
    ),
    labels = c(
      "AQR Style Premia Fund",
      "Berkshire Hathaway Inc",
      "SPDR S&P 500 ETF Trust",
      "US 2030 Target Fund",
      "US 2025 Target Fund",
      "US 2050 Target Fund",
      "MSCI World ex US",
      "Bloomberg US Treasury Unhedged",
      "ZZ Portfolio"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



##### END OF PERFORMANCE MEASUREMENT ####


#### STYLE ANALYSIS ####


# SECTION 0: PRELIMINARIES ----
# Load necessary packages
library(lubridate)
library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(zoo)
library(quadprog)
library(ggplot2)
library(gridExtra)

# SECTION 1: Put Data together

#Our own portfolio prices are in the price_wide dataframe

# DT dataframe holds the prices and Total Return Index (TRI) of the given target funds
# select only TRI for return analysis and use data for the last 4 years
# TRI is not a return itself but an index that tracks the cumulative value of an asset
# including price movements and reinvested dividends.

# GIVEN FUNDS
DT <- DT[DT$date >= "2021-02-28",]
DT_TRI <- DT[,c(1,2,4,6)]

DT_TRI <- DT_TRI %>% drop_na()

DT_ret <- DT_TRI %>%
  arrange(date) %>%
  mutate(
    aqr.TRI_ret = (aqr.TRI / lag(aqr.TRI)) - 1,
    brk.TRI_ret = (brk.TRI / lag(brk.TRI)) - 1,
    spdr.TRI_ret = (spdr.TRI / lag(spdr.TRI)) - 1
  )
DT_ret <- DT_ret[,-(2:4)]

# ZZ Portfolio 2024

ZZ_TRI <- price_wide %>%
  mutate(total_sum = rowSums(across(-1), na.rm = TRUE))

ZZ_ret <- ZZ_TRI  %>%
  mutate(
    zz_ret = (total_sum / lag(total_sum)) - 1
  )

dim(ZZ_ret)
ZZ_ret <- ZZ_ret[,-(2:18)]

# Asset Classes / Regressors
# US Equity, MSCI World, US T-Bills, US government bonds

#Price Data
us_equity <- bloomberg_data[[1]]
colnames(us_equity) <- c("date","us.equity.PX")
msci_world <- bloomberg_data[[2]]
colnames(msci_world) <- c("date","msciworld.PX")
us_govbonds <- bloomberg_data[[3]]
colnames(us_govbonds) <- c("date","us.govbonds.PX")

us_tbill <- tbill_daily %>%
  mutate(
    price = 1000 - (DTB3*1000*90/360)
  )
us_tbill <- us_tbill[,-2]
colnames(us_tbill) <- c("date","us.tbill.PX")

AC <- us_equity %>%
  left_join(msci_world, by = "date") %>%
  left_join(us_govbonds, by = "date") %>%
  left_join(us_tbill, by = "date")

AC <- AC[AC$date >= "2021-02-28",]

AC_ret <- AC %>%
  mutate(
    us.equity.ret = (us.equity.PX / lag(us.equity.PX)) - 1,
    msciworld.ret = (msciworld.PX / lag(msciworld.PX)) - 1,
    us.govbonds.ret = (us.govbonds.PX / lag(us.govbonds.PX)) - 1,
    us.tbill.ret = (us.tbill.PX / lag(us.tbill.PX)) - 1
  )
AC_ret <- AC_ret[,-(2:5)]

# New Asset Classes 

euribor_3M <- bloomberg_data_add[[1]]
colnames(euribor_3M) <- c("date","EURIBOR3M.PX")
em_govbonds <- bloomberg_data_add[[2]]
colnames(em_govbonds) <- c("date","EMgovbonds.PX")
commodities <- bloomberg_data_add[[3]]
colnames(commodities) <- c("date","commodities.PX")

Add_AC <- euribor_3M %>%
  left_join(em_govbonds, by = "date") %>%
  left_join(commodities, by = "date")

Add_AC_ret <- Add_AC %>%
  mutate(
    EURIBOR3M.ret = (EURIBOR3M.PX / lag(EURIBOR3M.PX)) - 1,
    EMgovbonds.ret = (EMgovbonds.PX / lag(EMgovbonds.PX)) - 1,
    commodities.ret = (commodities.PX / lag(commodities.PX)) - 1,
  )

Add_AC_ret <- Add_AC_ret[,-(2:4)]




# in order to use quantmod package functions, use xts format
DT_ret_xts <- xts(DT_ret[, -1], order.by = DT_ret$date)
ZZ_ret_xts <-xts(ZZ_ret[, -1], order.by = ZZ_ret$date)
AC_ret_xts <- xts(AC_ret[, -1], order.by = AC_ret$date)
Add_AC_ret_xts <- xts(Add_AC_ret[, -1], order.by = Add_AC_ret$date)



# SECTION 2: Calculate weekly returns Returns and eliminate possible NAs

# GIVEN FUNDS
# Convert data to weekly returns
DT_ret_w <- apply.weekly(DT_ret_xts,last)
dim(DT_ret_w)
# Find possible NAs
which(rowSums(is.na(DT_ret_w)) > 0)
which(colSums(is.na(DT_ret_w)) > 0)

# OWN PORTFOLIO
ZZ_ret_w <- apply.weekly(ZZ_ret_xts,last)
dim(ZZ_ret_w)
# Find possible NAs
which(rowSums(is.na(ZZ_ret_w)) > 0)
which(colSums(is.na(ZZ_ret_w)) > 0)

# ASSET CLASSES
AC_ret_w <- apply.weekly(AC_ret_xts,last)
#delete last entry as it is not given for the given funds
AC_ret_w <- AC_ret_w[-209,]
#Find NA values
which(rowSums(is.na(AC_ret_w)) > 0)
which(colSums(is.na(AC_ret_w)) > 0)
#Linearly interpolate the two missing values
for (i in 2:(nrow(AC_ret_w) - 1)) {  # Avoid first and last row
  if (is.na(AC_ret_w$us.tbill.ret[i])) {
    AC_ret_w$us.tbill.ret[i] <- mean(c(AC_ret_w$us.tbill.ret[i - 1], AC_ret_w$us.tbill.ret[i + 1]), na.rm = TRUE)
  }
  if (is.na(AC_ret_w$us.govbonds.ret[i])) {
    AC_ret_w$us.govbonds.ret[i] <- mean(c(AC_ret_w$us.govbonds.ret[i - 1], AC_ret_w$us.govbonds.ret[i + 1]), na.rm = TRUE)
  }
}
#Check
which(rowSums(is.na(AC_ret_w)) > 0)
which(colSums(is.na(AC_ret_w)) > 0)

# Additional Asset Classes
Add_AC_ret_w <- apply.weekly(Add_AC_ret_xts,last)
Add_AC_ret_w <- Add_AC_ret_w[-209,]
dim(Add_AC_ret_w)
which(rowSums(is.na(Add_AC_ret_w)) > 0)
which(colSums(is.na(Add_AC_ret_w)) > 0)

for (i in 2:(nrow(Add_AC_ret_w) - 1)) {  # Avoid first and last row
  if (is.na(Add_AC_ret_w$commodities.ret[i])) {
    Add_AC_ret_w$commodities.ret[i] <- mean(c(Add_AC_ret_w$commodities.ret[i - 1], Add_AC_ret_w$commodities.ret[i + 1]), na.rm = TRUE)
  }
}

which(rowSums(is.na(Add_AC_ret_w)) > 0)
which(colSums(is.na(Add_AC_ret_w)) > 0)




# SECTION 3: QUADRATIC PROGRAMMING

# Translate the data frames into the form for the constraint quadratic program
# We need D, the quadratic matrix, the linear term d, the coefficients beta and the constraints

# GIVEN FUNDS OVER FOUR YEARS

# Match dates

style_analysis <- list()

GF <- inner_join(data.frame(Date = index(DT_ret_w),coredata(DT_ret_w)),
                 data.frame(Date = index(AC_ret_w),coredata(AC_ret_w)),by = "Date")

factor_returns <- as.matrix(GF[,(5:8)])  # Factor return matrix (T x n)
# Add a column of 1s to the factor return matrix for analyzing an intercept alpha
intercept_col <- rep(1,nrow(factor_returns))
factor_returns <- cbind(intercept_col, factor_returns)

## SUBSECTION 3a: ORIGINAL EXERCISE

for(i in (2:4)){
  fund_returns <- as.matrix(GF[,i])  # Fund return vector (T x 1)
  
  # Create needed matrices in the correct form for quadratic programming
  # Matrix D
  D <- 2 * t(factor_returns) %*% factor_returns
  # Linear term d
  d <- 2 * t(factor_returns) %*% fund_returns
  
  # Add needed Constraints --> Coeffiecents sum up to one and Non-Negativity
  n <- ncol(factor_returns) -1 #leave out the intercept
  # Constraint Matrix A
  A <- rbind(c(0, rep(1,n)), cbind(0, diag(n)))
  # Right-hand side of the constraints
  b <- c(1, rep(0, n))
  
  # Solve quadratic optimization
  result <- solve.QP(D, d, t(A), b, meq = 1)
  
  # Add to list of style analysis
  style_analysis[[i-1]] <- result$solution
}

## SUBSECTION 3b: ROLLING WINDOW

style_analysis_rolling <- list()

for(i in (2:4)){
  
  fund_returns <- as.matrix(GF[,i])  # Fund return vector (T x 1)
  weekly_results <- data.frame()
  
  for (j in 53:nrow(factor_returns)){
    date_rolling <- GF$Date[j]
    factor_returns_rolling <- factor_returns[(j-52):j,]
    fund_returns_rolling <- fund_returns[(j-52):j,]
    
    # Create needed matrices in the correct form for quadratic programming
    # Matrix D
    D <- 2 * t(factor_returns_rolling) %*% factor_returns_rolling
    # Linear term d
    d <- 2 * t(factor_returns_rolling) %*% fund_returns_rolling
    
    # Add needed Constraints --> Coeffiecents sum up to one and Non-Negativity
    n <- ncol(factor_returns_rolling) -1 #leave out the intercept
    # Constraint Matrix A
    A <- rbind(c(0, rep(1,n)), cbind(0, diag(n)))
    # Right-hand side of the constraints
    b <- c(1, rep(0, n))
    
    # Solve quadratic optimization
    result <- solve.QP(D, d, t(A), b, meq = 1)
    
    # Add to list of style analysis
    weekly_results <- rbind(weekly_results, result$solution)
  }
  colnames(weekly_results) <- c("Intercept", "US Equity","MSCI World","US Gov Bonds","US TBills")
  style_analysis_rolling[[i-1]] <- weekly_results
}





# SECTION 4: VISUALIZATION
# Visualize style loadings for given funds

for (i in (1:3)) {
  par(mfrow = c(1, 2))
  # Create a bar plot
  bar_data <- style_analysis[[i]][-1]
  names(bar_data) <-  c("US Equity","MSCI World","US Gov Bonds","US TBills")
  barplot(bar_data, 
          main = "Style Analysis",  # Title of the chart
          xlab = "Categories",         # Label for the x-axis
          ylab = "Loading",             # Label for the y-axis
          col = "#0057B8",           # Color of the bars
          border = "black")            # Color of the borders around bars
  
  # Create a pie chart
  sum <- sum(style_analysis[[i]])
  style <- sum(style_analysis[[i]][-1]) / sum
  selection <- style_analysis[[i]][1] / sum
  pie(c(style,selection), 
      labels = c("Style","Selection"),             # Adding labels
      main = "Style versus Selection",   # Title of the chart
      col = c("#0057B8", "coral"))  # Colors for each slice
  
  par(mfrow = c(1, 1))
}


# ZZ PORTFOLIO

ZZ <- inner_join(data.frame(Date = index(ZZ_ret_w),coredata(ZZ_ret_w)),
                 data.frame(Date = index(AC_ret_w),coredata(AC_ret_w)),by = "Date")

factor_returns <- as.matrix(ZZ[,(3:6)])  # Factor return matrix (T x n)
# Add a column of 1s to the factor return matrix for analyzing an intercept alpha
intercept_col <- rep(1,nrow(factor_returns))
factor_returns <- cbind(intercept_col, factor_returns)
fund_returns <- as.matrix(ZZ[,2])  # Fund return vector (T x 1)

# Create needed matrices in the correct form for quadratic programming
# Matrix D
D <- 2 * t(factor_returns) %*% factor_returns
# Linear term d
d <- 2 * t(factor_returns) %*% fund_returns

# Add needed Constraints --> Coeffiecents sum up to one and Non-Negativity
n <- ncol(factor_returns) -1 #leave out the intercept
# Constraint Matrix A
A <- rbind(c(0, rep(1,n)), cbind(0, diag(n)))
# Right-hand side of the constraints
b <- c(1, rep(0, n))

# Solve quadratic optimization
result <- solve.QP(D, d, t(A), b, meq = 1)
ZZ_coef <- result$solution
names(ZZ_coef) <- c("Intercept","US Equity","MSCI World","US Gov Bonds","US TBills")

par(mfrow = c(1, 2))
# Create a bar plot
bar_data <- ZZ_coef[-1]
names(bar_data) <-  c("US Equity","MSCI World","US Gov Bonds","US TBills")
barplot(bar_data, 
        main = "Style Analysis",  # Title of the chart
        xlab = "Categories",         # Label for the x-axis
        ylab = "Loading",             # Label for the y-axis
        col = "#0057B8",           # Color of the bars
        border = "black")            # Color of the borders around bars

# Create a pie chart
sum <- sum(ZZ_coef)
style <- sum(ZZ_coef[-1]) / sum
selection <- ZZ_coef[1] / sum
pie(c(style,selection), 
    labels = c("Style","Selection"),             # Adding labels
    main = "Style versus Selection",   # Title of the chart
    col = c("#0057B8", "coral"))  # Colors for each slice

par(mfrow = c(1, 1))



# SECTION 5: Add the additional asset classes to the analysis 

style_analysis_add <- list()

GF_Add <- inner_join(data.frame(Date = index(DT_ret_w),coredata(DT_ret_w)),
                     data.frame(Date = index(AC_ret_w),coredata(AC_ret_w)),
                     by = "Date")
GF_Add <- inner_join(GF_Add,
                     data.frame(Date = index(Add_AC_ret_w),coredata(Add_AC_ret_w)),
                     by = "Date")


factor_returns_add <- as.matrix(GF_Add[,(5:11)])  # Factor return matrix (T x n)
# Add a column of 1s to the factor return matrix for analyzing an intercept alpha
intercept_col <- rep(1,nrow(factor_returns_add))
factor_returns_add <- cbind(intercept_col, factor_returns_add)


for(i in (2:4)){
  fund_returns_add <- as.matrix(GF_Add[,i])  # Fund return vector (T x 1)
  
  # Create needed matrices in the correct form for quadratic programming
  # Matrix D
  D <- 2 * t(factor_returns_add) %*% factor_returns_add
  # Linear term d
  d <- 2 * t(factor_returns_add) %*% fund_returns_add
  
  # Add needed Constraints --> Coeffiecents sum up to one and Non-Negativity
  n <- ncol(factor_returns_add) -1 #leave out the intercept
  # Constraint Matrix A
  A <- rbind(c(0, rep(1,n)), cbind(0, diag(n)))
  # Right-hand side of the constraints
  b <- c(1, rep(0, n))
  
  # Solve quadratic optimization
  result <- solve.QP(D, d, t(A), b, meq = 1)
  
  # Add to list of style analysis
  style_analysis_add[[i-1]] <- result$solution
}



for (i in (1:3)) {
  par(mfrow = c(1, 2))
  # Create a bar plot
  bar_data <- style_analysis_add[[i]][-1]
  names(bar_data) <-  c("US Equity","MSCI World","US Gov Bonds","US TBills","EURIBOR3M","EM Gov Bonds","Commodities")
  barplot(bar_data, 
          main = "Style Analysis",  # Title of the chart
          xlab = "Categories",         # Label for the x-axis
          ylab = "Loading",             # Label for the y-axis
          col = "#0057B8",           # Color of the bars
          border = "black")            # Color of the borders around bars
  
  # Create a pie chart
  sum <- sum(style_analysis_add[[i]])
  style <- sum(style_analysis_add[[i]][-1]) / sum
  selection <- style_analysis_add[[i]][1] / sum
  pie(c(style,selection), 
      labels = c("Style","Selection"),             # Adding labels
      main = "Style versus Selection",   # Title of the chart
      col = c("#0057B8", "coral"))  # Colors for each slice
  
  par(mfrow = c(1, 1))
}

# ZZ Portfolio with additional asset classes

ZZ_add <- inner_join(data.frame(Date = index(ZZ_ret_w),coredata(ZZ_ret_w)),
                     data.frame(Date = index(AC_ret_w),coredata(AC_ret_w)),by = "Date")
ZZ_add <- inner_join(ZZ_add,
                     data.frame(Date = index(Add_AC_ret_w),coredata(Add_AC_ret_w)),by = "Date")


factor_returns_add <- as.matrix(ZZ_add[,(3:9)])  # Factor return matrix (T x n)
# Add a column of 1s to the factor return matrix for analyzing an intercept alpha
intercept_col <- rep(1,nrow(factor_returns_add))
factor_returns_add <- cbind(intercept_col, factor_returns_add)
fund_returns_add <- as.matrix(ZZ_add[,2])  # Fund return vector (T x 1)

# Create needed matrices in the correct form for quadratic programming
# Matrix D
D <- 2 * t(factor_returns_add) %*% factor_returns_add
# Linear term d
d <- 2 * t(factor_returns_add) %*% fund_returns_add

# Add needed Constraints --> Coeffiecents sum up to one and Non-Negativity
n <- ncol(factor_returns_add) -1 #leave out the intercept
# Constraint Matrix A
A <- rbind(c(0, rep(1,n)), cbind(0, diag(n)))
# Right-hand side of the constraints
b <- c(1, rep(0, n))

# Solve quadratic optimization
result_add <- solve.QP(D, d, t(A), b, meq = 1)
ZZ_coef_add <- result_add$solution
names(ZZ_coef_add) <- c("Intercept","US Equity","MSCI World","US Gov Bonds","US TBills","EURIBOR3M","EM Gov Bonds","Commodities")

par(mfrow = c(1, 2))
# Create a bar plot
bar_data <- ZZ_coef_add[-1]
names(bar_data) <-  c("US Equity","MSCI World","US Gov Bonds","US TBills","EURIBOR3M","EM Gov Bonds","Commodities")
barplot(bar_data, 
        main = "Style Analysis",  # Title of the chart
        xlab = "Categories",         # Label for the x-axis
        ylab = "Loading",             # Label for the y-axis
        col = "#0057B8",           # Color of the bars
        border = "black")            # Color of the borders around bars

# Create a pie chart
sum <- sum(ZZ_coef_add)
style <- sum(ZZ_coef_add[-1]) / sum
selection <- ZZ_coef_add[1] / sum
pie(c(style,selection), 
    labels = c("Style","Selection"),             # Adding labels
    main = "Style versus Selection",   # Title of the chart
    col = c("#0057B8", "coral"))  # Colors for each slice

par(mfrow = c(1, 1))




#### END OF STYLE ANALYSIS ####

