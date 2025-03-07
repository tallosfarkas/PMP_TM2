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





