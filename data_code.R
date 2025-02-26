# SECTION 0: PRELIMINARIES ----
# Load necessary packages
library(pmp.engine)
library(lubridate)
library(Rblpapi)
library(quantmod)
library(xts)
library(dplyr)
library(tidyr)
library(imputeTS)
library(zoo)
# Connect to Bloomberg (the Bloomberg Terminal must be open)
Rblpapi::blpConnect()


# SECTION 1: LOAD SAVED DATA AND PORTFOLIO DATA ----
# Load previously saved workspace data (tm2.RData)
load("tm2.RData")

# Examine loaded objects:
# - assetdes: Asset descriptions
# - DT: Portfolio data; check first & last few rows, and summary statistics
print(assetdes)
head(DT)
tail(DT)
summary(DT)
df <- data.frame()
# Download portfolio data from Bloomberg using a custom function.
df <- download_port_bbg(start_date = Sys.Date() - years(11))
price <- df$prices  # Portfolio prices
id <- df$id         # Portfolio identifiers


# SECTION 2: DOWNLOAD BLOOMBERG DATA FOR BENCHMARK ASSET CLASSES ----
# Define date range for Bloomberg data
start_date <- as.Date("2014-12-31")
end_date   <- Sys.Date()

# Define tickers and fields for Bloomberg download.
tickers <- c("SPX Index", "MXWOU Index", "LUATTRUU Index")
fields  <- c("PX_LAST")  # We want the closing prices

# Download Bloomberg data for each asset using bdh() function
bloomberg_data <- lapply(tickers, function(ticker) {
  bdh(ticker, fields, start_date, end_date)
})
names(bloomberg_data) <- tickers

# Optionally, inspect the first few rows of the SPX Index data
head(bloomberg_data[["SPX Index"]])


# SECTION 3: DOWNLOAD U.S. T-BILL RATES FROM FRED ----
# "DTB3" is the symbol for 3-month Treasury bill rates from FRED.
# Get the T-bill rate data for the defined date range.
getSymbols("DTB3", src = "FRED", from = start_date, to = end_date)

# Inspect the first few rows of the T-bill rate data.
head(DTB3)


# SECTION 4: DOWNLOAD AND CLEAN FAMA-FRENCH FACTOR DATA ----

## 4a. Fama-French Factors (Rm-Rf, SMB, HML) ----
# Define the URL for the Fama-French factor dataset
ff_url  <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

# Download the Fama-French factors zip file.
download.file(ff_url, destfile = "F-F_Research_Data_Factors_CSV.zip", mode = "wb")

# Unzip the downloaded file into a directory named "FF_factors".
unzip("F-F_Research_Data_Factors_CSV.zip", exdir = "FF_factors")

# (Optional) Inspect the extracted files.
list.files("FF_factors")

# Read the CSV file containing the factors.
# Adjust 'skip' as needed to bypass header text.
ff_factors <- read.csv("FF_factors/F-F_Research_Data_Factors.CSV", skip = 3)

# Convert the 'X' column (in YYYYMM format) to a proper Date type.
ff_factors$Date <- as.Date(
  paste0(substr(ff_factors$X, 1, 4), "-",   # Extract year
         substr(ff_factors$X, 5, 6), "-01")   # Extract month and assign day=01
)

# Optionally remove the old 'X' column.
ff_factors$X <- NULL

# Reorder columns so that "Date" is the leftmost column.
ff_factors <- ff_factors[, c("Date", setdiff(names(ff_factors), "Date"))]

# Confirm the new column order.
head(ff_factors)


## 4b. Momentum Factor (WML) ----
# Define the URL for the momentum factor dataset
wml_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip"

# Download the momentum factor zip file.
download.file(wml_url, destfile = "F-F_Momentum_Factor_CSV.zip", mode = "wb")

# Unzip the downloaded file into a directory named "FF_momentum".
unzip("F-F_Momentum_Factor_CSV.zip", exdir = "FF_momentum")

# (Optional) Inspect the extracted files.
list.files("FF_momentum")

# Read the CSV file containing the momentum factor.
# Adjust the 'skip' parameter so that the data rows start at the correct line.
wml_factor <- read.csv("FF_momentum/F-F_Momentum_Factor.CSV",
                       skip = 14,             # Adjust as needed based on file structure
                       header = FALSE,
                       na.strings = c("-99.99", "-999")  # Specify missing value indicators
)

# Rename the columns.
colnames(wml_factor) <- c("YYYYMM", "WML")

# Convert the 'YYYYMM' column to a proper Date type.
wml_factor$Date <- as.Date(
  paste0(substr(wml_factor$YYYYMM, 1, 4), "-",
         substr(wml_factor$YYYYMM, 5, 6), "-01")
)

# Optionally remove the original 'YYYYMM' column.
wml_factor$YYYYMM <- NULL

# Reorder columns so that "Date" is the leftmost column.
wml_factor <- wml_factor[, c("Date", setdiff(names(wml_factor), "Date"))]

# Confirm the new column order.
head(wml_factor)









# Define the URL for the daily Fama-French factors dataset (example URL)
ff_daily_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"

# Download the daily factors zip file
download.file(ff_daily_url, destfile = "F-F_Research_Data_Factors_daily_CSV.zip", mode = "wb")

# Unzip the downloaded file into a directory
unzip("F-F_Research_Data_Factors_daily_CSV.zip", exdir = "FF_factors_daily")

# List files to confirm extraction
list.files("FF_factors_daily")

# Read the CSV file (adjust the skip parameter based on the file's structure)
ff_daily <- read.csv("FF_factors_daily/F-F_Research_Data_Factors_daily.CSV", skip = 3)

# Suppose the date column is named X and is in the format YYYYMMDD:
ff_daily$Date <- as.Date(as.character(ff_daily$X), format = "%Y%m%d")

# Optionally remove the old 'X' column
ff_daily$X <- NULL

# Reorder columns to have Date as the leftmost column
ff_daily <- ff_daily[, c("Date", setdiff(names(ff_daily), "Date"))]

# Check the cleaned daily factors data
head(ff_daily)









# SECTION: DOWNLOAD AND CLEAN DAILY MOMENTUM FACTOR (WML) DATA ----

# Define the URL for the daily momentum factor dataset
wml_daily_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_daily_CSV.zip"

# Download the daily momentum factor zip file
download.file(wml_daily_url, destfile = "F-F_Momentum_Factor_daily_CSV.zip", mode = "wb")

# Unzip the downloaded file into a directory named "FF_momentum_daily"
unzip("F-F_Momentum_Factor_daily_CSV.zip", exdir = "FF_momentum_daily")

# (Optional) List the files to verify extraction
list.files("FF_momentum_daily")

# Read the CSV file
# Adjust the 'skip' parameter based on the structure of the CSV file.
# For example, if the first actual data row starts after 14 lines of header text:
wml_daily <- read.csv("FF_momentum_daily/F-F_Momentum_Factor_daily.CSV",
                      skip = 14,          # adjust if necessary
                      header = FALSE,     # assume header is not part of the data rows
                      na.strings = c("-99.99", "-999")  # handle missing values if applicable
)

# Assign appropriate column names
# Typically, the file may have two columns: one for the date and one for the momentum factor.
colnames(wml_daily) <- c("YYYYMMDD", "WML")

# Convert the date column to an actual Date type (assumes format YYYYMMDD)
wml_daily$Date <- as.Date(as.character(wml_daily$YYYYMMDD), format = "%Y%m%d")

# Optionally, remove the original date column if you no longer need it
wml_daily$YYYYMMDD <- NULL

# Reorder columns so that "Date" is the leftmost column
wml_daily <- wml_daily[, c("Date", setdiff(names(wml_daily), "Date"))]

# Inspect the cleaned daily momentum factor data
head(wml_daily)





# 3. Prepare Daily T-Bill Rates (DTB3) from FRED ----
# The DTB3 data was already downloaded as a daily xts object using getSymbols()
# Convert the DTB3 xts object to a data frame
tbill_daily <- data.frame(Date = index(DTB3), coredata(DTB3))
# Rename the column (if needed) for clarity
colnames(tbill_daily)[2] <- "DTB3"

# Inspect the daily T-Bill data
head(tbill_daily)


# 4. Merge All Daily Factors on Date ----
# Merge the daily FF factors and daily Momentum factor
daily_factors <- merge(ff_daily, wml_daily, by = "Date", all = TRUE)
# Merge the result with the T-Bill rates
daily_factors <- merge(daily_factors, tbill_daily, by = "Date", all = TRUE)

# Inspect the final merged data frame
head(daily_factors)


daily_factors <- daily_factors[daily_factors$Date >= "2023-12-31",]
#print(daily_factors[daily_factors$Date == "2014-12-31",], digits=22)


plot_price_NAs(price)


# SECTION 5: CLEAN PORTFOLIO DATA ----
# remove TT333904     Corp and restrict the timeframe to 2024 - today from returns df
price_clean <- price[price$id != "TT333904     Corp",]
price_clean <- price_clean[price_clean$id != "USD Curncy",]

price_clean <- price_clean[price_clean$date >= "2023-12-31",]

plot_price_NAs(price_clean)

# convert the price data to wide format, assets are the cols and the dates are the rows
price_wide <- price_clean %>% pivot_wider(names_from = id, values_from = px)

head(price_wide)

# plot every asset (col) from price_wide with quantmod chartSeries()
for (i in 2:ncol(price_wide)) {
  chartSeries(price_wide[,c(1,i)], theme = chartTheme("white"), name = colnames(price_wide)[i])
}


DT_clean <- DT[DT$date >= "2023-12-31",]

# drop NA rows (WEEKEND)
DT_clean <- DT_clean[complete.cases(DT_clean),]

# rename all df date col to date (might be Date somewhere) (price clean, daily_factors and DT_clean based on date)
colnames(price_wide)[colnames(price_wide) == "Date"] <- "date"
colnames(daily_factors)[colnames(daily_factors) == "Date"] <- "date"
colnames(DT_clean)[colnames(DT_clean) == "Date"] <- "date"
#merge all df (price clean, daily_factors and DT_clean based on date) 
df_for_regrs <- merge(price_wide, daily_factors, by = "date", all = TRUE)

#chop the timeframe at the end of 2024
df_for_regrs <- df_for_regrs[df_for_regrs$date <= "2024-12-31",]
#count and plot NAs here







# Assume df_for_regrs is your original data frame with a 'date' column.
df_original <- df_for_regrs

# Create a copy to hold the imputed data
df_imputed <- df_original

# Loop over each numeric column (excluding the 'date' column)
for (col in setdiff(names(df_imputed), "date")) {
  # Extract the series
  series <- df_imputed[[col]]
  
  # Calculate the 7-day moving average (centered window)
  # na.rm = TRUE ensures that we compute the mean from available values.
  ma_values <- rollapply(series, 
                         width = 7, 
                         FUN = function(x) mean(x, na.rm = TRUE), 
                         fill = NA, 
                         align = "center")
  
  # Replace NA values with the computed moving average where available
  series_imputed <- ifelse(is.na(series) & !is.na(ma_values), ma_values, series)
  
  # For any remaining NA values at the boundaries:
  # First, fill forward
  series_imputed <- na_locf(series_imputed)
  # Then fill backward by reversing the vector, filling forward, and reversing back.
  series_imputed <- rev(na_locf(rev(series_imputed)))
  
  # Store the imputed series back in the data frame
  df_imputed[[col]] <- series_imputed
}

# Optionally, drop the last row of df_imputed if needed
df_imputed <- df_imputed[1:(nrow(df_imputed) - 1), ]

# Convert the resulting data frame to an xts object (excluding the date column)
df_final_xts <- xts(df_imputed[,-1], order.by = as.Date(df_imputed$date))

# Plot each asset (each column except date) using quantmod's chartSeries()
for (i in 2:ncol(df_imputed)) {
  chartSeries(df_imputed[, c(1, i)], 
              theme = chartTheme("white"), 
              name = colnames(df_imputed)[i])
}




# END OF SCRIPT ----
# This script downloads and cleans multiple datasets:
# - Bloomberg portfolio and benchmark data
# - U.S. T-bill rates from FRED
# - Fama-French factor data (including Momentum factor)
#
# Each section is labeled for clarity in RStudio.
# The script can be run in its entirety to download and clean the data.
# The cleaned datasets can then be used for further analysis and modeling.
