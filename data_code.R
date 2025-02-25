# SECTION 0: PRELIMINARIES ----
# Load necessary packages
library(pmp.engine)
library(lubridate)
library(Rblpapi)
library(quantmod)
library(xts)

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

# Download portfolio data from Bloomberg using a custom function.
df <- download_port_bbg(start_date = Sys.Date() - years(10))
price <- df$prices  # Portfolio prices
id <- df$id         # Portfolio identifiers


# SECTION 2: DOWNLOAD BLOOMBERG DATA FOR BENCHMARK ASSET CLASSES ----
# Define date range for Bloomberg data
start_date <- as.Date("2010-01-01")
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


# END OF SCRIPT ----
# This script downloads and cleans multiple datasets:
# - Bloomberg portfolio and benchmark data
# - U.S. T-bill rates from FRED
# - Fama-French factor data (including Momentum factor)
#
# Each section is labeled for clarity in RStudio.
