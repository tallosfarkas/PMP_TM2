# packages
library(pmp.engine)
library(lubridate)
library(Rblpapi)   
library(quantmod) 

# connect to BBG (Terminal must be open)
Rblpapi::blpConnect()







# =============================================================================
# SECTION 1: LOAD SAVED DATA AND PORTFOLIO DATA
# =============================================================================

# Load previously saved workspace data
load("tm2.RData")

# Examine the loaded objects
assetdes   # asset descriptions
head(DT)   # first few rows of DT
tail(DT)   # last few rows of DT
summary(DT) # summary statistics of DT

# Download portfolio data from Bloomberg using a custom function
df <- download_port_bbg(start_date = Sys.Date() - years(10))
price <- df$prices  # portfolio prices
id <- df$id         # portfolio identifiers


# =============================================================================
# SECTION 2: DOWNLOAD BLOOMBERG DATA FOR BENCHMARK ASSET CLASSES
# =============================================================================

# Set your date range for the Bloomberg data
start_date <- as.Date("2010-01-01")
end_date   <- Sys.Date()

# Define tickers and fields for Bloomberg download
tickers <- c("SPX Index", "MXWOU Index", "LUATTRUU Index")
fields  <- c("PX_LAST")  # we want the closing prices

# Download Bloomberg data for each asset using bdh() function
bloomberg_data <- lapply(tickers, function(ticker) {
  bdh(ticker, fields, start_date, end_date)
})
names(bloomberg_data) <- tickers

# Optional: Check the first few rows of the SPX Index data
head(bloomberg_data[["SPX Index"]])


# =============================================================================
# SECTION 3: DOWNLOAD U.S. T-BILL RATES FROM FRED
# =============================================================================

# "DTB3" is commonly used for 3-month T-bill rates from FRED
getSymbols("DTB3", src = "FRED", from = start_date, to = end_date)

# Inspect the first few rows of the T-bill rate data
head(DTB3)


# =============================================================================
# SECTION 4: DOWNLOAD FAMA-FRENCH FACTOR DATA FROM KEN FRENCH'S WEBSITE
# =============================================================================

# Define the URLs for the factor datasets
ff_url  <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
wml_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip"

# --- 4a. Fama-French Factors (Rm-Rf, SMB, HML) ---
# Download the Fama-French factors zip file
download.file(ff_url, destfile = "F-F_Research_Data_Factors_CSV.zip", mode = "wb")
# Unzip the downloaded file into a directory
unzip("F-F_Research_Data_Factors_CSV.zip", exdir = "FF_factors")

# Inspect the files extracted (optional)
list.files("FF_factors")

# Read the CSV file containing the factors (adjust 'skip' if needed)
ff_factors <- read.csv("FF_factors/F-F_Research_Data_Factors.CSV", skip = 3)
head(ff_factors)


# --- 4b. Momentum Factor (WML) ---
# Download the momentum factor zip file
download.file(wml_url, destfile = "F-F_Momentum_Factor_CSV.zip", mode = "wb")
# Unzip the downloaded file into a directory
unzip("F-F_Momentum_Factor_CSV.zip", exdir = "FF_momentum")

# Inspect the extracted files (optional)
list.files("FF_momentum")

# Read the CSV file containing the momentum factor (adjust 'skip' if necessary)
wml_factor <- read.csv("FF_momentum/F-F_Momentum_Factor.CSV", skip = 3)
head(wml_factor)
