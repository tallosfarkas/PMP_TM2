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

