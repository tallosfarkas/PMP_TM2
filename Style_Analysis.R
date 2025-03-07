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






