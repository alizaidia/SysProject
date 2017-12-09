install.packages("readxl")
library(readxl)


d2017_financial <- as.data.frame(read_excel("Financial_Futures_COT_2017.xls"))
d2006_financial <- as.data.frame(read_excel("Financial_Futures_COT_2006_2016.xls"))
cot <- rbind(d2017_financial, d2006_financial)
min(cot$Open_Interest_All)
# I am pretty sure all the products in this file are heavily traded. No needed to check for volumes. The minimum OI across all the entire file is 3100. Hence, heavily traded.
 # The columns are pretty similar are to the file in commodities. The merchant columns are replaced with leveraged positions columns. Not analyzing all the columns again
 ncol(cot)
# The original data set had 84 columns initially before manipulation
# I manipulated column names to ensure most of the column names are consistent across both the data sets
cot$name <- cot$Market_and_Exchange_Names
cot$date <- cot$Report_Date_as_MM_DD_YYYY
cot$OI <- cot$Open_Interest_All
cot$leverage_long <- cot$Pct_of_OI_Lev_Money_Long_All
cot$leverage_short <- cot$Pct_of_OI_Lev_Money_Short_All
cot$leverage_spread <- cot$Pct_of_OI_Lev_Money_Spread_All
cot$swap_long <- cot$Pct_of_OI_Dealer_Long_All
cot$swap_short <- cot$Pct_of_OI_Dealer_Short_All
cot$swap_spread <- cot$Pct_of_OI_Dealer_Spread_All
cot$manager_long <- cot$Pct_of_OI_Asset_Mgr_Long_All
cot$manager_short <- cot$Pct_of_OI_Asset_Mgr_Short_All
cot$manager_spread <- cot$Pct_of_OI_Asset_Mgr_Spread_All
cot$otherreportable_long <- cot$Pct_of_OI_Other_Rept_Long_All
cot$otherreportable_short <- cot$Pct_of_OI_Other_Rept_Short_All
cot$otherreportable_spread <- cot$Pct_of_OI_Other_Rept_Spread_All
cot$low_volume_traders_long <- cot$Pct_of_OI_NonRept_Long_All
cot$low_volume_traders_short <- cot$Pct_of_OI_NonRept_Short_All
cot$big8_long <- cot$Conc_Net_LE_8_TDR_Long_All
cot$big8_short <- cot$Conc_Net_LE_8_TDR_Short_All

# But do many columns contain NA's in this data set too just like the commodity one? Let's check that
colSums(is.na(cot))
# Change_in_Open_Interest_All: 205
# Many of these change (from last week to this week) columns have 205 missing values. Don't know why. It doesn't matter because I am definitely not going to use these columns.
# All the traders have 1000's of missing values (Just like the previous data set). But the total traders columns are not missing (once again, just like the previous data set).
# I want to look at the total traders columns again. Is the time series effect really bad?
plot(cot$Report_Date_as_MM_DD_YYYY, cot$Traders_Tot_Rept_Long_All)
# the graph is a bit confusing but the pattern is clear: total number of traders depends on time (Obviously, the increase in liquidity correlates with rise in stock markets, no. of hedge funds and thus total number of traders)
# Hence, not using these columns. These columns are not worth it to get caught up in time series analysis. We already have the aggregate positions.
cot_focus <-  cot[, 85:103]
# the COT data set with the columns we are actually going to use
# I am certainly not going to worry about about time series in this data set.
# I am now going to download the data sets for each of the products and understand the products better
# I noticed there is a Euro FX / British GBP column. Not Removing this. Because speculators might just bet on euro and GBP rather than betting on both EUR/USD and GBP/USD
# But I need to do something about dow jones index columns. We have a two dow jones columns (based on contract size although the underlying asset is the same). Hence, I wish to build a composite dow jones column that is weighted by respective value of contracts traded (which is price multiplied by volume (volume is same as OI))
# I will compute such weights for certain variables


# But there is some more data cleaning
# I realized midway through the project that some weeks (rows) data is missing




dow_jones_larger <- subset(cot_focus, cot$Market_and_Exchange_Names == 'DJIA Consolidated - CHICAGO BOARD OF TRADE')
nrow(dow_jones_larger) # 383
dow_jones_smaller <- subset(cot_focus, cot$Market_and_Exchange_Names == 'DOW JONES INDUSTRIAL AVG- x $5 - CHICAGO BOARD OF TRADE')
nrow(dow_jones_smaller) # 592
# The length's don't match? omg

# Just like commodities data set, not all products were being traded on CME back in 2006
# I am still going to weight them by value of contracts traded (from the point when the 2nd product started trading). No change in earlier plan
# I do not intend to develop a weighting for all US stock indices to create a composite us stock market index. Index trading is a field of its own and our very own legendary Jaffray woodriff trades indices in a massive way.
k <- min(c(nrow(dow_jones_larger), nrow(dow_jones_smaller)))

bigger = 2
# bigger = 2 means the value of the large contract is two times more valuable than the small contract
DJIA_ratio <- bigger*dow_jones_larger$OI[1:k]/(bigger*dow_jones_larger$OI[1:k] + dow_jones_smaller$OI[1:k])
mean(DJIA_ratio)
sd(DJIA_ratio)
# something really weird: mean of the DJIA contracts open interest ratio is 0.51 and sd is just 0.0237. Why are the values so consistent across years. What does this mean? Let me just save this column. I have a feeling about this ratio. Also, its just odd that the money is split equally between both the products.
# I am not going to write a function. There are barely 5 cases where wweighing needs to be done. I want to look at them on an individual basis
# the S & P 500 certainly doesn't make any sense. Because the contract size is also same. But I am going to follow the framework described above. Please refer the above block of code for explanation of the process and comments.
SP500_larger <- subset(cot_focus, cot$Market_and_Exchange_Names == "S&P 500 Consolidated - CHICAGO MERCANTILE EXCHANGE")
SP500_smaller <- subset(cot_focus, cot$Market_and_Exchange_Names == "S&P 500 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE")
# But there's another S&P500 that is 5 times smaller than these two. Need to adjust for that as well
# I knew unexpected things like would like would happen. that's why I didn't write function!
SP500_mini <- subset(cot_focus, cot$Market_and_Exchange_Names == "E-MINI S&P 500 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE")
k <- min(c(nrow(SP500_larger), nrow(SP500_smaller), nrow(SP500_mini)))
bigger = 5
SP500_larger_ratio <- bigger*SP500_larger$OI[1:k]/(bigger*SP500_larger$OI[1:k] + bigger*SP500_smaller$OI[1:k] + SP500_mini$OI[1:k])
SP500_smaller_ratio <- bigger*SP500_smaller$OI[1:k]/(bigger*SP500_larger$OI[1:k] + bigger*SP500_smaller$OI[1:k] + SP500_mini$OI[1:k])
SP500_mini_ratio <- bigger*SP500_mini$OI[1:k]/(bigger*SP500_larger$OI[1:k] + bigger*SP500_smaller$OI[1:k] + SP500_mini$OI[1:k])
# The next one we are going to weight is NASDAQ 100 (2 contracts - large value and small value)
# I promise I have a reason to try the weights
NASDAQ100_larger <- subset(cot_focus, cot$Market_and_Exchange_Names == 'NASDAQ-100 Consolidated - CHICAGO MERCANTILE EXCHANGE')
NASDAQ100_smaller <- subset(cot_focus, cot$Market_and_Exchange_Names == 'NASDAQ-100 STOCK INDEX (MINI) - CHICAGO MERCANTILE EXCHANGE')
k <- min(c(nrow(NASDAQ100_larger), nrow(NASDAQ100_smaller)))
bigger = 5
NASDAQ100_ratio <- bigger*NASDAQ100_larger$OI[1:k]/(bigger*NASDAQ100_larger$OI[1:k] + NASDAQ100_smaller$OI[1:k])

# There are two Russell 2000's. Both are equal in value. But one is traded on CME and another on ICE. Does it matter? We will know soon. Collecting the ratios
Russell2000_larger <- subset(cot_focus, cot$Market_and_Exchange_Names == 'E-MINI RUSSELL 2000 INDEX - CHICAGO MERCANTILE EXCHANGE')
Russell2000_smaller <- subset(cot_focus, cot$Market_and_Exchange_Names == 'RUSSELL 2000 MINI INDEX FUTURE - ICE FUTURES U.S.')
k <- min(c(nrow(Russell2000_larger), nrow(Russell2000_smaller)))
bigger = 1
Russell2000_ratio <- bigger*Russell2000_larger$OI[1:k]/(bigger*Russell2000_larger$OI[1:k] + Russell2000_smaller$OI[1:k])


unique(d2017$Market_and_Exchange_Names)

# Unfortunately, I couldn't find a data set for Dollar denominated Nikkei futures. But I was able to find a data set for Nikkei futures in japanese yen. Just deleting all the dollar Nikkei futures rows
cot_focus <- cot_focus[!(cot_focus$name == "NIKKEI STOCK AVERAGE - CHICAGO MERCANTILE EXCHANGE"),]
# Unfortunately, I can't find the data set for S&P 500 ANNUAL DIVIDEND INDEX - CHICAGO MERCANTILE EXCHANGE. That would have really interesting to look that. Have to delete that columns
cot_focus <- cot_focus[!(cot_focus$name == 'S&P 500 ANNUAL DIVIDEND INDEX - CHICAGO MERCANTILE EXCHANGE'),]
nrow(cot_focus)
# Just 17 rows. Oh, its a new product launched this year! Great idea! I would love to look at it few years later
# The next product is U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE. this is a composite of all short term US Treasury bonds. I don't want to study this column because I see specific US treasury bonds below (2 year, 5 year etc.). Moreover, the open interest (trading volume) is a fraction relative to the OI of the specific products.
cot_focus <- cot_focus[!(cot_focus$name == 'U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE'),]
# Next trading product is ULTRA U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE. This is a composite of long term US treasury bonds. For the same reason given above, I am not going to use this column either for the same reason stated above
cot_focus <- cot_focus[!(cot_focus$name == 'ULTRA U.S. TREASURY BONDS - CHICAGO BOARD OF TRADE'),]
# I am also deleting rows of ULTRA 10-YEAR U.S. T-NOTES - CHICAGO BOARD OF TRADE. We already have 10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE. too much choice!
cot_focus <- cot_focus[!(cot_focus$name == 'ULTRA 10-YEAR U.S. T-NOTES - CHICAGO BOARD OF TRADE'),]
# I can't find the right data for 30 day federal fund rate. Something is weird in the price data set. Let me just exclude this.
cot_focus <- cot_focus[!(cot_focus$name == '30-DAY FEDERAL FUNDS - CHICAGO BOARD OF TRADE'),]
# I can't find any data set for the swaps. Also, I am pretty sure CME launched them recently. I am not sure how much value these will add anyway considering I am already looking at T bills at multiple maturities
cot_focus <- cot_focus[!(cot_focus$name == '10 YEAR DELIVERABLE IR SWAP - CHICAGO BOARD OF TRADE'),]
cot_focus <- cot_focus[!(cot_focus$name == '5 YEAR DELIVERABLE IR SWAP - CHICAGO BOARD OF TRADE'),]
# Just to clarify, the bloomberg commodity index is a composite of many popular commodities. We are going to analyze BCOM index just like another product in this data set (We don't want to mix this up with the other individual commodity data set we are working on)
# Time to load in the prices for all the data sets
unique(cot_focus$name)
# There are actually 59 products. Some of these are actually trading now. I don't know why I can't find or scrape COT forms for these. Products not important enough to be tracked? (not on main boards?) But this is the
# Actually, what are the products trading presently?
trading_now <- unique(d2017$Market_and_Exchange_Names)
length(trading_now)
# Actually, some of these products have been removed this year
# And after adjusting for all these duplicate products and the deleting unimportant ones, we get a final list of 29
# Using a function similar to the function used in commodity data set cleaning to join the price data set
# But first, we need to read in the 29 price data sets
# CAD_USD_weekly <- read.csv("CAD USD Historical Data.csv")
# I just realized something. Since you can look at all files inside working directory, you just need to hit import and R will automatically load in the data frame (and you can rename the file name also while importing)
# Oh wait. I can't run this script from top to bottom and get all price data sets if I take the shortcut this time. So, I will have to manually read them

AUD_USD_weekly <- read.csv("AUD USD Historical Data.csv")
BCOM_weekly <- read.csv("Bloomberg Commodity Historical Data.csv")
BRL_USD_weekly <- read.csv("BRL USD Historical Data.csv")
CAD_USD_Weekly <- read.csv("CAD USD Historical Data.csv")
MXN_USD_weekly <- read.csv("MXN USD Historical Data.csv")
CHF_USD_weekly <- read.csv("CHF USD Historical Data.csv")
DJIA_weekly <- read.csv("Dow 30 Futures Historical Data.csv")
EUR_GBP_weekly <- read.csv("EUR GBP Historical Data.csv")
EUR_USD_weekly <- read.csv("EUR USD Historical Data.csv")
Eurodollar_LIBOR_weekly <- read.csv("Eurodollar Futures Historical Data.csv")
GBP_USD_weekly <- read.csv("GBP USD Historical Data.csv")
JPY_USD_weekly <- read.csv("JPY USD Historical Data.csv")
MSCI_EAFE_weekly <- read.csv("MSCI International EAFE Net Historical Data.csv")
MSCI_EMEA_weekly <- read.csv("MSCI Emerging Markets Historical Data.csv")
Nasdaq100_weekly <- read.csv("Nasdaq Futures Historical Data.csv")
Nikkei225_weekly <- read.csv("Nikkei 225 Futures Historical Data.csv")
NZD_USD_weekly <- read.csv("NZD USD Historical Data.csv")
RUB_USD_weekly <- read.csv("RUB USD Historical Data.csv")
SP500_Consumer_Staples_weekly <- read.csv("S&amp%3BP 500 Consumer Staples Historical Data.csv")
SP500_Financials_weekly <- read.csv("S&amp%3BP 500 Financials Historical Data.csv")
SP500_weekly <- read.csv("S&amp%3BP 500 Futures Historical Data.csv")
SP500_Utilities_weekly <- read.csv("S&amp%3BP 500 Utilities Historical Data.csv")
SP500_VIX_weekly <- read.csv("S&amp%3BP 500 VIX Futures Historical Data.csv")
SP400_weekly <- read.csv("S&amp%3BP MidCap 400 Futures Historical Data.csv")
RUSEELL2000_weekly <- read.csv("SmallCap 2000 Futures Historical Data.csv")
USTbill_10Y_weekly <- read.csv("US 10 Year T-Note Futures Historical Data.csv")
USTbill_5Y_weekly <- read.csv("US 5 Year T-Note Futures Historical Data.csv")
USTbill_2Y_weekly <- read.csv("US 2 Year T-Note Futures Historical Data.csv")
USD_index_weekly <- read.csv("US Dollar Index Futures Historical Data.csv")
ZAR_USD_weekly <- read.csv("ZAR USD Historical Data.csv")


length(DJIA_ratio)


# But before I join the price, data sets, I intend to explore if the earlier computed ratios for various ratios can explain the % change in any way (because I need to know how to incorporate weights into my analysis. this is a
cor(DJIA_ratio, DJIA_weekly$Change..[1:length(DJIA_ratio)])
cor(DJIA_ratio, abs(DJIA_weekly$Change..[1:length(DJIA_ratio)])) 
cor(SP500_larger_ratio, SP500_weekly$Change..[1:length(SP500_larger_ratio)]) 
cor(SP500_larger_ratio, abs(SP500_weekly$Change..[1:length(SP500_larger_ratio)])) 
cor(SP500_mini_ratio, SP500_weekly$Change..[1:length(SP500_mini_ratio)]) 
cor(SP500_mini_ratio, abs(SP500_weekly$Change..[1:length(SP500_mini_ratio)])) 
cor(NASDAQ100_ratio, Nasdaq100_weekly$Change..[1:length(NASDAQ100_ratio)])
cor(NASDAQ100_ratio, abs(Nasdaq100_weekly$Change..[1:length(NASDAQ100_ratio)]))

#cor(Rusell2000_ratio, Russell2000_weekly$Change..[1:length(Rusell2000_ratio)]) # -0.17
#cor(Rusell2000_ratio, abs(Russell2000_weekly$Change..[1:length(Rusell2000_ratio)])) # -0.29 Interesting
# Actually, lets not worry about Russell cause I noticed CME only introduced the mini contract. we have just 10 observations. The other three have 384 observations each


# We actually explored in detail if the product choice of investors (mini or large) can predict price in both regression and classification setting (with gini index)

# We can absolutely confirm that which product investors are buying doesn't affect weekly change


library(lubridate)
price_add <- function(x,y) {
  # I learnt it the hard way that we should do as.character first and then as.numeric. Else, we will get a data frame but since the prices are coded as factors, the numbers out of as.numeric are something random and are not the real prices
  # I noticed that the last 2 rows of these weekly return files are actually summary statistics like mean, median, mode etc. removing them for all weekly returns files
  y <- head(y, -2)
  # I checked the series, codes and downloaded data from investing.com. But noticed that sometimes the top row is Oct 15 (which is price as of Oct 15 - Oct 22). For some others the top row is Oct 22 which is actually returns from Oct 22 - Oct 29 week. But its not friday yet.
  # wheatSRW_weekly$ï..Date[1] == 'Oct 22, 2017' : the date format in the downloaded data sets
  if (y[1,1] == 'Oct 22, 2017') {
    y <- y[2:nrow(y), ]
  }
  # Sometimes investing.com's price data set is sometimes shorter than COT data set. Need to take care of that. Only for few commodities anyway
  if (nrow(y) < nrow(x)) {
    x <- x[1:nrow(y), ]
  }
  if (nrow(y) > nrow(x)) {
    y <- y[1:nrow(x), ]
  }
  z = x
  x <- data.frame(apply(x, 2, function(l) as.numeric(as.character(l))))
  y <- data.frame(apply(y, 2, function(l) as.numeric(as.character(l))))
  # The above code will also generate some errors as we run the code because we have date, name columns
  final <- cbind(x, y$Change..)
  colnames(final)[20] <- "act_change"
  final$name = z$name
  final$date = z$date
  # Also, I decided to add some indicator variables for the spread columns. Can't hurt the predictions
  final$ind_lev_spread <- ifelse(final$leverage_spread > 1, 1, 0)
  final$ind_swap_spread <- ifelse(final$swap_spread > 1, 1, 0)
  final$ind_manager_spread <- ifelse(final$manager_spread > 1, 1, 0)
  final$ind_other_report_spread <- ifelse(final$otherreportable_spread > 1, 1, 0)
  final$weekly_change <- final$act_change
  final$act_change <- NULL
  return(final)
}

# Joining percentage changes for all the financial futures with the above function
CAD_USD_final <- price_add(subset(cot_focus, cot_focus$name == "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"), CAD_USD_Weekly)
CHF_USD_final <- price_add(subset(cot_focus, cot_focus$name == "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE" ), CHF_USD_weekly)
GBP_USD_final <- price_add(subset(cot_focus, cot_focus$name == "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE" ), GBP_USD_weekly)
JPY_USD_final <- price_add(subset(cot_focus, cot_focus$name == "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE" ), JPY_USD_weekly)
EUR_USD_final <- price_add(subset(cot_focus, cot_focus$name == "EURO FX - CHICAGO MERCANTILE EXCHANGE" ), EUR_USD_weekly)
RUB_USD_final <- price_add(subset(cot_focus, cot_focus$name == "RUSSIAN RUBLE - CHICAGO MERCANTILE EXCHANGE" ), RUB_USD_weekly)
MXN_USD_final <- price_add(subset(cot_focus, cot_focus$name == "MEXICAN PESO - CHICAGO MERCANTILE EXCHANGE" ), MXN_USD_weekly)
BRL_USD_final <- price_add(subset(cot_focus, cot_focus$name == "BRAZILIAN REAL - CHICAGO MERCANTILE EXCHANGE" ), BRL_USD_weekly)
NZD_USD_final <- price_add(subset(cot_focus, cot_focus$name == "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE" ), NZD_USD_weekly)
ZAR_USD_final <- price_add(subset(cot_focus, cot_focus$name == "SOUTH AFRICAN RAND - CHICAGO MERCANTILE EXCHANGE" ), ZAR_USD_weekly)

# Now coming to the DJIA index (that has multiple value contracts for the same underlying asset)
# let me write a function to clean these indices separately (the ones with multiple contract values)
# I am not going to comment too much. The function is pretty similar to the functions I have been using
# For products like these, I am going to collect the OI column as well. I just think it might be useful later ()
weighted_by_value_data_set <- function(name1, name2, bigger, the_new_name_defined) {
# V.IMP: enter the larger contract as name 1
index1 <- subset(cot_focus, cot_focus$name == name1) # I repeat the larger contract first
index2 <- subset(cot_focus, cot_focus$name == name2)
k = min(c(nrow(index1), nrow(index2)))
value_ratio <- bigger*index1$OI[1:k]/(bigger*index1$OI[1:k] + index2$OI[1:k])
if (nrow(index1) > nrow(index2)) {
longer = index1
}  else {
longer = index2
}
if (nrow(index1) < nrow(index2)) {
shorter = index1
}  else {
shorter = index2
}
# Just because a contract is larger, doesn't mean it was trading from earlier. Some of the mini's were trading much earlier on main boards
longer$name = the_new_name_defined
for (i in 1:nrow(shorter)) {
for (j in 4:ncol(longer))
longer[i,j] = (value_ratio[i]*longer[i,j]) + ((1 - value_ratio[i])*shorter[i,j])
# You don't have to adjust for the name and date
# I will adjust for OI in a different way later
}
# But the OI column should not be averaged like the Pct. of OI columns. Tricky concept but think about it. it should be added
for (i in 1:nrow(shorter)) {
longer[i,3] = longer[i,3] + ((1 - value_ratio[i])*shorter[i,3])
# Special loop to take the cummulative OI and not the average (obviously adjuted for value of contract just like the other columns)
}
return(longer)
}
# Lets see if this function works
DJIA_COT <- weighted_by_value_data_set("DJIA Consolidated - CHICAGO BOARD OF TRADE", "DOW JONES INDUSTRIAL AVG- x $5 - CHICAGO BOARD OF TRADE", 2, "DJIA")
DJIA_final <- price_add(DJIA_COT, DJIA_weekly)


#  I remember 2 of the 3 SP500 indices are exactly the same volume. What I am going to do is adjust the index step by step
# Adding the largest 2 contract value indices first

SP500_COT_temp <- weighted_by_value_data_set("S&P 500 Consolidated - CHICAGO MERCANTILE EXCHANGE", "S&P 500 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE", 1, "SP500_temp")
cot_focus = rbind(cot_focus, SP500_COT_temp)
# Now I will use this data frame to join with the 3rd index
SP500_COT <- weighted_by_value_data_set("SP500_temp", "E-MINI S&P 500 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE", 5, "SP500")
SP500_final <- price_add(SP500_COT, SP500_weekly)

SP500_final <- SP500_final[1:592, ]

RUSSELL2000_COT <- weighted_by_value_data_set("E-MINI RUSSELL 2000 INDEX - CHICAGO MERCANTILE EXCHANGE", "RUSSELL 2000 MINI INDEX FUTURE - ICE FUTURES U.S.", 1, "RUSSELL2000")
RUSSELL2000_final <- price_add(RUSSELL2000_COT, RUSSELL2000_weekly)

NASDAQ100_COT <- weighted_by_value_data_set("NASDAQ-100 Consolidated - CHICAGO MERCANTILE EXCHANGE", "NASDAQ-100 STOCK INDEX (MINI) - CHICAGO MERCANTILE EXCHANGE", 5, "NASDAQ100")
NASDAQ100_final <- price_add(NASDAQ100_COT, Nasdaq100_weekly)


BCOM_final <- price_add(subset(cot_focus, cot_focus$name == "BLOOMBERG COMMODITY INDEX - CHICAGO BOARD OF TRADE"), BCOM_weekly)

EUR_GBP_final <- price_add(subset(cot_focus, cot_focus$name == "EURO FX/BRITISH POUND XRATE - CHICAGO MERCANTILE EXCHANGE"), EUR_GBP_weekly)

Eurodollar_LIBOR_final <- price_add(subset(cot_focus, cot_focus$name == "3-MONTH EURODOLLARS - CHICAGO MERCANTILE EXCHANGE"), Eurodollar_LIBOR_weekly)

MSCI_EMEA_final <- price_add(subset(cot_focus, cot_focus$name == "MSCI EMERGING MKTS MINI INDEX - ICE FUTURES U.S."), MSCI_EMEA_weekly)

MSCI_EAFE_final <- price_add(subset(cot_focus, cot_focus$name == "MSCI EAFE MINI INDEX - ICE FUTURES U.S."), MSCI_EAFE_weekly)

Nikkei225_final <- price_add(subset(cot_focus, cot_focus$name == "NIKKEI STOCK AVERAGE YEN DENOM - CHICAGO MERCANTILE EXCHANGE"), Nikkei225_weekly)

SP500_VIX_final <- price_add(subset(cot_focus, cot_focus$name == "VIX FUTURES - CBOE FUTURES EXCHANGE"), SP500_VIX_weekly)

US.Bond_2Y_final <- price_add(subset(cot_focus, cot_focus$name == "2-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE" ), USTbill_2Y_weekly)

US.Bond_5Y_final <- price_add(subset(cot_focus, cot_focus$name == "5-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE" ), USTbill_5Y_weekly)

US.Bond_10Y_final <- price_add(subset(cot_focus, cot_focus$name == "10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE" ), USTbill_10Y_weekly)

USD_index_final <- price_add(subset(cot_focus, cot_focus$name == "U.S. DOLLAR INDEX - ICE FUTURES U.S." ), USD_index_weekly)

SP400_final <- price_add(subset(cot_focus, cot_focus$name == "E-MINI S&P 400 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE" ), SP400_weekly)

AUD_USD_final <- price_add(subset(cot_focus, cot_focus$name == "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE" ), AUD_USD_weekly)

# some rows are missing for some of the products. So, I had to cut the files. The function is in commodities data cleaning script

ZAR_USD_final <- ZAR_USD_final[1:44, ]
EUR_GBP_final <- EUR_GBP_final[1:4, ]
BRL_USD_final <- BRL_USD_final[1:126, ]
RUB_USD_final <- RUB_USD_final[1:182, ]



# Studying the skew in the data sets
# The features we engineered are actually based on what we saw in the data distributions

normal <- function(x) {
  diff_btw_mean_median <- c()
  data_within_one_s.d <- c()
  data_within_two_s.d <- c()
  for (i in 4:19) {
    mean_x = mean(x[, i])
    median_x = median(x[, i])
    # mode_x = mode(x[, i]) # Never mind. I messed up. Mode is not so relavent here
    diff_btw_mean_median = c(diff_btw_mean_median, (abs(mean_x - median_x))*100/mean_x)
    data_within_one_s.d = c(data_within_one_s.d, abs((sum(x[, i] > (mean(x[, i]) - sd(x[, i])) & x[, i] < (mean(x[, i]) + sd(x[, i])))/nrow(x)) - 0.68)*100/0.68)
    data_within_two_s.d = c(data_within_two_s.d, abs((sum(x[, i] > (mean(x[, i]) - 2*sd(x[, i])) & x[, i] < (mean(x[, i]) + 2*sd(x[, i])))/nrow(x)) - 0.95)*100/0.95)
  }
  diff_btw_mean_median = t(as.data.frame(diff_btw_mean_median))
  data_within_one_s.d = t(as.data.frame(data_within_one_s.d))
  data_within_two_s.d = t(as.data.frame(data_within_two_s.d))
  colnames_list <- colnames(x[4:19])
  colnames(diff_btw_mean_median) <- colnames_list
  colnames(data_within_one_s.d) <- colnames_list
  colnames(data_within_two_s.d) <- colnames_list
  normal_metrics <- c(diff_btw_mean_median, data_within_one_s.d, data_within_two_s.d)
  # The spread columns often have many 0's, erratic values (there is a business reason behind this also). Hence, it may make sense to look if atleast the columns without spread are normally distributed
  normal_metrics_withspread <- c(mean(diff_btw_mean_median), mean(data_within_one_s.d), mean(data_within_two_s.d))
  nospread_columns <- c(1,2,4,5,7,8,10,11,13,14,15,16)
  normal_metrics_withoutspread <- c(mean(diff_btw_mean_median[, nospread_columns]), mean(data_within_one_s.d[, nospread_columns]), mean(data_within_two_s.d[, nospread_columns]))
  metrics <- c(normal_metrics_withspread, normal_metrics_withoutspread)
  metrics = t(as.data.frame(metrics))
  colnames(metrics) <- c("avg_diff_btw_mean_median", "diff_at_1S.D_level", "%diff_at_2S.D_level", "avg_diff_btw_mean_median(nospread)", "diff_at_1S.D_level(nospread)", "%diff_at_2S.D_level(nospread)")
  return(metrics)
}


currency_normality <- rbind(normal(CAD_USD_final), normal(CHF_USD_final), normal(GBP_USD_final), normal(JPY_USD_final), normal(EUR_GBP_final), normal(EUR_USD_final), normal(AUD_USD_final), normal(EUR_GBP_final), normal(RUB_USD_final), normal(MXN_USD_final), normal(BRL_USD_final), normal(NZD_USD_final), normal(ZAR_USD_final), normal(USD_index_final))






# We want to combine few data sets and then build a model. The strategies are explained in detail in the report

# strategy 1

value <- function(data, contract_value, latest_exchange_rate) {
  data$total_change <- NA
  data$total_change[1] = 1
  for (i in 2:nrow(data)) {
    data$total_change[i] = (1 + (data$weekly_change[i]/100))*(data$total_change[i - 1])
  }
  data$exchange_rates <- NA
  data$exchange_rates[1] = latest_exchange_rate
  for (i in 2:nrow(data)) {
    data$exchange_rates[i] <- latest_exchange_rate/data$total_change[i]
  }
  for (j in 1:nrow(data)) {
    data$total_product_value[j] = contract_value*data$exchange_rate[j]*data$OI[j]
  }
  data <- data[order(data$date, decreasing = TRUE),]
  data$monthly_product_value <- NA
  if (nrow(data) < 4) {
    data$monthly_product_value <- data$total_product_value
  } else {
  
      for (i in 1:(nrow(data)-4)) {
      z <- data[i:(i+3), ]
      data$monthly_product_value[i] <- (sum(z$total_product_value)/4)
    }
    for (i in (nrow(data) - 3): nrow(data)) {
      data$monthly_product_value[i] = data$total_product_value[i]
    }
  }
  data$quarterly_product_value <- NA
  if (nrow(data) < 13) {
    data$quarterly_product_value <- data$total_product_value
  } else {
    for (i in 1:(nrow(data)-13)) {
      z <- data[i:(i+12), ]
      data$quarterly_product_value[i] <- sum(z$total_product_value)/13
    }
    for (i in (nrow(data) - 12): nrow(data)) {
      data$quarterly_product_value[i] = data$total_product_value[i]
    }
  }
  data$yearly_product_value <- NA
  if (nrow(data) < 52) {
    data$yearly_product_value <- data$total_product_value
  } else {
    for (i in 1:(nrow(data)-52)) {
      z <- data[i:(i+51), ]
      data$yearly_product_value[i] <- sum(z$total_product_value)/52
    }
    for (i in (nrow(data) - 51): nrow(data)) {
      data$yearly_product_value[i] = data$total_product_value[i]
    }
  }
  data$value_change_4 <- NA
  for (i in 1:nrow(data)) {
    data$value_change_4[i] <- (data$total_product_value[i] - data$monthly_product_value[i])*100/data$monthly_product_value[i]
  }
  data$value_change_13 <- NA
  for (i in 1:nrow(data)) {
    data$value_change_13[i] <- (data$total_product_value[i] - data$quarterly_product_value[i])*100/data$quarterly_product_value[i]
  }
  data$value_change_52 <- NA
  for (i in 1:nrow(data)) {
    data$value_change_52[i] <- (data$total_product_value[i] - data$yearly_product_value[i])*100/data$yearly_product_value[i]
  }
  
  data$monthly_product_value <- NULL
  data$quarterly_product_value <- NULL
  data$yearly_product_value <- NULL
  data$total_change <- NULL
  data$exchange_rates <- NULL
  
  return(data)
}


CAD_USD_value <- value(CAD_USD_final, 0.1, 0.79)
CHF_USD_value <- value(CHF_USD_final, 0.125, 1.02)
GBP_USD_value <- value(GBP_USD_final, 0.1, 1.32)
JPY_USD_value <- value(JPY_USD_final, 12.5, 0.0089)
EUR_GBP_value <- value(EUR_GBP_final, 0.125, 0.90)
EUR_USD_value <- value(EUR_USD_final, 0.125, 1.19)
AUD_USD_value <- value(AUD_USD_final, 0.1, 0.76)
RUB_USD_value <- value(RUB_USD_final, 2.5, 0.017)
MXN_USD_value <- value(MXN_USD_final, 0.5, 0.054)
BRL_USD_value <- value(BRL_USD_final, 0.1, 0.31)
NZD_USD_value <- value(NZD_USD_final, 1, 0.71)
ZAR_USD_value <- value(ZAR_USD_final, 0.5, 0.075)
USD_index_value <- value(USD_index_final, 0.1, 0.93)
US.Bond_2Y_value <- value(US.Bond_2Y_final, 0.2, 1)
US.Bond_5Y_value <- value(US.Bond_5Y_final, 0.1, 1)
US.Bond_10Y_value <- value(US.Bond_10Y_final, 0.1, 1)
DJIA_value <- value(DJIA_final, 0.23, 1)
NASDAQ100_value <- value(NASDAQ100_final, 0.065, 1)
RUSSELL2000_value <- value(RUSSELL2000_final, 0.075, 1)
SP400_value <- value(SP400_final, 0.2, 1)
SP500_value <- value(SP500_final, 0.5, 1)
Eurodollar_LIBOR_value <- value(Eurodollar_LIBOR_final, 1, 1)
BCOM_value <- value(BCOM_final, 0.1, 1)
SP500_VIX_value <- value(SP500_VIX_final, 0.02, 1) # VIX contract is really cheap. $20,000 per contract
MSCI_EAFE_value <- value(MSCI_EAFE_final, 0.1, 1)
MSCI_EMEA_value <- value(MSCI_EMEA_final, 0.05, 1) # The EMEA is also cheap
Nikkei225_value <- value(Nikkei225_final, 0.022, 1) # approximate

currency_s1 <- rbind(CAD_USD_value, CHF_USD_value, GBP_USD_value, JPY_USD_value, EUR_GBP_value, EUR_USD_value, AUD_USD_value, RUB_USD_value, MXN_USD_value, BRL_USD_value, NZD_USD_value, ZAR_USD_value, USD_index_value)

bonds_s1 <- rbind(US.Bond_2Y_value, US.Bond_5Y_value, US.Bond_10Y_value)

US_stocks_s1 <- rbind(DJIA_value, SP500_value, SP400_value, NASDAQ100_value, RUSSELL2000_value)

all_financials_s1 <- rbind(currency_s1, bonds_s1, US_stocks_s1, Nikkei225_value, MSCI_EAFE_value, MSCI_EMEA_value, Eurodollar_LIBOR_value, BCOM_value, SP500_VIX_value)




relative_percnt_change_added <- function(data) {
  products_total_value <- aggregate(data$total_product_value, list(data$date), sum)
  # for example, We see very low aggregate currency trading OI (less than $50 billion) during financial crisis. Recent weeks aggregate's OI are nearly $250 Billion
  # I need to get time averages columns I got earlier
  colnames(products_total_value) <- c("date", "agg_value")
  products_total_value <- products_total_value[order(products_total_value$date, decreasing = TRUE),]
  products_total_value$monthly_agg_value <- NA
  for (i in 1:(nrow(products_total_value)-4)) {
    z <- products_total_value[i:(i+3), ]
    products_total_value$monthly_agg_value[i] <- (sum(z$agg_value)/4)
  }
  for (i in (nrow(products_total_value) - 3): nrow(products_total_value)) {
    products_total_value$monthly_agg_value[i] = products_total_value$agg_value[i]
  }
  products_total_value$quarterly_agg_value <- NA
  for (i in 1:(nrow(products_total_value)-13)) {
    z <- products_total_value[i:(i+12), ]
    products_total_value$quarterly_agg_value[i] <- (sum(z$agg_value)/13)
  }
  for (i in (nrow(products_total_value) - 12): nrow(products_total_value)) {
    products_total_value$quarterly_agg_value[i] = products_total_value$agg_value[i]
  }
  products_total_value$yearly_agg_value <- NA
  for (i in 1:(nrow(products_total_value)-52)) {
    z <- products_total_value[i:(i+51), ]
    products_total_value$yearly_agg_value[i] <- (sum(z$agg_value)/52)
  }
  for (i in (nrow(products_total_value) - 51): nrow(products_total_value)) {
    products_total_value$yearly_agg_value[i] = products_total_value$agg_value[i]
  }
  products_total_value$agg_change_4 <- NA
  products_total_value$agg_change_4[nrow(products_total_value)] = 0
  for (i in 1:(nrow(products_total_value) - 1)) {
    products_total_value$agg_change_4[i] <- (products_total_value$agg_value[i] - products_total_value$monthly_agg_value[i])*100/products_total_value$monthly_agg_value[i]
  }
  products_total_value$agg_change_13 <- NA
  products_total_value$agg_change_13[nrow(products_total_value)] = 0
  for (i in 1:(nrow(products_total_value)-1)) {
    products_total_value$agg_change_13[i] <- (products_total_value$agg_value[i] - products_total_value$quarterly_agg_value[i])*100/products_total_value$quarterly_agg_value[i]
  }
  products_total_value$agg_change_52 <- NA
  products_total_value$agg_change_52[nrow(products_total_value)] = 0
  for (i in 1:(nrow(products_total_value)-1)) {
    products_total_value$agg_change_52[i] <- (products_total_value$agg_value[i] - products_total_value$yearly_agg_value[i])*100/products_total_value$yearly_agg_value[i]
  }
  merged_df <- merge(x = data, y = products_total_value, by = "date")
  merged_df$monthly_agg_value <- NULL
  merged_df$quarterly_agg_value <- NULL
  merged_df$yearly_agg_value <- NULL
  
  
  for (i in 1:nrow(merged_df)) {
    
    if (merged_df$value_change_4[i] == 0) {merged_df$rel_change_4[i] = 1} else {
      merged_df$rel_change_4[i] <- (1 + ((merged_df$value_change_4[i])/100))/((1 + ((merged_df$agg_change_4[i])/100)))}
    
    if (merged_df$value_change_13[i] == 0) {merged_df$rel_change_13[i] = 1} else {
      merged_df$rel_change_13[i] <- (1 + ((merged_df$value_change_13[i])/100))/((1 + ((merged_df$agg_change_13[i])/100)))}
    
    if (merged_df$value_change_52[i] == 0) {merged_df$rel_change_52[i] = 1} else {
      merged_df$rel_change_52[i] <- (1 + ((merged_df$value_change_52[i])/100))/((1 + ((merged_df$agg_change_52[i])/100)))}
    
  }  
  
  
  
    merged_df$agg_value <- NULL
  merged_df$agg_change_4 <- NULL
  merged_df$agg_change_13 <- NULL
  merged_df$agg_change_52 <- NULL
  merged_df$total_product_value <- NULL
  return(merged_df)
}

currency_s1 <- relative_percnt_change_added(currency_s1)
bonds_s1 <- relative_percnt_change_added(bonds_s1)
US_stocks_s1 <- relative_percnt_change_added(US_stocks_s1)
all_financials_s1 <- relative_percnt_change_added(all_financials_s1)

OI_columns_adjusted <- function(data) {
for (j in 1:nrow(data)) {
for (i in 4:19) {
data[j,i] = data[j, i]*data$rel_change_4[j]
}
}
data$rel_change_4 <- NULL
data$rel_change_13 <- NULL
data$rel_change_52 <- NULL
return(data)
}

currency_s1.1 <- OI_columns_adjusted(currency_s1)
bonds_s1.1 <- OI_columns_adjusted(bonds_s1)
US_stocks_s1.1 <- OI_columns_adjusted(US_stocks_s1)
all_financials_s1.1 <- OI_columns_adjusted(all_financials_s1)

# strategy 2

currency_s2 <- currency_s1
bonds_s2 <- bonds_s1
US_stocks_s2 <- US_stocks_s1
all_financials_s2 <- all_financials_s1
  
write.csv(subset(currency_s2, currency_s2$name == "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"), "CAD_USD.csv", row.names = FALSE)
unique(currency_s2$name)
write.csv(subset(currency_s2, currency_s2$name == "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE"), "GBP_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "EURO FX - CHICAGO MERCANTILE EXCHANGE"), "EUR_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"), "AUD_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE"), "CHF_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "MEXICAN PESO - CHICAGO MERCANTILE EXCHANGE"), "MXN_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "U.S. DOLLAR INDEX - ICE FUTURES U.S."), "USD_index.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE"), "NZD_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "RUSSIAN RUBLE - CHICAGO MERCANTILE EXCHANGE"), "RUB_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE"), "JPY_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "BRAZILIAN REAL - CHICAGO MERCANTILE EXCHANGE"), "BRL_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "SOUTH AFRICAN RAND - CHICAGO MERCANTILE EXCHANGE"), "ZAR_USD.csv", row.names = FALSE)
write.csv(subset(currency_s2, currency_s2$name == "EURO FX/BRITISH POUND XRATE - CHICAGO MERCANTILE EXCHANGE"), "EUR_GBP.csv", row.names = FALSE)
write.csv(subset(US_stocks_s2, US_stocks_s2$name == "DJIA"), "DJIA30.csv", row.names = FALSE)
write.csv(subset(US_stocks_s2, US_stocks_s2$name == "NASDAQ100"), "NASDAQ100.csv", row.names = FALSE)
write.csv(subset(US_stocks_s2, US_stocks_s2$name == "E-MINI S&P 400 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE"), "SP400mini.csv", row.names = FALSE)
write.csv(subset(US_stocks_s2, US_stocks_s2$name == "SP500"), "SP500.csv", row.names = FALSE)
write.csv(subset(US_stocks_s2, US_stocks_s2$name == "RUSSELL2000"), "RUSSELL2000.csv", row.names = FALSE)
unique(bonds_s2$name)
write.csv(subset(bonds_s2, bonds_s2$name == "2-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"), "US_2Y_bond.csv", row.names = FALSE)
write.csv(subset(bonds_s2, bonds_s2$name == "5-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"), "US_5Y_bond.csv", row.names = FALSE)
write.csv(subset(bonds_s2, bonds_s2$name == "10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"), "US_10Y_bond.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "NIKKEI STOCK AVERAGE YEN DENOM - CHICAGO MERCANTILE EXCHANGE"), "Nikkei225.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "MSCI EAFE MINI INDEX - ICE FUTURES U.S."), "MSCI_EAFE.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "MSCI EMERGING MKTS MINI INDEX - ICE FUTURES U.S."), "MSCI_EMEA.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "VIX FUTURES - CBOE FUTURES EXCHANGE"), "SP500_VIX.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "BLOOMBERG COMMODITY INDEX - CHICAGO BOARD OF TRADE"), "BCOM_index.csv", row.names = FALSE)
write.csv(subset(all_financials_s2, all_financials_s2$name == "3-MONTH EURODOLLARS - CHICAGO MERCANTILE EXCHANGE"), "Eurodollar_Libor.csv", row.names = FALSE)  
  
  
# strategy 3: Normalization

normalization <- function(x) {
  z = x
  for (j in 1:nrow(x)) {
  for (i in 4:19) {
    
    mean_col = mean(z[, i])
    sd_col = sd(z[, i])
    
    x[j, i] = (z[j, i] - mean_col)/sd_col

      }
  }
  
  return(x)
}



CAD_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
CHF_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE"))
GBP_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE"))
JPY_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE"))
EUR_GBP_normalized <- normalization(subset(currency_s2, currency_s2$name == "EURO FX/BRITISH POUND XRATE - CHICAGO MERCANTILE EXCHANGE"))
EUR_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "EURO FX - CHICAGO MERCANTILE EXCHANGE"))
AUD_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
RUB_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "RUSSIAN RUBLE - CHICAGO MERCANTILE EXCHANGE"))
MXN_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "MEXICAN PESO - CHICAGO MERCANTILE EXCHANGE"))
BRL_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "BRAZILIAN REAL - CHICAGO MERCANTILE EXCHANGE"))
NZD_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
ZAR_USD_normalized <- normalization(subset(currency_s2, currency_s2$name == "SOUTH AFRICAN RAND - CHICAGO MERCANTILE EXCHANGE"))
USD_index_normalized <- normalization(subset(currency_s2, currency_s2$name == "U.S. DOLLAR INDEX - ICE FUTURES U.S."))
currency_s3 <- rbind(CAD_USD_normalized, CHF_USD_normalized, GBP_USD_normalized, JPY_USD_normalized, EUR_GBP_normalized, EUR_USD_normalized, AUD_USD_normalized, RUB_USD_normalized, MXN_USD_normalized, BRL_USD_normalized, NZD_USD_normalized, ZAR_USD_normalized, USD_index_normalized)

US.Bond_2Y_normalized <- normalization(subset(bonds_s2, bonds_s2$name == "2-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
US.Bond_5Y_normalized <- normalization(subset(bonds_s2, bonds_s2$name == "5-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
US.Bond_10Y_normalized <- normalization(subset(bonds_s2, bonds_s2$name == "10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
bonds_s3 <- rbind(US.Bond_2Y_normalized, US.Bond_5Y_normalized, US.Bond_10Y_normalized)


DJIA_normalized <- normalization(subset(US_stocks_s2, US_stocks_s2$name == "DJIA"))
NASDAQ100_normalized <- normalization(subset(US_stocks_s2, US_stocks_s2$name == "NASDAQ100"))
RUSSELL2000_normalized <- normalization(subset(US_stocks_s2, US_stocks_s2$name == "RUSSELL2000"))
SP400_normalized <- normalization(subset(US_stocks_s2, US_stocks_s2$name == "E-MINI S&P 400 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE"))
SP500_normalized <- normalization(subset(US_stocks_s2, US_stocks_s2$name == "SP500"))
US_stocks_s3 <- rbind(DJIA_normalized, NASDAQ100_normalized, RUSSELL2000_normalized, SP400_normalized, SP500_normalized)

Eurodollar_LIBOR_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "3-MONTH EURODOLLARS - CHICAGO MERCANTILE EXCHANGE"))
BCOM_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "BLOOMBERG COMMODITY INDEX - CHICAGO BOARD OF TRADE"))
SP500_VIX_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "VIX FUTURES - CBOE FUTURES EXCHANGE"))
MSCI_EAFE_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "MSCI EAFE MINI INDEX - ICE FUTURES U.S."))
MSCI_EMEA_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "MSCI EMERGING MKTS MINI INDEX - ICE FUTURES U.S."))
Nikkei225_normalized <- normalization(subset(all_financials_s2, all_financials_s2$name == "NIKKEI STOCK AVERAGE YEN DENOM - CHICAGO MERCANTILE EXCHANGE"))
all_financials_s3 <- rbind(currency_s3, bonds_s3, US_stocks_s3, Eurodollar_LIBOR_normalized, BCOM_normalized, SP500_VIX_normalized, MSCI_EAFE_normalized, MSCI_EMEA_normalized, Nikkei225_normalized)

# Strategy 4: Converting the columns to 0 - 1 scale instead of normalizing them

scaling <- function(x) {
  z = x
  for (j in 1:nrow(x)) {
    for (i in 4:19) {
      
      maxi_col = max(z[, i])
      mini_col = min(z[, i])
      range_col = maxi_col - mini_col
      
      x[j, i] = (z[j, i] - mini_col)/range_col
      
    }
  }
  
  return(x)
}

  
CAD_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
CHF_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE"))
GBP_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE"))
JPY_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE"))
EUR_GBP_scaled <- scaling(subset(currency_s2, currency_s2$name == "EURO FX/BRITISH POUND XRATE - CHICAGO MERCANTILE EXCHANGE"))
EUR_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "EURO FX - CHICAGO MERCANTILE EXCHANGE"))
AUD_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
RUB_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "RUSSIAN RUBLE - CHICAGO MERCANTILE EXCHANGE"))
MXN_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "MEXICAN PESO - CHICAGO MERCANTILE EXCHANGE"))
BRL_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "BRAZILIAN REAL - CHICAGO MERCANTILE EXCHANGE"))
NZD_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE"))
ZAR_USD_scaled <- scaling(subset(currency_s2, currency_s2$name == "SOUTH AFRICAN RAND - CHICAGO MERCANTILE EXCHANGE"))
USD_index_scaled <- scaling(subset(currency_s2, currency_s2$name == "U.S. DOLLAR INDEX - ICE FUTURES U.S."))
currency_s4 <- rbind(CAD_USD_scaled, CHF_USD_scaled, GBP_USD_scaled, JPY_USD_scaled, EUR_GBP_scaled, EUR_USD_scaled, AUD_USD_scaled, RUB_USD_scaled, MXN_USD_scaled, BRL_USD_scaled, NZD_USD_scaled, ZAR_USD_scaled, USD_index_scaled)

US.Bond_2Y_scaled <- scaling(subset(bonds_s2, bonds_s2$name == "2-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
US.Bond_5Y_scaled <- scaling(subset(bonds_s2, bonds_s2$name == "5-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
US.Bond_10Y_scaled <- scaling(subset(bonds_s2, bonds_s2$name == "10-YEAR U.S. TREASURY NOTES - CHICAGO BOARD OF TRADE"))
bonds_s4 <- rbind(US.Bond_2Y_scaled, US.Bond_5Y_scaled, US.Bond_10Y_scaled)


DJIA_scaled <- scaling(subset(US_stocks_s2, US_stocks_s2$name == "DJIA"))
NASDAQ100_scaled <- scaling(subset(US_stocks_s2, US_stocks_s2$name == "NASDAQ100"))
RUSSELL2000_scaled <- scaling(subset(US_stocks_s2, US_stocks_s2$name == "RUSSELL2000"))
SP400_scaled <- scaling(subset(US_stocks_s2, US_stocks_s2$name == "E-MINI S&P 400 STOCK INDEX - CHICAGO MERCANTILE EXCHANGE"))
SP500_scaled <- scaling(subset(US_stocks_s2, US_stocks_s2$name == "SP500"))
US_stocks_s4 <- rbind(DJIA_scaled, NASDAQ100_scaled, RUSSELL2000_scaled, SP400_scaled, SP500_scaled)

Eurodollar_LIBOR_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "3-MONTH EURODOLLARS - CHICAGO MERCANTILE EXCHANGE"))
BCOM_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "BLOOMBERG COMMODITY INDEX - CHICAGO BOARD OF TRADE"))
SP500_VIX_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "VIX FUTURES - CBOE FUTURES EXCHANGE"))
MSCI_EAFE_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "MSCI EAFE MINI INDEX - ICE FUTURES U.S."))
MSCI_EMEA_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "MSCI EMERGING MKTS MINI INDEX - ICE FUTURES U.S."))
Nikkei225_scaled <- scaling(subset(all_financials_s2, all_financials_s2$name == "NIKKEI STOCK AVERAGE YEN DENOM - CHICAGO MERCANTILE EXCHANGE"))
all_financials_s4 <- rbind(currency_s4, bonds_s4, US_stocks_s4, Eurodollar_LIBOR_scaled, BCOM_scaled, SP500_VIX_scaled, MSCI_EAFE_scaled, MSCI_EMEA_scaled, Nikkei225_scaled)

# strategy 5

currency_s5.1 <- OI_columns_adjusted(currency_s3)
bonds_s5.1 <- OI_columns_adjusted(bonds_s3)
US_stocks_s5.1 <- OI_columns_adjusted(US_stocks_s3)
all_financials_s5.1 <- OI_columns_adjusted(all_financials_s3)

# Strategy 6

currency_s6.1 <- OI_columns_adjusted(currency_s4)
bonds_s6.1 <- OI_columns_adjusted(bonds_s4)
US_stocks_s6.1 <- OI_columns_adjusted(US_stocks_s4)
all_financials_s6.1 <- OI_columns_adjusted(all_financials_s4)

# Final checks

colSums(is.na(all_financials_s6.1)) # I checked all of them

# There is no deviation in some of columns in the case of EUR_GBP and ZAR_USD. I am going to set those to 0

currency_s1.1[is.na(currency_s1.1)] <- 0
currency_s2[is.na(currency_s2)] <- 0
currency_s3[is.na(currency_s3)] <- 0
currency_s4[is.na(currency_s4)] <- 0
currency_s5.1[is.na(currency_s5.1)] <- 0
currency_s6.1[is.na(currency_s6.1)] <- 0

bonds_s1.1[is.na(bonds_s1.1)] <- 0
bonds_s2[is.na(bonds_s2)] <- 0
bonds_s3[is.na(bonds_s3)] <- 0
bonds_s4[is.na(bonds_s4)] <- 0
bonds_s5.1[is.na(bonds_s5.1)] <- 0
bonds_s6.1[is.na(bonds_s6.1)] <- 0

US_stocks_s1.1[is.na(US_stocks_s1.1)] <- 0
US_stocks_s2[is.na(US_stocks_s2)] <- 0
US_stocks_s3[is.na(US_stocks_s3)] <- 0
US_stocks_s4[is.na(US_stocks_s4)] <- 0
US_stocks_s5.1[is.na(US_stocks_s5.1)] <- 0
US_stocks_s6.1[is.na(US_stocks_s6.1)] <- 0

all_financials_s1.1[is.na(all_financials_s1.1)] <- 0
all_financials_s2[is.na(all_financials_s2)] <- 0
all_financials_s3[is.na(all_financials_s3)] <- 0
all_financials_s4[is.na(all_financials_s4)] <- 0
all_financials_s5.1[is.na(all_financials_s5.1)] <- 0
all_financials_s6.1[is.na(all_financials_s6.1)] <- 0

# cleaning completed

