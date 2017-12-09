# Analyzing COT forms obtained from CFTC website to predict future commodity prices

#data preparation, descriptive analytics and preparation----

library(readxl)

d2017_commodities <- as.data.frame(read_excel("Commodities_COT_2017.xls"))

d2016_commodities <- as.data.frame(read_excel("Commodities_COT_2016.xls"))

d2006_commodities <- as.data.frame(read_excel("Commodities_COT_2006-2015.xls"))

data_commodities <- rbind(d2017_commodities, d2016_commodities)

cot_commodities <- rbind(data_commodities, d2006_commodities)

# Its important that the most recent data sets are read first. Else, date order will make your life harder later

# heating oil is entered sometimes as #2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE and sometimes as #2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE

# I realized this few days into analysis. need to check df dimensions and summary statistics as a part of data validation

popular_names <- c("WHEAT-SRW - CHICAGO BOARD OF TRADE", "CORN - CHICAGO BOARD OF TRADE", "SOYBEANS - CHICAGO BOARD OF TRADE", "SOYBEAN MEAL - CHICAGO BOARD OF TRADE", "SOYBEAN OIL - CHICAGO BOARD OF TRADE", "OATS - CHICAGO BOARD OF TRADE", "ROUGH RICE - CHICAGO BOARD OF TRADE", "CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE", "#2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE", "#2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE", "GASOLINE BLENDSTOCK (RBOB)  - NEW YORK MERCANTILE EXCHANGE", "NATURAL GAS - NEW YORK MERCANTILE EXCHANGE", "CBT ETHANOL - CHICAGO BOARD OF TRADE", "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE", "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE", "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE", "MILK, Class III - CHICAGO MERCANTILE EXCHANGE", "GOLD - COMMODITY EXCHANGE INC.", "SILVER - COMMODITY EXCHANGE INC.", "COPPER-GRADE #1 - COMMODITY EXCHANGE INC.", "PLATINUM - NEW YORK MERCANTILE EXCHANGE", "PALLADIUM - NEW YORK MERCANTILE EXCHANGE", "COTTON NO. 2 - ICE FUTURES U.S.", "FRZN CONCENTRATED ORANGE JUICE - ICE FUTURES U.S.", "COFFEE C - ICE FUTURES U.S.", "SUGAR NO. 11 - ICE FUTURES U.S.", "COCOA - ICE FUTURES U.S.", "RANDOM LENGTH LUMBER - CHICAGO MERCANTILE EXCHANGE")

unique(cot_popular_final_rem$name)

cot_popular <- subset(cot_commodities, cot_commodities$Market_and_Exchange_Names %in% popular_names)

# time to select the columns we want to work with. I want to learn more about each of these columns and decided if I want them in individually

cot_popular$name <- cot_popular$Market_and_Exchange_Names
cot_popular$date <- as.Date(cot_popular$Report_Date_as_MM_DD_YYYY)
cot_popular$OI <- cot_popular$Open_Interest_All
cot_popular$merchant_long <- cot_popular$Prod_Merc_Positions_Long_ALL
cot_popular$merchant_short <- cot_popular$Prod_Merc_Positions_Short_ALL
cot_popular$swap_long <- cot_popular$Swap_Positions_Long_All
cot_popular$swap_short <- cot_popular$Swap__Positions_Short_All
cot_popular$swap_spread <- cot_popular$Swap__Positions_Spread_All
cot_popular$manager_long <- cot_popular$M_Money_Positions_Long_ALL
cot_popular$manager_short <- cot_popular$M_Money_Positions_Short_ALL
cot_popular$manager_spread <- cot_popular$M_Money_Positions_Spread_ALL
cot_popular$other_long <- cot_popular$Other_Rept_Positions_Long_ALL
cot_popular$other_short <- cot_popular$Other_Rept_Positions_Short_ALL
cot_popular$other_spread <- cot_popular$Other_Rept_Positions_Spread_ALL
cot_popular$nonreport_long <- cot_popular$NonRept_Positions_Long_All
cot_popular$nonreport_short <- cot_popular$NonRept_Positions_Short_All
cot_popular$merchant_long_OI <- cot_popular$Pct_of_OI_Prod_Merc_Long_All
cot_popular$merchant_short_OI <- cot_popular$Pct_of_OI_Prod_Merc_Short_All
cot_popular$swap_long_OI <- cot_popular$Pct_of_OI_Swap_Long_All
cot_popular$swap_short_OI <- cot_popular$Pct_of_OI_Swap_Short_All
cot_popular$swap_spread_OI <- cot_popular$Pct_of_OI_Swap_Spread_All
cot_popular$manager_long_OI <- cot_popular$Pct_of_OI_M_Money_Long_All
cot_popular$manager_short_OI <- cot_popular$Pct_of_OI_M_Money_Short_All
cot_popular$manager_spread_OI <- cot_popular$Pct_of_OI_M_Money_Spread_All
cot_popular$otherreportable_long_OI <- cot_popular$Pct_of_OI_Other_Rept_Long_All
cot_popular$otherreportable_short_OI <- cot_popular$Pct_of_OI_Other_Rept_Short_All
cot_popular$otherreportable_spread_OI <- cot_popular$Pct_of_OI_Other_Rept_Spread_All
cot_popular$bigboys_long_OI <- cot_popular$Pct_of_OI_Tot_Rept_Long_All
cot_popular$bigboys_short_OI <- cot_popular$Pct_of_OI_Tot_Rept_Short_All
cot_popular$smallboys_long_OI <- cot_popular$Pct_of_OI_NonRept_Long_All
cot_popular$smallboys_short_OI <- cot_popular$Pct_of_OI_NonRept_Short_All
cot_popular$merchant_long_traders <- cot_popular$Traders_Prod_Merc_Long_All
cot_popular$merchant_short_traders <- cot_popular$Traders_Prod_Merc_Short_All
cot_popular$swap_long_traders <- cot_popular$Traders_Swap_Long_All
cot_popular$swap_short_traders <- cot_popular$Traders_Swap_Short_All
cot_popular$swap_spread_traders <- cot_popular$Traders_Swap_Spread_All
cot_popular$manager_long_traders <- cot_popular$Traders_M_Money_Long_All
cot_popular$manager_short_traders <- cot_popular$Traders_M_Money_Short_All
cot_popular$manager_spread_traders <- cot_popular$Traders_M_Money_Spread_All
cot_popular$other_long_traders <- cot_popular$Traders_Other_Rept_Long_All
cot_popular$other_short_traders <- cot_popular$Traders_Other_Rept_Short_All
cot_popular$other_spread_traders <- cot_popular$Traders_Other_Rept_Spread_All
cot_popular$big_long_traders <- cot_popular$Traders_Tot_Rept_Long_All
cot_popular$big_short_traders <- cot_popular$Traders_Tot_Rept_Short_All
cot_popular$big4_long <- cot_popular$Conc_Net_LE_4_TDR_Long_All
cot_popular$big8_long <- cot_popular$Conc_Net_LE_8_TDR_Long_All
cot_popular$big4_short <- cot_popular$Conc_Net_LE_4_TDR_Short_All
cot_popular$big8_short <- cot_popular$Conc_Net_LE_8_TDR_Short_All
cot_popular_final <- cot_popular[188:236]

colSums(is.na(cot_popular_final))

# swap_spread_traders: 2423
# other_long_trader: 191
# merchant_long_traders: 569
# swap_long_traders: 1235
# swap_short_traders: 4034
# manager_long_traders: 569
# manager_short_traders: 1198
# manager_spread_traders: 2430
# other_short_traders: 708
# other_spread_traders: 1041

# The merchant trader data is never missing!

no.ofNAinrows <- rowSums(is.na(cot_popular_final))

hist(no.ofNAinrows)

# Many of them are missing just 1 or 2. Damn

plot(cot_popular_final$date, no.ofNAinrows)

# Its not like only old data is corrupted

not_traded <- subset(cot_popular_final, cot_popular_final$manager_long_OI == 0)

# 79 times when the manager OI itself is 0

# But that still doesn't explain how there are 576 NA in manager_long_traders. We can't just assume all NA's are naturally zeros

popular_NA <- subset(cot_popular_final, is.na(cot_popular_final$manager_long_traders))

as.data.frame(table(popular_NA$name))

# The problem is not systematic with a contract or exchange. Its just really random

popular_NA2 <- subset(cot_popular_final, is.na(cot_popular_final$manager_short_traders))

as.data.frame(table(popular_NA2$name))

# the NA's are really random

# verified with original excel files. There's no problem with R data frame

# Leaving all no. of traders out. May build a model after removing or impute based on average of nearest number?

# but interestingly, the aggregate traders doesn't have any missing values

cot_popular_final_rem <- cot_popular_final

cot_popular_final_rem[c("swap_long_traders", "other_long_traders", "merchant_long_traders", "merchant_short_traders", "swap_spread_traders", "swap_short_traders", "manager_long_traders", "manager_short_traders", "manager_spread_traders", "other_short_traders", "other_spread_traders")] <- list(NULL)

colSums(is.na(cot_popular_final_rem))

# no missing values

# But let me modify the data set some more based on what I observed in the modelling stage

# I am just going to big 8 numbers going forward. If big 4 is high, then obviously big 8 is going to be high. I am sure there are atleast 8 smart traders in the world.

# It's interesting to note that contract positions and contract position OI's are not essentially related. I actually expected a string of 0.9's in corr matrix

# I am not including any of the bigboys, smallboys columns. They are derived from (correlated) with other OI columns

cot_popular_final_rem[c("big4_long", "big4_short", "bigboys_long_OI", "bigboys_short_OI")] <- list(NULL)

#write.csv(cot_popular_final_rem, file = "full_clean_noNA.csv", row.names = FALSE)

# generated a clean data set. verified

min(cot_popular_final_rem$date)

max(cot_popular_final_rem$date)

#The data set ranges from 2006-06-13 (June 13th, 2006) to 2017-10-17 (Oct 17, 2017)





#Joining Price data for all the commodities----

# Now I have to figure out joins and generate separate commodity data sets before exploring the possibility of one model for all commodities

# Also, each of the commodities started trading at a different time. Not all them were trading at 2006

# loading in the individual prices data sets

wheatSRW_weekly <- read.csv("US Wheat SRW Historical Data.csv")
corn_weekly <- read.csv("US Corn Futures Historical Data.csv")
class3_milk_weekly <- read.csv("Class III Milk Futures Historical Data.csv")
copper_weekly <- read.csv("Copper Futures Historical Data.csv")
crude_weekly <- read.csv("Crude Oil WTI Futures Historical Data.csv")
ethanol_weekly <- read.csv("Ethanol Futures Historical Data.csv")
feeder_cattle_weekly <- read.csv("Feeder Cattle Futures Historical Data.csv")
gasoline_weekly <- read.csv("Gasoline RBOB Futures Historical Data.csv")
gold_weekly <- read.csv("Gold Futures Historical Data.csv")
heatingoil_weekly <- read.csv("Heating Oil Futures Historical Data.csv")
leanhogs_weekly <- read.csv("Lean Hogs Futures Historical Data.csv")
livecattle_weekly <- read.csv("Live Cattle Futures Historical Data.csv")
lumber_weekly <- read.csv("Lumber Futures Historical Data.csv")
naturalgas_weekly <- read.csv("Natural Gas Futures Historical Data.csv")
oats_weekly <- read.csv("Oats Futures Historical Data.csv")
orangejuice_weekly <- read.csv("Orange Juice Futures Historical Data.csv")
palladium_weekly <- read.csv("Palladium Futures Historical Data.csv")
platinum_weekly <- read.csv("Platinum Futures Historical Data.csv")
roughrice_weekly <- read.csv("Rough Rice Futures Historical Data.csv") 
silver_weekly <- read.csv("Silver Futures Historical Data.csv")
cocoa_weekly <- read.csv("US Cocoa Futures Historical Data.csv")
coffee_weekly <- read.csv("US Coffee C Futures Historical Data.csv")
cotton_weekly <- read.csv("US Cotton #2 Futures Historical Data.csv")
soybeanmeal_weekly <- read.csv("US Soybean Meal Futures Historical Data.csv")
soybeanoil_weekly <- read.csv("US Soybean Oil Futures Historical Data.csv")
soybeans_weekly <- read.csv("US Soybeans Futures Historical Data.csv")
sugar_weekly <- read.csv("US Sugar #11 Futures Historical Data.csv")

# Now I generate COT data frames for each commodity from the original cleaned COT data frame





# But before we do that, we need to ensure that there is no missing data between weeks (for detailed comments, look at comments in financial futures file)
 

modified_COT_focus = data.frame(matrix(NA, nrow = 1, ncol = ncol(cot_popular_final_rem)))

colnames(modified_COT_focus) <- colnames(cot_popular_final_rem)

all_product_names <- unique(cot_popular_final_rem$name)

for (i in all_product_names) {
  
  temp_df = subset(cot_popular_final_rem, cot_popular_final_rem$name == i)
  
  if ((max(temp_df$date) - min(temp_df$date) + 7)/nrow(temp_df) != 7) {
    
    for (j in 1:(nrow(temp_df)-1)) {
      temp_df$diff_btw_dates[j] = temp_df$date[j] - temp_df$date[j+1]
    }
    
    temp_x = data.frame(matrix(NA, nrow = 1, ncol = ncol(cot_popular_final_rem)))
    
    
    for (k in 1:(nrow(temp_df)-1)) {
      
      if (temp_df$diff_btw_dates[k] > 13) {break}
      
      temp_df2 <- temp_df[,c(1:34)]
      
      colnames(temp_x) <- colnames(temp_df2)
      
      temp_x <- rbind(temp_x, temp_df2[k, ])
      
      final_x <- temp_x[2:nrow(temp_x), ]
    }
    
    modified_COT_focus <- rbind(modified_COT_focus, final_x)
    
  }  
  
}  



modified_COT_focus <- modified_COT_focus[2:nrow(modified_COT_focus), ]

modified_product_names <- unique(modified_COT_focus$name) 

modified_product_names # Only CBT Ethanol data set has a problem



final_modified_COT_focus = data.frame(matrix(NA, nrow = 1, ncol = ncol(cot_popular_final_rem)))

colnames(final_modified_COT_focus) <- colnames(cot_popular_final_rem)

for (i in modified_product_names) {
  
  tempor_df <- subset(modified_COT_focus, modified_COT_focus$name == i)
  actual_cot_df <- subset(cot_popular_final_rem, cot_popular_final_rem$name == i)
  tempor_df$date <- as.character(actual_cot_df$date[1:nrow(tempor_df)])
  final_modified_COT_focus <- rbind(final_modified_COT_focus, tempor_df)
}

final_modified_COT_focus <- final_modified_COT_focus[2:nrow(final_modified_COT_focus), ]





# But there are obviously products which were never modified in the first place (they had no missing dates)

modified_products <- final_modified_COT_focus$name

never_modified_COT_focus <- subset(cot_popular_final_rem, !(cot_popular_final_rem$name %in% modified_products))

unique(never_modified_COT_focus$name) # verified. There are no overlaps

never_modified_COT_focus$date = as.character(never_modified_COT_focus$date)


# let's define a final cot_focus once and for all

cot_popular_final_rem <- rbind(final_modified_COT_focus, never_modified_COT_focus)

nrow(cot_popular_final_rem) # Mostly, the ethanol rows got removed






wheatSRW_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "WHEAT-SRW - CHICAGO BOARD OF TRADE")
Corn_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "CORN - CHICAGO BOARD OF TRADE")
Soybeans_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "SOYBEANS - CHICAGO BOARD OF TRADE")
soybeanmeal_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "SOYBEAN MEAL - CHICAGO BOARD OF TRADE")
soybeanoil_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "SOYBEAN OIL - CHICAGO BOARD OF TRADE")
oats_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "OATS - CHICAGO BOARD OF TRADE")
roughrice_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "ROUGH RICE - CHICAGO BOARD OF TRADE")
crudeoil_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE")
heatingoil_COT_data <- subset(cot_popular_final_rem, (cot_popular_final_rem$name == "#2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE" | cot_popular_final_rem$name == "#2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE"))
gasoline_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "GASOLINE BLENDSTOCK (RBOB)  - NEW YORK MERCANTILE EXCHANGE")
naturalgas_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "NATURAL GAS - NEW YORK MERCANTILE EXCHANGE")
ethanol_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "CBT ETHANOL - CHICAGO BOARD OF TRADE")
livecattle_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE")
leanhogs_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE")
feedercattle_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE")
class3milk_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "MILK, Class III - CHICAGO MERCANTILE EXCHANGE")
gold_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "GOLD - COMMODITY EXCHANGE INC.")
silver_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "SILVER - COMMODITY EXCHANGE INC.")
copper_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "COPPER-GRADE #1 - COMMODITY EXCHANGE INC.")
platinum_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "PLATINUM - NEW YORK MERCANTILE EXCHANGE")
palladium_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "PALLADIUM - NEW YORK MERCANTILE EXCHANGE")
cotton2_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "COTTON NO. 2 - ICE FUTURES U.S.")
orangejuice_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "FRZN CONCENTRATED ORANGE JUICE - ICE FUTURES U.S.")
coffee_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "COFFEE C - ICE FUTURES U.S.")
sugar_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "SUGAR NO. 11 - ICE FUTURES U.S.")
cocoa_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "COCOA - ICE FUTURES U.S.")
lumber_COT_data <- subset(cot_popular_final_rem, cot_popular_final_rem$name == "RANDOM LENGTH LUMBER - CHICAGO MERCANTILE EXCHANGE")


#Building Final Data sets for each commodity----

# A function to add the prices to the COT forms data for all the commodities

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

  if (x[1,3] == "2017-10-17") {
  x <- x[2:nrow(x), ]
}


# Sometimes investing.com's price data set is sometimes shorter than COT data set. Need to take care of that. Only for few commodities anyway

  if (nrow(y) < nrow(x)) {
    x <- x[1:nrow(y), ]
  }
  
  if (nrow(y) > nrow(x)) {
    y <- y[1:nrow(x), ]
  }

# If I make everything numeric, then I will lose the date and name column

# So, I am just going to save a copy of name and date

  z = x
  
  x <- data.frame(apply(x, 2, function(l) as.numeric(as.character(l))))
  y <- data.frame(apply(y, 2, function(l) as.numeric(as.character(l))))
  

  # The above code will also generate some errors as we run the code because we have date, name columns
  
  final <- cbind(x, y$Price, y$Change..)
  
  final <- cbind(z$name, z$date, final[, c(4:36)])

  colnames(final)[34] <- "act_price"
  
  colnames(final)[35] <- "act_change"
  
  colnames(final)[1] <- "name"
  
  colnames(final)[2] <- "date"

  colnames(final)[28] <- "low_volume_traders_long"
  
  colnames(final)[29] <- "low_volume_traders_short"
  
  final$ind_swap_spread <- ifelse(final$swap_spread_OI > 1, 1, 0)
  final$ind_manager_spread <- ifelse(final$manager_spread_OI > 1, 1, 0)
  final$ind_other_report_spread <- ifelse(final$otherreportable_spread_OI > 1, 1, 0)
  
  final$weekly_change <- final$act_change
  
  final <- final[, -35]
  
  return(final)
}


# generating final data sets for each commodity


wheatSRW_final <- price_add(wheatSRW_COT_data, wheatSRW_weekly)
corn_final <- price_add(Corn_COT_data, corn_weekly)
soybeans_final <- price_add(Soybeans_COT_data, soybeans_weekly)
soybeanmeal_final <- price_add(soybeanmeal_COT_data, soybeanmeal_weekly)
soybeanoil_final <- price_add(soybeanoil_COT_data, soybeanoil_weekly)
oats_final <- price_add(oats_COT_data, oats_weekly)
roughrice_final <- price_add(roughrice_COT_data, roughrice_weekly)
crudeoil_final <- price_add(crudeoil_COT_data, crude_weekly)
heatingoil_final <- price_add(heatingoil_COT_data, heatingoil_weekly)
gasoline_final <- price_add(gasoline_COT_data, gasoline_weekly)
naturalgas_final <- price_add(naturalgas_COT_data, naturalgas_weekly)
ethanol_final <- price_add(ethanol_COT_data, ethanol_weekly)
livecattle_final <- price_add(livecattle_COT_data, livecattle_weekly)
leanhogs_final <- price_add(leanhogs_COT_data, leanhogs_weekly)
feedercattle_final <- price_add(feedercattle_COT_data, feeder_cattle_weekly)
class3milk_final <- price_add(class3milk_COT_data, class3_milk_weekly)
gold_final <- price_add(gold_COT_data, gold_weekly)
silver_final <- price_add(silver_COT_data, silver_weekly)
copper_final <- price_add(copper_COT_data, copper_weekly)
platinum_final <- price_add(platinum_COT_data, platinum_weekly)
palladium_final <- price_add(palladium_COT_data, palladium_weekly)
cotton2_final <- price_add(cotton2_COT_data, cotton_weekly)
orangejuice_final <- price_add(orangejuice_COT_data, orangejuice_weekly)
coffee_final <- price_add(coffee_COT_data, coffee_weekly)
sugar_final <- price_add(sugar_COT_data, sugar_weekly)
cocoa_final <- price_add(cocoa_COT_data, cocoa_weekly)
lumber_final <- price_add(lumber_COT_data, lumber_weekly)

# write.csv(corn_final, file = "corn.csv", row.names = FALSE)

# wrote out some files. verified


#Time Series discussion------

# We have clean data sets. But how should we analyze them because there are certain time dependant variables in the data set?

# Strategy 1 - Look at only the time independent predictors and %change as response variable

# Strategy 2 - adjust the time series dependant variables and include them

# Strategy 3 - Use only last 3 years data. interest rates are so low. time series effects don't matter much. But does only interest rate matter? Or does Hedge funds AUM growth matter more?

# Strategy 4 - Will any Feature extraction help?

# Strategy 5 - Does it make any sense to look at only time dependent variables with price as response variable

# Strategy 6 - Just throw in everything.

# Based on literature review, scater plots, it has been decided it is best to focus only on the best percentage OI variables. One could argue that over a period of time commodity (real assets) tend to go up and hence OI and volume will remain more or less constant. But that relationship may not hold in this QE era. Yes real assets will continue to go up at the rate of inflation but I think Hedge fund and trader AUM + leverage (which actually impacts OI) which are correlated with broad asset returns will continue to outpace inflation for the next few years.

# I have also spoken to a fin engg professor and he mentioned that I would miss out on the seasonality of the hedging (and OI) activity based on harvest cycle especially for the agriculture commodities and softs

# Hence, I will build another data set that has only time independent variables with %change as response variable

# However, I am going to include total OI column because I have plans to build a value weighted indiex later

# I am going to write out name and date as well. Else, the data sets are too confusing

time_independent_columns <- c(1:3,17:29,32,33,35,36, 37, 38)

wheatSRW_notime <- wheatSRW_final[, time_independent_columns]
corn_notime <- corn_final[, time_independent_columns]
oats_notime <- oats_final[, time_independent_columns]
soybeanoil_notime <- soybeanoil_final[, time_independent_columns]
soybeans_notime <- soybeans_final[, time_independent_columns]
heatingoil_notime <- heatingoil_final[, time_independent_columns]
naturalgas_notime <- naturalgas_final[, time_independent_columns]
ethanol_notime <- ethanol_final[, time_independent_columns]
soybeanmeal_notime <- soybeanmeal_final[, time_independent_columns]
cotton2_notime <- cotton2_final[, time_independent_columns]
roughrice_notime <- roughrice_final[, time_independent_columns]
orangejuice_notime <- orangejuice_final[, time_independent_columns]
copper_notime <- copper_final[, time_independent_columns]
platinum_notime <- platinum_final[, time_independent_columns]
palladium_notime <- palladium_final[, time_independent_columns]
silver_notime <- silver_final[, time_independent_columns]
gold_notime <- gold_final[, time_independent_columns]
sugar_notime <- sugar_final[, time_independent_columns]
cocoa_notime <- cocoa_final[, time_independent_columns]
gasoline_notime <- gasoline_final[, time_independent_columns]
leanhogs_notime <- leanhogs_final[, time_independent_columns]
livecattle_notime <- livecattle_final[, time_independent_columns]
coffee_notime <- coffee_final[, time_independent_columns]
lumber_notime <- lumber_final[, time_independent_columns]
feedercattle_notime <- feedercattle_final[, time_independent_columns]
class3milk_notime <- class3milk_final[, time_independent_columns]
crudeoil_notime <- crudeoil_final[, time_independent_columns]


# Building the aggregate data sets now for different strategies

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

# all values approximate

WheatSRW_value <- value(wheatSRW_notime, 0.022, 1)
livecattle_value <- value(livecattle_notime, 0.045, 1)
class3milk_value <- value(class3milk_notime, 0.029, 1)
cocoa_value <- value(cocoa_notime, 0.02, 1)
coffee_value <- value(coffee_notime, 0.048, 1)
copper_value <- value(copper_notime, 0.0075, 1)
cotton2_value <- value(cotton2_notime, 0.035, 1)
corn_value <- value(corn_notime, 0.0175, 1)
crudeoil_value <- value(crudeoil_notime, 0.06, 1)
ethanol_value <- value(ethanol_notime, 0.037, 1)
feedercattle_value <- value(feedercattle_notime, 0.075, 1)
gasoline_value <- value(gasoline_notime, 0.07, 1)
gold_value <- value(gold_notime, 0.125, 1)
heatingoil_value <- value(heatingoil_notime, 0.78, 1)
leanhogs_value <- value(leanhogs_notime, 0.027, 1)
lumber_value <- value(lumber_notime, 0.0466, 1)
naturalgas_value <- value(naturalgas_notime, 0.028, 1)
oats_value <- value(oats_notime, 0.0123, 1)
orangejuice_value <- value(orangejuice_notime, 0.02352, 1)
palladium_value <- value(palladium_notime, 0.098, 1)
platinum_value <- value(platinum_notime, 0.045, 1)
roughrice_value <- value(roughrice_notime, 0.024, 1)
silver_value <- value(silver_notime, 0.08, 1)
soybeanmeal_value <- value(soybeanmeal_notime, 0.034, 1)
soybeanoil_value <- value(soybeanoil_notime, 0.02, 1)
soybeans_value <- value(soybeans_notime, 0.05, 1)
sugar_value <- value(sugar_notime, 0.016, 1) 


grains_s1 <- rbind(WheatSRW_value, corn_value, soybeans_value, soybeanmeal_value, soybeanoil_value, oats_value, roughrice_value)
energies_s1 <- rbind(crudeoil_value, heatingoil_value, gasoline_value, naturalgas_value, ethanol_value)
meats_s1 <- rbind(livecattle_value, feedercattle_value, class3milk_value, leanhogs_value)
metals_s1 <- rbind(gold_value, silver_value, platinum_value, palladium_value, copper_value)
softs_s1 <- rbind(cotton2_value, orangejuice_value, coffee_value, sugar_value, cocoa_value, lumber_value)
all_commodities_s1 <- rbind(grains_s1, energies_s1, meats_s1, metals_s1, softs_s1)



relative_percnt_change_added <- function(data) {
  products_total_value <- aggregate(data$total_product_value, list(data$date), sum)
  colnames(products_total_value) <- c("date", "agg_value")
  data$date <- as.Date(data$date)
  products_total_value$date <- as.Date(products_total_value$date)
  products_total_value <- products_total_value[order(products_total_value$date, decreasing = TRUE),]
  products_total_value$date = as.Date(products_total_value$date)
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


  merged_df <- merge(x = data, y = products_total_value, by = "date", all.x = TRUE)
  merged_df$monthly_agg_value <- NULL
  merged_df$quarterly_agg_value <- NULL
  merged_df$yearly_agg_value <- NULL
  
  merged_df$rel_change_4 <- NA
  merged_df$rel_change_13 <- NA
  merged_df$rel_change_52 <- NA
  
  
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

grains_s1 <- relative_percnt_change_added(grains_s1)
meats_s1 <- relative_percnt_change_added(meats_s1)
metals_s1 <- relative_percnt_change_added(metals_s1)
softs_s1 <- relative_percnt_change_added(softs_s1)
energies_s1 <- relative_percnt_change_added(energies_s1)
all_commodities_s1 <- relative_percnt_change_added(all_commodities_s1)



OI_columns_adjusted <- function(data) {
  for (j in 1:nrow(data)) {
    for (i in 4:18) {
      data[j,i] = data[j, i]*data$rel_change_4[j]
    }
  }
  
  data$rel_change_4 <- NULL
  data$rel_change_13 <- NULL
  data$rel_change_52 <- NULL
  
  return(data)
}

grains_s1.1 <- OI_columns_adjusted(grains_s1)
meats_s1.1 <- OI_columns_adjusted(meats_s1)
metals_s1.1 <- OI_columns_adjusted(metals_s1)
softs_s1.1 <- OI_columns_adjusted(softs_s1)
energies_s1.1 <- OI_columns_adjusted(energies_s1)
all_commodities_s1.1 <- OI_columns_adjusted(all_commodities_s1)



# strategy 2

grains_s2 <- grains_s1
meats_s2 <- meats_s1
metals_s2 <- metals_s1
energies_s2 <- energies_s1
softs_s2 <- softs_s1
all_commodities_s2 <- all_commodities_s1


write.csv(subset(meats_s2, meats_s2$name == "MILK, Class III - CHICAGO MERCANTILE EXCHANGE"), "Class3milk.csv", row.names = FALSE)
write.csv(subset(meats_s2, meats_s2$name == "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"), "Livecattle.csv", row.names = FALSE)
write.csv(subset(meats_s2, meats_s2$name == "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"), "Feedercattle.csv", row.names = FALSE)
write.csv(subset(meats_s2, meats_s2$name == "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"), "Leanhogs.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "WHEAT-SRW - CHICAGO BOARD OF TRADE"), "WheatSRW.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "SOYBEAN MEAL - CHICAGO BOARD OF TRADE"), "Soybeanmeal.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "OATS - CHICAGO BOARD OF TRADE"), "Oats.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "CORN - CHICAGO BOARD OF TRADE"), "Corn.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "ROUGH RICE - CHICAGO BOARD OF TRADE"), "Roughrice.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "SOYBEANS - CHICAGO BOARD OF TRADE"), "Soybeans.csv", row.names = FALSE)
write.csv(subset(grains_s2, grains_s2$name == "SOYBEAN OIL - CHICAGO BOARD OF TRADE"), "Soybeanoil.csv", row.names = FALSE)
write.csv(subset(metals_s2, metals_s2$name == "GOLD - COMMODITY EXCHANGE INC."), "Gold.csv", row.names = FALSE)
write.csv(subset(metals_s2, metals_s2$name == "COPPER-GRADE #1 - COMMODITY EXCHANGE INC."), "Copper.csv", row.names = FALSE)
write.csv(subset(metals_s2, metals_s2$name == "SILVER - COMMODITY EXCHANGE INC."), "Silver.csv", row.names = FALSE)
write.csv(subset(metals_s2, metals_s2$name == "PLATINUM - NEW YORK MERCANTILE EXCHANGE"), "Platinum.csv", row.names = FALSE)
write.csv(subset(metals_s2, metals_s2$name == "PALLADIUM - NEW YORK MERCANTILE EXCHANGE"), "Palladium.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "FRZN CONCENTRATED ORANGE JUICE - ICE FUTURES U.S."), "Orangejuice.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "RANDOM LENGTH LUMBER - CHICAGO MERCANTILE EXCHANGE"), "Lumber.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "COCOA - ICE FUTURES U.S."), "Cocoa.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "COTTON NO. 2 - ICE FUTURES U.S."), "Cotton2.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "SUGAR NO. 11 - ICE FUTURES U.S."), "Sugar.csv", row.names = FALSE)
write.csv(subset(softs_s2, softs_s2$name == "COFFEE C - ICE FUTURES U.S."), "Coffee.csv", row.names = FALSE)
unique(energies_s2$name)

write.csv(subset(energies_s2, energies_s2$name == "CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"), "Crude.csv", row.names = FALSE)
write.csv(subset(energies_s2, energies_s2$name == "NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"), "Naturalgas.csv", row.names = FALSE)
write.csv(subset(energies_s2, energies_s2$name == "GASOLINE BLENDSTOCK (RBOB)  - NEW YORK MERCANTILE EXCHANGE"), "Gasoline.csv", row.names = FALSE)
write.csv(subset(energies_s2, energies_s2$name == "CBT ETHANOL - CHICAGO BOARD OF TRADE"), "CBTethanol.csv", row.names = FALSE)
write.csv(subset(energies_s2, energies_s2$name == "#2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE" | energies_s2$name == "#2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE"), "Heatingoil.csv", row.names = FALSE)


# strategy 3: normalization but not adjusting to OI

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




class3milk_normalized <- normalization(subset(meats_s2, meats_s2$name == "MILK, Class III - CHICAGO MERCANTILE EXCHANGE"))
livecattle_normalized <- normalization(subset(meats_s2, meats_s2$name == "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"))
feedercattle_normalized <- normalization(subset(meats_s2, meats_s2$name == "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"))
leanhogs_normalized <- normalization(subset(meats_s2, meats_s2$name == "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"))
wheatSRW_normalized <- normalization(subset(grains_s2, grains_s2$name == "WHEAT-SRW - CHICAGO BOARD OF TRADE"))
soybeanmeal_normalized <- normalization(subset(grains_s2, grains_s2$name == "SOYBEAN MEAL - CHICAGO BOARD OF TRADE"))
oats_normalized <- normalization(subset(grains_s2, grains_s2$name == "OATS - CHICAGO BOARD OF TRADE"))
corn_normalized <- normalization(subset(grains_s2, grains_s2$name == "CORN - CHICAGO BOARD OF TRADE"))
roughrice_normalized <- normalization(subset(grains_s2, grains_s2$name == "ROUGH RICE - CHICAGO BOARD OF TRADE"))
soybeans_normalized <- normalization(subset(grains_s2, grains_s2$name == "SOYBEANS - CHICAGO BOARD OF TRADE"))
soybeanoil_normalized <- normalization(subset(grains_s2, grains_s2$name == "SOYBEAN OIL - CHICAGO BOARD OF TRADE"))
gold_normalized <- normalization(subset(metals_s2, metals_s2$name == "GOLD - COMMODITY EXCHANGE INC."))
copper_normalized <- normalization(subset(metals_s2, metals_s2$name == "COPPER-GRADE #1 - COMMODITY EXCHANGE INC."))
silver_normalized <- normalization(subset(metals_s2, metals_s2$name == "SILVER - COMMODITY EXCHANGE INC."))
platinum_normalized <- normalization(subset(metals_s2, metals_s2$name == "PLATINUM - NEW YORK MERCANTILE EXCHANGE"))
palladium_normalized <- normalization(subset(metals_s2, metals_s2$name == "PALLADIUM - NEW YORK MERCANTILE EXCHANGE"))
orangejuice_normalized <- normalization(subset(softs_s2, softs_s2$name == "FRZN CONCENTRATED ORANGE JUICE - ICE FUTURES U.S."))
lumber_normalized <- normalization(subset(softs_s2, softs_s2$name == "RANDOM LENGTH LUMBER - CHICAGO MERCANTILE EXCHANGE"))
cocoa_normalized <- normalization(subset(softs_s2, softs_s2$name == "COCOA - ICE FUTURES U.S."))
cotton2_normalized <- normalization(subset(softs_s2, softs_s2$name == "COTTON NO. 2 - ICE FUTURES U.S."))
sugar_normalized<- normalization(subset(softs_s2, softs_s2$name == "SUGAR NO. 11 - ICE FUTURES U.S."))
coffee_normalized <- normalization(subset(softs_s2, softs_s2$name == "COFFEE C - ICE FUTURES U.S."))
crudeoil_normalized <- normalization(subset(energies_s2, energies_s2$name == "CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"))
naturalgas_normalized <- normalization(subset(energies_s2, energies_s2$name == "NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"))
gasoline_normalized <- normalization(subset(energies_s2, energies_s2$name == "GASOLINE BLENDSTOCK (RBOB)  - NEW YORK MERCANTILE EXCHANGE"))
ethanol_normalized <- normalization(subset(energies_s2, energies_s2$name == "CBT ETHANOL - CHICAGO BOARD OF TRADE"))
heatingoil_normalized <- normalization(subset(energies_s2, energies_s2$name == "#2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE" | energies_s2$name == "#2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE"))


grains_s3 <- rbind(wheatSRW_normalized, corn_normalized, soybeans_normalized, soybeanmeal_normalized,soybeanoil_normalized, oats_normalized, roughrice_normalized)
energies_s3 <- rbind(crudeoil_normalized, heatingoil_normalized, gasoline_normalized, naturalgas_normalized, ethanol_normalized)
meats_s3 <- rbind(livecattle_normalized, feedercattle_normalized, class3milk_normalized, leanhogs_normalized)
metals_s3 <- rbind(gold_normalized, silver_normalized, platinum_normalized, palladium_normalized, copper_normalized)
softs_s3 <- rbind(cotton2_normalized, orangejuice_normalized, coffee_normalized, sugar_normalized, cocoa_normalized, lumber_normalized)
all_commodities_s3 <- rbind(grains_s3, energies_s3, meats_s3, metals_s3, softs_s3)


# Strategy 4

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




class3milk_scaled <- scaling(subset(meats_s2, meats_s2$name == "MILK, Class III - CHICAGO MERCANTILE EXCHANGE"))
livecattle_scaled <- scaling(subset(meats_s2, meats_s2$name == "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"))
feedercattle_scaled <- scaling(subset(meats_s2, meats_s2$name == "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"))
leanhogs_scaled <- scaling(subset(meats_s2, meats_s2$name == "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"))
wheatSRW_scaled <- scaling(subset(grains_s2, grains_s2$name == "WHEAT-SRW - CHICAGO BOARD OF TRADE"))
soybeanmeal_scaled <- scaling(subset(grains_s2, grains_s2$name == "SOYBEAN MEAL - CHICAGO BOARD OF TRADE"))
oats_scaled <- scaling(subset(grains_s2, grains_s2$name == "OATS - CHICAGO BOARD OF TRADE"))
corn_scaled <- scaling(subset(grains_s2, grains_s2$name == "CORN - CHICAGO BOARD OF TRADE"))
roughrice_scaled <- scaling(subset(grains_s2, grains_s2$name == "ROUGH RICE - CHICAGO BOARD OF TRADE"))
soybeans_scaled <- scaling(subset(grains_s2, grains_s2$name == "SOYBEANS - CHICAGO BOARD OF TRADE"))
soybeanoil_scaled <- scaling(subset(grains_s2, grains_s2$name == "SOYBEAN OIL - CHICAGO BOARD OF TRADE"))
gold_scaled <- scaling(subset(metals_s2, metals_s2$name == "GOLD - COMMODITY EXCHANGE INC."))
copper_scaled <- scaling(subset(metals_s2, metals_s2$name == "COPPER-GRADE #1 - COMMODITY EXCHANGE INC."))
silver_scaled <- scaling(subset(metals_s2, metals_s2$name == "SILVER - COMMODITY EXCHANGE INC."))
platinum_scaled <- scaling(subset(metals_s2, metals_s2$name == "PLATINUM - NEW YORK MERCANTILE EXCHANGE"))
palladium_scaled <- scaling(subset(metals_s2, metals_s2$name == "PALLADIUM - NEW YORK MERCANTILE EXCHANGE"))
orangejuice_scaled <- scaling(subset(softs_s2, softs_s2$name == "FRZN CONCENTRATED ORANGE JUICE - ICE FUTURES U.S."))
lumber_scaled <- scaling(subset(softs_s2, softs_s2$name == "RANDOM LENGTH LUMBER - CHICAGO MERCANTILE EXCHANGE"))
cocoa_scaled <- scaling(subset(softs_s2, softs_s2$name == "COCOA - ICE FUTURES U.S."))
cotton2_scaled <- scaling(subset(softs_s2, softs_s2$name == "COTTON NO. 2 - ICE FUTURES U.S."))
sugar_scaled<- scaling(subset(softs_s2, softs_s2$name == "SUGAR NO. 11 - ICE FUTURES U.S."))
coffee_scaled <- scaling(subset(softs_s2, softs_s2$name == "COFFEE C - ICE FUTURES U.S."))
crudeoil_scaled <- scaling(subset(energies_s2, energies_s2$name == "CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"))
naturalgas_scaled <- scaling(subset(energies_s2, energies_s2$name == "NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"))
gasoline_scaled <- scaling(subset(energies_s2, energies_s2$name == "GASOLINE BLENDSTOCK (RBOB)  - NEW YORK MERCANTILE EXCHANGE"))
ethanol_scaled <- scaling(subset(energies_s2, energies_s2$name == "CBT ETHANOL - CHICAGO BOARD OF TRADE"))
heatingoil_scaled <- scaling(subset(energies_s2, energies_s2$name == "#2 HEATING OIL- NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE" | energies_s2$name == "#2 HEATING OIL, NY HARBOR-ULSD - NEW YORK MERCANTILE EXCHANGE"))


grains_s4 <- rbind(wheatSRW_scaled, corn_scaled, soybeans_scaled, soybeanmeal_scaled,soybeanoil_scaled, oats_scaled, roughrice_scaled)
energies_s4 <- rbind(crudeoil_scaled, heatingoil_scaled, gasoline_scaled, naturalgas_scaled, ethanol_scaled)
meats_s4 <- rbind(livecattle_scaled, feedercattle_scaled, class3milk_scaled, leanhogs_scaled)
metals_s4 <- rbind(gold_scaled, silver_scaled, platinum_scaled, palladium_scaled, copper_scaled)
softs_s4 <- rbind(cotton2_scaled, orangejuice_scaled, coffee_scaled, sugar_scaled, cocoa_scaled, lumber_scaled)
all_commodities_s4 <- rbind(grains_s4, energies_s4, meats_s4, metals_s4, softs_s4)


# Strategy 5

grains_s5.1 <- OI_columns_adjusted(grains_s3)
metals_s5.1 <- OI_columns_adjusted(metals_s3)
meats_s5.1 <- OI_columns_adjusted(meats_s3)
energies_s5.1 <- OI_columns_adjusted(energies_s3)
softs_s5.1 <- OI_columns_adjusted(softs_s3)
all_commodities_s5.1 <- OI_columns_adjusted(all_commodities_s3)

# Strategy 6

grains_s6.1 <- OI_columns_adjusted(grains_s4)
metals_s6.1 <- OI_columns_adjusted(metals_s4)
meats_s6.1 <- OI_columns_adjusted(meats_s4)
energies_s6.1 <- OI_columns_adjusted(energies_s4)
softs_s6.1 <- OI_columns_adjusted(softs_s4)
all_commodities_s6.1 <- OI_columns_adjusted(all_commodities_s4)


meats_s1.1[is.na(meats_s1.1)] <- 0
meats_s2[is.na(meats_s2)] <- 0
meats_s3[is.na(meats_s3)] <- 0
meats_s4[is.na(meats_s4)] <- 0
meats_s5.1[is.na(meats_s5.1)] <- 0
meats_s6.1[is.na(meats_s6.1)] <- 0


metals_s1.1[is.na(metals_s1.1)] <- 0
metals_s2[is.na(metals_s2)] <- 0
metals_s3[is.na(metals_s3)] <- 0
metals_s4[is.na(metals_s4)] <- 0
metals_s5.1[is.na(metals_s5.1)] <- 0
metals_s6.1[is.na(metals_s6.1)] <- 0


energies_s1.1[is.na(energies_s1.1)] <- 0
energies_s2[is.na(energies_s2)] <- 0
energies_s3[is.na(energies_s3)] <- 0
energies_s4[is.na(energies_s4)] <- 0
energies_s5.1[is.na(energies_s5.1)] <- 0
energies_s6.1[is.na(energies_s6.1)] <- 0

grains_s1.1[is.na(grains_s1.1)] <- 0
grains_s2[is.na(grains_s2)] <- 0
grains_s3[is.na(grains_s3)] <- 0
grains_s4[is.na(grains_s4)] <- 0
grains_s5.1[is.na(grains_s5.1)] <- 0
grains_s6.1[is.na(grains_s6.1)] <- 0

softs_s1.1[is.na(softs_s1.1)] <- 0
softs_s2[is.na(softs_s2)] <- 0
softs_s3[is.na(softs_s3)] <- 0
softs_s4[is.na(softs_s4)] <- 0
softs_s5.1[is.na(softs_s5.1)] <- 0
softs_s6.1[is.na(softs_s6.1)] <- 0


all_commodities_s1.1[is.na(all_commodities_s1.1)] <- 0
all_commodities_s2[is.na(all_commodities_s2)] <- 0
all_commodities_s3[is.na(all_commodities_s3)] <- 0
all_commodities_s4[is.na(all_commodities_s4)] <- 0
all_commodities_s5.1[is.na(all_commodities_s5.1)] <- 0
all_commodities_s6.1[is.na(all_commodities_s6.1)] <- 0




