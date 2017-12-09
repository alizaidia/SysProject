#Sys 6018 Final Project
#Ali Zaidi

library(readr)
library(lubridate)

##########################################################################################################################################################################################
#Create Empty Mean DF

meandf <- setNames(data.frame(matrix(ncol = 12, nrow = 27)), 
                   c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
row.names(meandf) <- c("Copper", "CrudeOil", "Ethanol", "FeederCattle", "Gasoline", "Gold", "HeatingOil", "LeanHogs", "LiveCattle", "Lumber", 
                       "Milk", "NaturalGas", "Oats", "OrangeJuice", "Palladium", "Platinum", "RoughRice", "Silver", "USCocoa", "USCoffee", "USCorn",
                       "USCotton", "USSoybeanMeal","USSoybeanOil", "USSoybeans", "USSugar", "USWheat")

#Create Empty Median DF
mediandf <- setNames(data.frame(matrix(ncol = 12, nrow = 27)), 
                     c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
row.names(mediandf) <- c("Copper", "CrudeOil", "Ethanol", "FeederCattle", "Gasoline", "Gold", "HeatingOil", "LeanHogs", "LiveCattle", "Lumber", 
                         "Milk", "NaturalGas", "Oats", "OrangeJuice", "Palladium", "Platinum", "RoughRice", "Silver", "USCocoa", "USCoffee", "USCorn",
                         "USCotton", "USSoybeanMeal","USSoybeanOil", "USSoybeans", "USSugar", "USWheat")

##########################################################################################################################################################################################
#Copper#
##########################################################################################################################################################################################
copp <- read.csv("Copper Futures Historical Data.csv")
coppdf <- as.data.frame(copp, StringsAsFactors = FALSE)

#Remove NA values
coppdf <- coppdf[-c(nrow(coppdf)-1, nrow(coppdf)), ]

#Get the ï..Date and change columns
coppdf <- coppdf[, c(1,7)]

#Convert copp ï..Date data into data format
copptime <- coppdf[, 1]
copptime<- as.Date(as.character(copptime), "%b %d, %Y")
coppdf[, 1]  <- copptime

coppdf[,2] = abs(coppdf[,2])

#Get monthly mean data

coppmean <- mean(coppdf$Change..[months(coppdf$ï..Date) == "January"])
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "February"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "March"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "April"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "May"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "June"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "July"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "August"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "September"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "October"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "November"]))
coppmean <- c(coppmean, mean(coppdf$Change..[months(coppdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[1, ] <- coppmean

#Get monthly median data
coppmed <- median(coppdf$Change..[months(coppdf$ï..Date) == "January"])
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "February"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "March"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "April"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "May"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "June"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "July"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "August"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "September"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "October"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "November"]))
coppmed <- c(coppmed, median(coppdf$Change..[months(coppdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[1, ] <- coppmed

##########################################################################################################################################################################################
#Crude Oil#
##########################################################################################################################################################################################
crude <- read.csv("Crude Oil WTI Futures Historical Data.csv")
crudedf <- as.data.frame(crude, StringsAsFactors = FALSE)

#Remove NA values
crudedf <- crudedf[-c(nrow(crudedf)-1, nrow(crudedf)), ]

#Get the ï..Date and change columns
crudedf <- crudedf[, c(1,7)]

#Convert crude ï..Date data into data format
crudetime <- crudedf[, 1]
crudetime<- as.Date(as.character(crudetime), "%b %d, %Y")
crudedf[, 1]  <- crudetime

crudedf[,2] = abs(crudedf[,2])

#Get monthly mean data
crudemean <- mean(crudedf$Change..[months(crudedf$ï..Date) == "January"])
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "February"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "March"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "April"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "May"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "June"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "July"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "August"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "September"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "October"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "November"]))
crudemean <- c(crudemean, mean(crudedf$Change..[months(crudedf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[2, ] <- crudemean

#Get monthly median data
crudemed <- median(crudedf$Change..[months(crudedf$ï..Date) == "January"])
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "February"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "March"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "April"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "May"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "June"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "July"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "August"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "September"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "October"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "November"]))
crudemed <- c(crudemed, median(crudedf$Change..[months(crudedf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[2, ] <- crudemed

##########################################################################################################################################################################################
#Ethanol#
##########################################################################################################################################################################################
eth <- read.csv("Ethanol Futures Historical Data.csv")
ethdf <- as.data.frame(eth, StringsAsFactors = FALSE)

#Remove NA values
ethdf <- ethdf[-c(nrow(ethdf)-1, nrow(ethdf)), ]

#Get the ï..Date and change columns
ethdf <- ethdf[, c(1,7)]

#Convert eth ï..Date data into data format
ethtime <- ethdf[, 1]
ethtime<- as.Date(as.character(ethtime), "%b %d, %Y")
ethdf[, 1]  <- ethtime

ethdf[,2] = abs(ethdf[,2])

#Get monthly mean data
ethmean <- mean(ethdf$Change..[months(ethdf$ï..Date) == "January"])
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "February"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "March"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "April"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "May"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "June"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "July"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "August"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "September"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "October"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "November"]))
ethmean <- c(ethmean, mean(ethdf$Change..[months(ethdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[3, ] <- ethmean

#Get monthly median data
ethmed <- median(ethdf$Change..[months(ethdf$ï..Date) == "January"])
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "February"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "March"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "April"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "May"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "June"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "July"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "August"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "September"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "October"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "November"]))
ethmed <- c(ethmed, median(ethdf$Change..[months(ethdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[3, ] <- ethmed

##########################################################################################################################################################################################
#Feeder Cattle#
##########################################################################################################################################################################################
feed <- read.csv("Feeder Cattle Futures Historical Data.csv")
feeddf <- as.data.frame(feed, StringsAsFactors = FALSE)

#Remove NA values
feeddf <- feeddf[-c(nrow(feeddf)-1, nrow(feeddf)), ]

#Get the ï..Date and change columns
feeddf <- feeddf[, c(1,7)]

#Convert feed ï..Date data into data format
feedtime <- feeddf[, 1]
feedtime<- as.Date(as.character(feedtime), "%b %d, %Y")
feeddf[, 1]  <- feedtime

feeddf[,2] = abs(feeddf[,2])

#Get monthly mean data
feedmean <- mean(feeddf$Change..[months(feeddf$ï..Date) == "January"])
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "February"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "March"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "April"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "May"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "June"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "July"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "August"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "September"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "October"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "November"]))
feedmean <- c(feedmean, mean(feeddf$Change..[months(feeddf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[4, ] <- feedmean

#Get monthly median data
feedmed <- median(feeddf$Change..[months(feeddf$ï..Date) == "January"])
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "February"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "March"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "April"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "May"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "June"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "July"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "August"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "September"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "October"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "November"]))
feedmed <- c(feedmed, median(feeddf$Change..[months(feeddf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[4, ] <- feedmed

##########################################################################################################################################################################################
#Gasoline#
##########################################################################################################################################################################################
gasol <- read.csv("Gasoline RBOB Futures Historical Data.csv")
gasoldf <- as.data.frame(gasol, StringsAsFactors = FALSE)

#Remove NA values
gasoldf <- gasoldf[-c(nrow(gasoldf)-1, nrow(gasoldf)), ]

#Get the ï..Date and change columns
gasoldf <- gasoldf[, c(1,7)]

#Convert gasol ï..Date data into data format
gasoltime <- gasoldf[, 1]
gasoltime<- as.Date(as.character(gasoltime), "%b %d, %Y")
gasoldf[, 1]  <- gasoltime

gasoldf[,2] = abs(gasoldf[,2])

#Get monthly mean data
gasolmean <- mean(gasoldf$Change..[months(gasoldf$ï..Date) == "January"])
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "February"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "March"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "April"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "May"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "June"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "July"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "August"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "September"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "October"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "November"]))
gasolmean <- c(gasolmean, mean(gasoldf$Change..[months(gasoldf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[5, ] <- gasolmean

#Get monthly median data
gasolmed <- median(gasoldf$Change..[months(gasoldf$ï..Date) == "January"])
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "February"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "March"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "April"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "May"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "June"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "July"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "August"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "September"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "October"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "November"]))
gasolmed <- c(gasolmed, median(gasoldf$Change..[months(gasoldf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[5, ] <- gasolmed

##########################################################################################################################################################################################
#Gold#
##########################################################################################################################################################################################
gold <- read.csv("Gold Futures Historical Data.csv")
golddf <- as.data.frame(gold, StringsAsFactors = FALSE)

#Remove NA values
golddf <- golddf[-c(nrow(golddf)-1, nrow(golddf)), ]

#Get the ï..Date and change columns
golddf <- golddf[, c(1,7)]

#Convert gold ï..Date data into data format
goldtime <- golddf[, 1]
goldtime<- as.Date(as.character(goldtime), "%b %d, %Y")
golddf[, 1]  <- goldtime

golddf[,2] = abs(golddf[,2])

#Get monthly mean data
goldmean <- mean(golddf$Change..[months(golddf$ï..Date) == "January"])
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "February"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "March"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "April"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "May"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "June"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "July"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "August"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "September"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "October"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "November"]))
goldmean <- c(goldmean, mean(golddf$Change..[months(golddf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[6, ] <- goldmean

#Get monthly median data
goldmed <- median(golddf$Change..[months(golddf$ï..Date) == "January"])
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "February"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "March"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "April"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "May"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "June"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "July"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "August"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "September"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "October"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "November"]))
goldmed <- c(goldmed, median(golddf$Change..[months(golddf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[6, ] <- goldmed

##########################################################################################################################################################################################
#Heating Oil#
##########################################################################################################################################################################################
oil <- read.csv("Heating Oil Futures Historical Data.csv")
oildf <- as.data.frame(oil, StringsAsFactors = FALSE)

#Remove NA values
oildf <- oildf[-c(nrow(oildf)-1, nrow(oildf)), ]

#Get the ï..Date and change columns
oildf <- oildf[, c(1,7)]

#Convert oil ï..Date data into data format
oiltime <- oildf[, 1]
oiltime<- as.Date(as.character(oiltime), "%b %d, %Y")
oildf[, 1]  <- oiltime

oildf[,2] = abs(oildf[,2])

#Get monthly mean data
oilmean <- mean(oildf$Change..[months(oildf$ï..Date) == "January"])
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "February"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "March"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "April"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "May"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "June"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "July"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "August"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "September"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "October"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "November"]))
oilmean <- c(oilmean, mean(oildf$Change..[months(oildf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[7, ] <- oilmean

#Get monthly median data
oilmed <- median(oildf$Change..[months(oildf$ï..Date) == "January"])
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "February"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "March"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "April"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "May"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "June"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "July"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "August"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "September"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "October"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "November"]))
oilmed <- c(oilmed, median(oildf$Change..[months(oildf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[7, ] <- oilmed

##########################################################################################################################################################################################
#Lean Hogs#
##########################################################################################################################################################################################
hogs <- read.csv("Lean Hogs Futures Historical Data.csv")
hogsdf <- as.data.frame(hogs, StringsAsFactors = FALSE)

#Remove NA values
hogsdf <- hogsdf[-c(nrow(hogsdf)-1, nrow(hogsdf)), ]

#Get the ï..Date and change columns
hogsdf <- hogsdf[, c(1,7)]

#Convert hogs ï..Date data into data format
hogstime <- hogsdf[, 1]
hogstime<- as.Date(as.character(hogstime), "%b %d, %Y")
hogsdf[, 1]  <- hogstime

hogsdf[,2] = abs(hogsdf[,2])

#Get monthly mean data
hogsmean <- mean(hogsdf$Change..[months(hogsdf$ï..Date) == "January"])
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "February"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "March"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "April"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "May"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "June"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "July"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "August"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "September"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "October"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "November"]))
hogsmean <- c(hogsmean, mean(hogsdf$Change..[months(hogsdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[8, ] <- hogsmean

#Get monthly median data
hogsmed <- median(hogsdf$Change..[months(hogsdf$ï..Date) == "January"])
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "February"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "March"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "April"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "May"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "June"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "July"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "August"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "September"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "October"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "November"]))
hogsmed <- c(hogsmed, median(hogsdf$Change..[months(hogsdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[8, ] <- hogsmed

##########################################################################################################################################################################################
#Live Cattle#
##########################################################################################################################################################################################
catt <- read.csv("Live Cattle Futures Historical Data.csv")
cattdf <- as.data.frame(catt, StringsAsFactors = FALSE)

#Remove NA values
cattdf <- cattdf[-c(nrow(cattdf)-1, nrow(cattdf)), ]

#Get the ï..Date and change columns
cattdf <- cattdf[, c(1,7)]

#Convert catt ï..Date data into data format
catttime <- cattdf[, 1]
catttime<- as.Date(as.character(catttime), "%b %d, %Y")
cattdf[, 1]  <- catttime

cattdf[,2] = abs(cattdf[,2])

#Get monthly mean data
cattmean <- mean(cattdf$Change..[months(cattdf$ï..Date) == "January"])
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "February"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "March"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "April"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "May"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "June"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "July"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "August"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "September"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "October"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "November"]))
cattmean <- c(cattmean, mean(cattdf$Change..[months(cattdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[9, ] <- cattmean

#Get monthly median data
cattmed <- median(cattdf$Change..[months(cattdf$ï..Date) == "January"])
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "February"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "March"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "April"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "May"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "June"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "July"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "August"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "September"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "October"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "November"]))
cattmed <- c(cattmed, median(cattdf$Change..[months(cattdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[9, ] <- cattmed

##########################################################################################################################################################################################
#Lumber#
##########################################################################################################################################################################################
lumb <- read.csv("Lumber Futures Historical Data.csv")
lumbdf <- as.data.frame(lumb, StringsAsFactors = FALSE)

#Remove NA values
lumbdf <- lumbdf[-c(nrow(lumbdf)-1, nrow(lumbdf)), ]

#Get the ï..Date and change columns
lumbdf <- lumbdf[, c(1,7)]

#Convert lumb ï..Date data into data format
lumbtime <- lumbdf[, 1]
lumbtime<- as.Date(as.character(lumbtime), "%b %d, %Y")
lumbdf[, 1]  <- lumbtime

lumbdf[,2] = abs(lumbdf[,2])

#Get monthly mean data
lumbmean <- mean(lumbdf$Change..[months(lumbdf$ï..Date) == "January"])
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "February"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "March"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "April"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "May"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "June"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "July"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "August"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "September"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "October"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "November"]))
lumbmean <- c(lumbmean, mean(lumbdf$Change..[months(lumbdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[10, ] <- lumbmean

#Get monthly median data
lumbmed <- median(lumbdf$Change..[months(lumbdf$ï..Date) == "January"])
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "February"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "March"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "April"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "May"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "June"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "July"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "August"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "September"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "October"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "November"]))
lumbmed <- c(lumbmed, median(lumbdf$Change..[months(lumbdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[10, ] <- lumbmed

##########################################################################################################################################################################################
#Milk#
##########################################################################################################################################################################################
milk <- read.csv("Class III Milk Futures Historical Data.csv")
milkdf <- as.data.frame(milk, StringsAsFactors = FALSE)

#Remove NA values
milkdf <- milkdf[-c(nrow(milkdf)-1, nrow(milkdf)), ]

#Get the ï..Date and change columns
milkdf <- milkdf[, c(1,7)]

#Convert milk ï..Date data into data format
milktime <- milkdf[, 1]
milktime<- as.Date(as.character(milktime), "%b %d, %Y")
milkdf[, 1]  <- milktime

milkdf[,2] = abs(milkdf[,2])

#Get monthly mean data
milkmean <- mean(milkdf$Change..[months(milkdf$ï..Date) == "January"])
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "February"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "March"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "April"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "May"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "June"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "July"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "August"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "September"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "October"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "November"]))
milkmean <- c(milkmean, mean(milkdf$Change..[months(milkdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[11, ] <- milkmean

#Get monthly median data
milkmed <- median(milkdf$Change..[months(milkdf$ï..Date) == "January"])
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "February"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "March"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "April"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "May"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "June"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "July"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "August"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "September"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "October"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "November"]))
milkmed <- c(milkmed, median(milkdf$Change..[months(milkdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[11, ] <- milkmed

##########################################################################################################################################################################################
#Natural Gas#
##########################################################################################################################################################################################
gas <- read.csv("Natural Gas Futures Historical Data.csv")
gasdf <- as.data.frame(gas, StringsAsFactors = FALSE)

#Remove NA values
gasdf <- gasdf[-c(nrow(gasdf)-1, nrow(gasdf)), ]

#Get the ï..Date and change columns
gasdf <- gasdf[, c(1,7)]

#Convert gas ï..Date data into data format
gastime <- gasdf[, 1]
gastime<- as.Date(as.character(gastime), "%b %d, %Y")
gasdf[, 1]  <- gastime

gasdf[,2] = abs(gasdf[,2])

#Get monthly mean data
gasmean <- mean(gasdf$Change..[months(gasdf$ï..Date) == "January"])
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "February"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "March"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "April"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "May"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "June"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "July"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "August"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "September"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "October"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "November"]))
gasmean <- c(gasmean, mean(gasdf$Change..[months(gasdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[12, ] <- gasmean

#Get monthly median data
gasmed <- median(gasdf$Change..[months(gasdf$ï..Date) == "January"])
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "February"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "March"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "April"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "May"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "June"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "July"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "August"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "September"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "October"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "November"]))
gasmed <- c(gasmed, median(gasdf$Change..[months(gasdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[12, ] <- gasmed

##########################################################################################################################################################################################
#Oats#
##########################################################################################################################################################################################
oats <- read.csv("Oats Futures Historical Data.csv")
oatsdf <- as.data.frame(oats, StringsAsFactors = FALSE)

#Remove NA values
oatsdf <- oatsdf[-c(nrow(oatsdf)-1, nrow(oatsdf)), ]

#Get the ï..Date and change columns
oatsdf <- oatsdf[, c(1,7)]

#Convert oats ï..Date data into data format
oatstime <- oatsdf[, 1]
oatstime<- as.Date(as.character(oatstime), "%b %d, %Y")
oatsdf[, 1]  <- oatstime

oatsdf[,2] = abs(oatsdf[,2])

#Get monthly mean data
oatsmean <- mean(oatsdf$Change..[months(oatsdf$ï..Date) == "January"])
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "February"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "March"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "April"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "May"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "June"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "July"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "August"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "September"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "October"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "November"]))
oatsmean <- c(oatsmean, mean(oatsdf$Change..[months(oatsdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[13, ] <- oatsmean

#Get monthly median data
oatsmed <- median(oatsdf$Change..[months(oatsdf$ï..Date) == "January"])
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "February"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "March"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "April"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "May"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "June"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "July"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "August"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "September"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "October"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "November"]))
oatsmed <- c(oatsmed, median(oatsdf$Change..[months(oatsdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[13, ] <- oatsmed

##########################################################################################################################################################################################
#Orange Juice#
##########################################################################################################################################################################################
oj <- read.csv("Orange Juice Futures Historical Data.csv")
ojdf <- as.data.frame(oj, StringsAsFactors = FALSE)

#Remove NA values
ojdf <- ojdf[-c(nrow(ojdf)-1, nrow(ojdf)), ]

#Get the ï..Date and change columns
ojdf <- ojdf[, c(1,7)]

#Convert oj ï..Date data into data format
ojtime <- ojdf[, 1]
ojtime<- as.Date(as.character(ojtime), "%b %d, %Y")
ojdf[, 1]  <- ojtime

ojdf[,2] = abs(ojdf[,2])

#Get monthly mean data
ojmean <- mean(ojdf$Change..[months(ojdf$ï..Date) == "January"])
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "February"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "March"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "April"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "May"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "June"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "July"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "August"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "September"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "October"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "November"]))
ojmean <- c(ojmean, mean(ojdf$Change..[months(ojdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[14, ] <- ojmean

#Get monthly median data
ojmed <- median(ojdf$Change..[months(ojdf$ï..Date) == "January"])
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "February"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "March"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "April"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "May"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "June"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "July"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "August"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "September"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "October"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "November"]))
ojmed <- c(ojmed, median(ojdf$Change..[months(ojdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[14, ] <- ojmed

##########################################################################################################################################################################################
#Palladium#
##########################################################################################################################################################################################
palladium <- read.csv("Palladium Futures Historical Data.csv")
palladiumdf <- as.data.frame(palladium, StringsAsFactors = FALSE)

#Remove NA values
palladiumdf <- palladiumdf[-c(nrow(palladiumdf)-1, nrow(palladiumdf)), ]

#Get the ï..Date and change columns
palladiumdf <- palladiumdf[, c(1,7)]

#Convert palladium ï..Date data into data format
palladiumtime <- palladiumdf[, 1]
palladiumtime<- as.Date(as.character(palladiumtime), "%b %d, %Y")
palladiumdf[, 1]  <- palladiumtime

palladiumdf[,2] = abs(palladiumdf[,2])

#Get monthly mean data
palladiummean <- mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "January"])
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "February"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "March"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "April"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "May"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "June"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "July"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "August"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "September"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "October"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "November"]))
palladiummean <- c(palladiummean, mean(palladiumdf$Change..[months(palladiumdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[15, ] <- palladiummean

#Get monthly median data
palladiummed <- median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "January"])
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "February"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "March"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "April"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "May"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "June"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "July"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "August"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "September"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "October"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "November"]))
palladiummed <- c(palladiummed, median(palladiumdf$Change..[months(palladiumdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[15, ] <- palladiummed

##########################################################################################################################################################################################
#Platinum#
##########################################################################################################################################################################################
platinum <- read.csv("Platinum Futures Historical Data.csv")
platinumdf <- as.data.frame(platinum, StringsAsFactors = FALSE)

#Remove NA values
platinumdf <- platinumdf[-c(nrow(platinumdf)-1, nrow(platinumdf)), ]

#Get the ï..Date and change columns
platinumdf <- platinumdf[, c(1,7)]

#Convert platinum ï..Date data into data format
platinumtime <- platinumdf[, 1]
platinumtime<- as.Date(as.character(platinumtime), "%b %d, %Y")
platinumdf[, 1]  <- platinumtime

platinumdf[,2] = abs(platinumdf[,2])

#Get monthly mean data
platinummean <- mean(platinumdf$Change..[months(platinumdf$ï..Date) == "January"])
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "February"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "March"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "April"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "May"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "June"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "July"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "August"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "September"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "October"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "November"]))
platinummean <- c(platinummean, mean(platinumdf$Change..[months(platinumdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[16, ] <- platinummean

#Get monthly median data
platinummed <- median(platinumdf$Change..[months(platinumdf$ï..Date) == "January"])
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "February"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "March"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "April"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "May"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "June"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "July"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "August"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "September"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "October"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "November"]))
platinummed <- c(platinummed, median(platinumdf$Change..[months(platinumdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[16, ] <- platinummed

##########################################################################################################################################################################################
#RoughRice#
##########################################################################################################################################################################################
roughrice <- read.csv("Rough Rice Futures Historical Data.csv")
roughricedf <- as.data.frame(roughrice, StringsAsFactors = FALSE)

#Remove NA values
roughricedf <- roughricedf[-c(nrow(roughricedf)-1, nrow(roughricedf)), ]

#Get the ï..Date and change columns
roughricedf <- roughricedf[, c(1,7)]

#Convert roughrice ï..Date data into data format
roughricetime <- roughricedf[, 1]
roughricetime<- as.Date(as.character(roughricetime), "%b %d, %Y")
roughricedf[, 1]  <- roughricetime

roughricedf[,2] = abs(roughricedf[,2])

#Get monthly mean data
roughricemean <- mean(roughricedf$Change..[months(roughricedf$ï..Date) == "January"])
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "February"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "March"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "April"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "May"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "June"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "July"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "August"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "September"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "October"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "November"]))
roughricemean <- c(roughricemean, mean(roughricedf$Change..[months(roughricedf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[17, ] <- roughricemean

#Get monthly median data
roughricemed <- median(roughricedf$Change..[months(roughricedf$ï..Date) == "January"])
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "February"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "March"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "April"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "May"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "June"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "July"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "August"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "September"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "October"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "November"]))
roughricemed <- c(roughricemed, median(roughricedf$Change..[months(roughricedf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[17, ] <- roughricemed

##########################################################################################################################################################################################
#Silver#
##########################################################################################################################################################################################
silver <- read.csv("Silver Futures Historical Data.csv")
silverdf <- as.data.frame(silver, StringsAsFactors = FALSE)

#Remove NA values
silverdf <- silverdf[-c(nrow(silverdf)-1, nrow(silverdf)), ]

#Get the ï..Date and change columns
silverdf <- silverdf[, c(1,7)]

#Convert silver ï..Date data into data format
silvertime <- silverdf[, 1]
silvertime<- as.Date(as.character(silvertime), "%b %d, %Y")
silverdf[, 1]  <- silvertime

silverdf[,2] = abs(silverdf[,2])

#Get monthly mean data
silvermean <- mean(silverdf$Change..[months(silverdf$ï..Date) == "January"])
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "February"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "March"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "April"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "May"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "June"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "July"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "August"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "September"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "October"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "November"]))
silvermean <- c(silvermean, mean(silverdf$Change..[months(silverdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[18, ] <- silvermean

#Get monthly median data
silvermed <- median(silverdf$Change..[months(silverdf$ï..Date) == "January"])
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "February"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "March"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "April"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "May"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "June"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "July"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "August"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "September"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "October"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "November"]))
silvermed <- c(silvermed, median(silverdf$Change..[months(silverdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[18, ] <- silvermed

##########################################################################################################################################################################################
#Cocoa#
##########################################################################################################################################################################################
cocoa <- read.csv("US Cocoa Futures Historical Data.csv")
cocoadf <- as.data.frame(cocoa, StringsAsFactors = FALSE)

#Remove NA values
cocoadf <- cocoadf[-c(nrow(cocoadf)-1, nrow(cocoadf)), ]

#Get the ï..Date and change columns
cocoadf <- cocoadf[, c(1,7)]

#Convert cocoa ï..Date data into data format
cocoatime <- cocoadf[, 1]
cocoatime<- as.Date(as.character(cocoatime), "%b %d, %Y")
cocoadf[, 1]  <- cocoatime

cocoadf[,2] = abs(cocoadf[,2])

#Get monthly mean data
cocoamean <- mean(cocoadf$Change..[months(cocoadf$ï..Date) == "January"])
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "February"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "March"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "April"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "May"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "June"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "July"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "August"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "September"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "October"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "November"]))
cocoamean <- c(cocoamean, mean(cocoadf$Change..[months(cocoadf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[19, ] <- cocoamean

#Get monthly median data
cocoamed <- median(cocoadf$Change..[months(cocoadf$ï..Date) == "January"])
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "February"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "March"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "April"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "May"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "June"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "July"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "August"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "September"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "October"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "November"]))
cocoamed <- c(cocoamed, median(cocoadf$Change..[months(cocoadf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[19, ] <- cocoamed

##########################################################################################################################################################################################
#Coffee#
##########################################################################################################################################################################################
coffee <- read.csv("US Coffee C Futures Historical Data.csv")
coffeedf <- as.data.frame(coffee, StringsAsFactors = FALSE)

#Remove NA values
coffeedf <- coffeedf[-c(nrow(coffeedf)-1, nrow(coffeedf)), ]

#Get the ï..Date and change columns
coffeedf <- coffeedf[, c(1,7)]

#Convert coffee ï..Date data into data format
coffeetime <- coffeedf[, 1]
coffeetime<- as.Date(as.character(coffeetime), "%b %d, %Y")
coffeedf[, 1]  <- coffeetime

coffeedf[,2] = abs(coffeedf[,2])

#Get monthly mean data
coffeemean <- mean(coffeedf$Change..[months(coffeedf$ï..Date) == "January"])
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "February"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "March"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "April"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "May"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "June"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "July"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "August"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "September"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "October"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "November"]))
coffeemean <- c(coffeemean, mean(coffeedf$Change..[months(coffeedf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[20, ] <- coffeemean

#Get monthly median data
coffeemed <- median(coffeedf$Change..[months(coffeedf$ï..Date) == "January"])
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "February"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "March"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "April"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "May"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "June"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "July"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "August"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "September"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "October"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "November"]))
coffeemed <- c(coffeemed, median(coffeedf$Change..[months(coffeedf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[20, ] <- coffeemed

##########################################################################################################################################################################################
#Corn#
##########################################################################################################################################################################################
corn <- read.csv("US Corn Futures Historical Data.csv")
corndf <- as.data.frame(corn, StringsAsFactors = FALSE)

#Remove NA values
corndf <- corndf[-c(nrow(corndf)-1, nrow(corndf)), ]

#Get the ï..Date and change columns
corndf <- corndf[, c(1,7)]

#Convert corn ï..Date data into data format
corntime <- corndf[, 1]
corntime<- as.Date(as.character(corntime), "%b %d, %Y")
corndf[, 1]  <- corntime

corndf[,2] = abs(corndf[,2])

#Get monthly mean data
cornmean <- mean(corndf$Change..[months(corndf$ï..Date) == "January"])
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "February"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "March"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "April"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "May"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "June"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "July"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "August"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "September"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "October"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "November"]))
cornmean <- c(cornmean, mean(corndf$Change..[months(corndf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[21, ] <- cornmean

#Get monthly median data
cornmed <- median(corndf$Change..[months(corndf$ï..Date) == "January"])
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "February"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "March"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "April"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "May"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "June"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "July"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "August"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "September"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "October"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "November"]))
cornmed <- c(cornmed, median(corndf$Change..[months(corndf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[21, ] <- cornmed

##########################################################################################################################################################################################
#Cotton#
##########################################################################################################################################################################################
cotton <- read.csv("US Cotton #2 Futures Historical Data.csv")
cottondf <- as.data.frame(cotton, StringsAsFactors = FALSE)

#Remove NA values
cottondf <- cottondf[-c(nrow(cottondf)-1, nrow(cottondf)), ]

#Get the ï..Date and change columns
cottondf <- cottondf[, c(1,7)]

#Convert cotton ï..Date data into data format
cottontime <- cottondf[, 1]
cottontime<- as.Date(as.character(cottontime), "%b %d, %Y")
cottondf[, 1]  <- cottontime

cottondf[,2] = abs(cottondf[,2])

#Get monthly mean data
cottonmean <- mean(cottondf$Change..[months(cottondf$ï..Date) == "January"])
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "February"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "March"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "April"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "May"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "June"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "July"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "August"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "September"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "October"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "November"]))
cottonmean <- c(cottonmean, mean(cottondf$Change..[months(cottondf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[22, ] <- cottonmean

#Get monthly median data
cottonmed <- median(cottondf$Change..[months(cottondf$ï..Date) == "January"])
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "February"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "March"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "April"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "May"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "June"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "July"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "August"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "September"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "October"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "November"]))
cottonmed <- c(cottonmed, median(cottondf$Change..[months(cottondf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[22, ] <- cottonmed

##########################################################################################################################################################################################
#SoybeanMeal#
##########################################################################################################################################################################################
soybeanmeal <- read.csv("US Soybean Meal Futures Historical Data.csv")
soybeanmealdf <- as.data.frame(soybeanmeal, StringsAsFactors = FALSE)

#Remove NA values
soybeanmealdf <- soybeanmealdf[-c(nrow(soybeanmealdf)-1, nrow(soybeanmealdf)), ]

#Get the ï..Date and change columns
soybeanmealdf <- soybeanmealdf[, c(1,7)]

#Convert soybeanmeal ï..Date data into data format
soybeanmealtime <- soybeanmealdf[, 1]
soybeanmealtime<- as.Date(as.character(soybeanmealtime), "%b %d, %Y")
soybeanmealdf[, 1]  <- soybeanmealtime

soybeanmealdf[,2] = abs(soybeanmealdf[,2])

#Get monthly mean data
soybeanmealmean <- mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "January"])
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "February"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "March"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "April"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "May"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "June"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "July"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "August"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "September"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "October"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "November"]))
soybeanmealmean <- c(soybeanmealmean, mean(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[23, ] <- soybeanmealmean

#Get monthly median data
soybeanmealmed <- median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "January"])
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "February"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "March"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "April"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "May"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "June"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "July"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "August"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "September"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "October"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "November"]))
soybeanmealmed <- c(soybeanmealmed, median(soybeanmealdf$Change..[months(soybeanmealdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[23, ] <- soybeanmealmed

##########################################################################################################################################################################################
#SoybeanOil#
##########################################################################################################################################################################################
soybeanoil <- read.csv("US Soybean Oil Futures Historical Data.csv")
soybeanoildf <- as.data.frame(soybeanoil, StringsAsFactors = FALSE)

#Remove NA values
soybeanoildf <- soybeanoildf[-c(nrow(soybeanoildf)-1, nrow(soybeanoildf)), ]

#Get the ï..Date and change columns
soybeanoildf <- soybeanoildf[, c(1,7)]

#Convert soybeanoil ï..Date data into data format
soybeanoiltime <- soybeanoildf[, 1]
soybeanoiltime<- as.Date(as.character(soybeanoiltime), "%b %d, %Y")
soybeanoildf[, 1]  <- soybeanoiltime

soybeanoildf[,2] = abs(soybeanoildf[,2])


#Get monthly mean data
soybeanoilmean <- mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "January"])
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "February"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "March"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "April"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "May"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "June"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "July"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "August"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "September"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "October"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "November"]))
soybeanoilmean <- c(soybeanoilmean, mean(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[24, ] <- soybeanoilmean

#Get monthly median data
soybeanoilmed <- median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "January"])
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "February"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "March"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "April"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "May"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "June"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "July"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "August"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "September"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "October"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "November"]))
soybeanoilmed <- c(soybeanoilmed, median(soybeanoildf$Change..[months(soybeanoildf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[24, ] <- soybeanoilmed

##########################################################################################################################################################################################
#Soybean#
##########################################################################################################################################################################################
soybean <- read.csv("US Soybeans Futures Historical Data.csv")
soybeandf <- as.data.frame(soybean, StringsAsFactors = FALSE)

#Remove NA values
soybeandf <- soybeandf[-c(nrow(soybeandf)-1, nrow(soybeandf)), ]

#Get the ï..Date and change columns
soybeandf <- soybeandf[, c(1,7)]

#Convert soybean ï..Date data into data format
soybeantime <- soybeandf[, 1]
soybeantime<- as.Date(as.character(soybeantime), "%b %d, %Y")
soybeandf[, 1]  <- soybeantime

soybeandf[,2] = abs(soybeandf[,2])

#Get monthly mean data
soybeanmean <- mean(soybeandf$Change..[months(soybeandf$ï..Date) == "January"])
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "February"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "March"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "April"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "May"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "June"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "July"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "August"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "September"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "October"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "November"]))
soybeanmean <- c(soybeanmean, mean(soybeandf$Change..[months(soybeandf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[25, ] <- soybeanmean

#Get monthly median data
soybeanmed <- median(soybeandf$Change..[months(soybeandf$ï..Date) == "January"])
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "February"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "March"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "April"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "May"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "June"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "July"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "August"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "September"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "October"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "November"]))
soybeanmed <- c(soybeanmed, median(soybeandf$Change..[months(soybeandf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[25, ] <- soybeanmed

##########################################################################################################################################################################################
#Sugar#
##########################################################################################################################################################################################
sugar <- read.csv("US Sugar #11 Futures Historical Data.csv")
sugardf <- as.data.frame(sugar, StringsAsFactors = FALSE)

#Remove NA values
sugardf <- sugardf[-c(nrow(sugardf)-1, nrow(sugardf)), ]

#Get the ï..Date and change columns
sugardf <- sugardf[, c(1,7)]

#Convert sugar ï..Date data into data format
sugartime <- sugardf[, 1]
sugartime<- as.Date(as.character(sugartime), "%b %d, %Y")
sugardf[, 1]  <- sugartime

sugardf[,2] = abs(sugardf[,2])

#Get monthly mean data
sugarmean <- mean(sugardf$Change..[months(sugardf$ï..Date) == "January"])
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "February"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "March"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "April"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "May"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "June"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "July"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "August"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "September"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "October"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "November"]))
sugarmean <- c(sugarmean, mean(sugardf$Change..[months(sugardf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[26, ] <- sugarmean

#Get monthly median data
sugarmed <- median(sugardf$Change..[months(sugardf$ï..Date) == "January"])
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "February"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "March"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "April"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "May"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "June"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "July"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "August"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "September"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "October"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "November"]))
sugarmed <- c(sugarmed, median(sugardf$Change..[months(sugardf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[26, ] <- sugarmed

##########################################################################################################################################################################################
#Wheat#
##########################################################################################################################################################################################
#Read in the data
wheat <- read.csv("US Wheat SRW Historical Data.csv")
wheatdf <- as.data.frame(wheat, StringsAsFactors = FALSE)

#Remove NA values
wheatdf <- wheatdf[-c(nrow(wheatdf)-1, nrow(wheatdf)), ]

#Get the ï..Date and change columns
wheatdf <- wheatdf[, c(1,7)]

#Convert sugar ï..Date data into data format
wheatime <- wheatdf[, 1]
wheatime<- as.Date(as.character(wheatime), "%b %d, %Y")
wheatdf[, 1]  <- wheatime

wheatdf[,2] = abs(wheatdf[,2])


#Get monthly mean data
wheatmean <- mean(wheatdf$Change..[months(wheatdf$ï..Date) == "January"])
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "February"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "March"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "April"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "May"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "June"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "July"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "August"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "September"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "October"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "November"]))
wheatmean <- c(wheatmean, mean(wheatdf$Change..[months(wheatdf$ï..Date) == "December"]))

#Add mean data to dataframe 
meandf[27, ] <- wheatmean

#Get monthly median data
wheatmed <- median(wheatdf$Change..[months(wheatdf$ï..Date) == "January"])
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "February"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "March"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "April"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "May"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "June"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "July"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "August"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "September"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "October"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "November"]))
wheatmed <- c(wheatmed, median(wheatdf$Change..[months(wheatdf$ï..Date) == "December"]))

#Add median data to dataframe
mediandf[27, ] <- wheatmed


write.csv(mediandf, file = "median_values_absolute.csv")
write.csv(meandf, file = "mean_values_absolute.csv")
