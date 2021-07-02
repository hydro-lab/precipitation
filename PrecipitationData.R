install.packages("dplyr")
library(dplyr)
install.packages("ncdf4")
library(ncdf4)
library(lubridate)
library(lubridate)


file <- file.choose()
PrecipData <- read.csv(file, stringsAsFactors = FALSE) # South African sanitation department data 

file <- file.choose()
SatelliteData <- read.csv(file, stringsAsFactors = FALSE)# satellite data 


GPM <- select(SatelliteData, DATE, GPM)

TRMM <- select(SatelliteData, DATE, TRMM)

GDLAS <- select(SatelliteData, DATE, GDLAS)

GDLAS$dt <- as_date(as.Date(GDLAS$DATE, format = "%m/%d/%y", origin = "1/1/70"))
GDLAS$day <- day(GDLAS$dt)
GDLAS$month <- month(GDLAS$dt)
GDLAS$year <- year(GDLAS$dt)


# DeRustHartbeesport <- select(PrecipData, Date, De.RustHartbeesport)
# 
# Rietvlei <- select(PrecipData, Date, Rietvlei)
# 
# Doornkraal <- select(PrecipData, Date, Doornkraal)
# 
# KalkDam <- select(PrecipData, Date, Kalk.Dam)
# 
# Rondebosch <- select (PrecipData, Date, Rondebosch)
# 
# Groenfontein <- select(PrecipData, Date, Groenfontein)
# 
# Loskop <- select(PrecipData, Date, Loskop)
# 
# RustDeWinter <- select(PrecipData, Date, Rust.De.Winter)
# 
# Rhenosterkop <- select(PrecipData, Date, Rhenosterkop)
# 
# Buffelskloof <- select(PrecipData, Date, Buffelskloof)
# 
# Tambotieboom <- select (PrecipData, Date, Tambotieboom)
# 
# Rozenkranz <- select(PrecipData, Date, Rozenkranz)
# 
# Blyderiv.Poortnatres <- select(PrecipData, Date, Blyderiv.Poortnatres)
# 
# Guernsey <- select(PrecipData, Date, Guernsey)
# 
# ToursDam <- select(PrecipData, Date, Tours.Dam)
# 
# Onverwacht <- select(PrecipData, Date, Onverwacht)
# 
# Doornhoek <- select(PrecipData, Date, Doornhoek)
# 
# WoodbushForest <- select(PrecipData, Date, Woodbush.Forest)
# 
# Turksvygbult <- select(PrecipData, Date, Turksvygbult)
# 
# Modjadjes424 <- select(PrecipData, Date, Modjadjes.424)
# 
# Lotteringskop <- select(PrecipData, Date, Lotteringskop)
# 
# Mokolo <- select(PrecipData, Date, Mokolo)
# 
# DoorndraaiDam <- select(PrecipData, Date, Doorndraai.Dam)
# 
# GlenAlpine <- select(PrecipData, Date, Glen.Alpine)
# 
# DuToitsKraal <- select(PrecipData, Date, Du.Toits.Kraal)
# 
# Pietersburg <- select(PrecipData, Date, Pietersburg)
# 
# Nairobi <- select(PrecipData, Date, Nairobi)
# 
# Nwanedzi <- select(PrecipData, Date, Nwanedzi)
# 
# Beaconsfield <- select(PrecipData, Date, Beaconsfield)
# 
# Goedehoop <- select(PrecipData, Date, Goedehoop)
# 
# NandoniDam <- select(PrecipData, Date, Nandoni.Dam)



getwd()
setwd("/Users/hydro/Desktop/r-marie")
# For daily data from NetCDF file format
x <- nc_open("gpcp_v01r03_daily_d20100115_c20170814.nc")
attributes(x)$names
y <- ncvar_get(x, attributes(x$var)$names[4])


PrecipData[1,1]

# Average precip across basin per year
z <- array(NA, dim = 10)
for (i in 1:10) {
     z[i] <- mean(as.numeric(x[i,2:32]), na.rm = TRUE)
}

plot(year,z)

# Average precip at each station (used to evaluate stations)
y <- array(NA, dim = 32)
#y <- as.numeric(NA)
for (i in 2:32) {
  y[i] <- mean(as.numeric(x[,i]), na.rm = TRUE)
}


# GPM
#install.packages("lubridate")
# Get sum of monthly and yearly GPM precip measurements 


# convert date info in format mm/dd/yyyy
GPM$dt <- as_date(as.Date(GPM$DATE, format = "%m/%d/%y", origin = "1/1/10"))
GPM$month <- month(GPM$dt)
GPM$year <- year(GPM$dt)

# gets rid of unnecessary 'X' columns 
PrecipData <- subset(PrecipData, select = -c(X, X.1, X.2, X.3, X.4, X.5, X.6))

# date formatting 
PrecipData$dt <- as_date(as.Date(PrecipData$Date, format = "%m/%d/%y", origin = "8/1/10"))
PrecipData$month <- month(PrecipData$dt)
PrecipData$year <- year(PrecipData$dt)

r <- paste(01,PrecipData$Date,sep = "-")
k <- dmy(r)
m <- month(k)
w <- year(k)

k[1]

PrecipDataLong <- PrecipData %>%
     select(Year, Date) %>% 
     pivot_longer

# to plot PrecipData, use plot(k,y)

# averages by station
y <- array(NA, dim = 32)
for (i in 1:143) {
     y[i] <- mean(as.numeric(PrecipData[i,2:32]), na.rm = TRUE)
}

# gets the monthly averages of GPM for each year 
GPMMonthly <- GPM %>%
     mutate(ym = 100 * year + month)%>%
     group_by(ym)%>%
     summarise(sum_GPM = sum(GPM))
GPMMonthly$dt <- ymd(GPMMonthly$ym *100 + 01)

# new table for GPM, each row is a month, each column is a year
GPM <- array(NA, dim = c(12, 11))
DWS <- GPM # DWS -> department of water sanitation 
for (i in 1:nrow(GPMMonthly)) {
     s <- year(GPMMonthly$dt[i]) - 2009
     t <- month(GPMMonthly$dt[i])
     GPM[t,s] <- GPMMonthly$sum_GPM[i]
}

# PrecipData new table, column is a year, row is a month
for (i in 1:length(y)) {
     s <- year(k[i]) - 2009
     t <- month(k[i])
     DWS[t,s] <- y[i]
}







     