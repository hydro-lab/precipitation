file <- file.choose()
PrecipData <- read.csv(file, stringsAsFactors = FALSE) # South African sanitation department data 

#file <- file.choose()
SatOtherData <- read.csv("CompiledPrecipData.csv", stringsAsFactors = FALSE)# satellite data and a little bit more 

GPM <- select(SatelliteData, DATE, GPM)

TRMM <- select(SatelliteData, DATE, TRMM)

GDLAS <- select(SatelliteData, DATE, GDLAS)

GDLAS$dt <- as_date(as.Date(GDLAS$DATE, format = "%m/%d/%y", origin = "1/1/70"))
GDLAS$day <- day(GDLAS$dt)


DeRustHartbeesport <- select(PrecipData, Date, De.RustHartbeesport)

Rietvlei <- select(PrecipData, Date, Rietvlei)

Doornkraal <- select(PrecipData, Date, Doornkraal)

KalkDam <- select(PrecipData, Date, Kalk.Dam)

Rondebosch <- select (PrecipData, Date, Rondebosch)

Groenfontein <- select(PrecipData, Date, Groenfontein)

Loskop <- select(PrecipData, Date, Loskop)

RustDeWinter <- select(PrecipData, Date, Rust.De.Winter)

Rhenosterkop <- select(PrecipData, Date, Rhenosterkop)

Buffelskloof <- select(PrecipData, Date, Buffelskloof)

Tambotieboom <- select (PrecipData, Date, Tambotieboom)

Rozenkranz <- select(PrecipData, Date, Rozenkranz)

Blyderiv.Poortnatres <- select(PrecipData, Date, Blyderiv.Poortnatres)

Guernsey <- select(PrecipData, Date, Guernsey)

ToursDam <- select(PrecipData, Date, Tours.Dam)

Onverwacht <- select(PrecipData, Date, Onverwacht)

Doornhoek <- select(PrecipData, Date, Doornhoek)

SatelliteData <- select(SatOtherData, DATE, GPM, TRMM, GDLAS)

WoodbushForest <- select(PrecipData, Date, Woodbush.Forest)

Turksvygbult <- select(PrecipData, Date, Turksvygbult)

Modjadjes424 <- select(PrecipData, Date, Modjadjes.424)

Lotteringskop <- select(PrecipData, Date, Lotteringskop)

Mokolo <- select(PrecipData, Date, Mokolo)

DoorndraaiDam <- select(PrecipData, Date, Doorndraai.Dam)

GlenAlpine <- select(PrecipData, Date, Glen.Alpine)

DuToitsKraal <- select(PrecipData, Date, Du.Toits.Kraal)

Pietersburg <- select(PrecipData, Date, Pietersburg)

Nairobi <- select(PrecipData, Date, Nairobi)

Nwanedzi <- select(PrecipData, Date, Nwanedzi)

Beaconsfield <- select(PrecipData, Data, Beaconsfield)

Goedehoop <- select(PrecipData, Date, Goedehoop)

NandoniDam <- select(PrecipData, Date, Nandoni.Dam)

install.packages("ncdf4")

library(ncdf4)

getwd()
setwd("/Users/hydro/Desktop/r-marie")
# For daily data from NetCDF file format
x <- nc_open("gpcp_v01r03_daily_d20100115_c20170814.nc")
attributes(x)$names
y <- ncvar_get(x, attributes(x$var)$names[4])

library(lubridate)

PrecipData[1,1]

# Average precip across basin per year
x <- filter(PrecipData[,1:32], Date == "Annual Total")
year <- c(2011:2020)
z <- array(NA, dim = 10)
for (i in 1:10) {
  z[i] <- mean(as.numeric(x[i,]), na.rm = TRUE)
}

# Average precip at each station (used to evaluate stations)
y <- array(NA, dim = 32)
for (i in 2:32) {
  y[i] <- mean(as.numeric(x[,i]), na.rm = TRUE)
}






