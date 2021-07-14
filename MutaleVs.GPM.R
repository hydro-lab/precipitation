library(readr)
# install.packages("dplyr")
library(dplyr)
# install.packages("ncdf4")
library(ncdf4)
library(lubridate)
library(lubridate)

# file <- file.choose()
GPMDailyPrecip <- read_csv("g4.areaAvgTimeSeries.GPM_3IMERGDF_06_precipitationCal.20190301-20210701.30E_22S_30E_22S.csv", skip = 8)
# file <- file.choose()
MutaleWeirDataset <- read_csv("Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv")

# get rid of unneeded columns in Mutale dataset (JUST dates and precipitation)
# MutalePrecip <- select(MutaleWeirDataset, YEAR, MONT, DAYN, HOUR, MINU, PRCP)

# convert -9999 values into NA
MutaleWeirDataset <- MutaleWeirDataset %>%
    select(YEAR, MONT, DAYN, HOUR, MINU, PRCP)%>%
     mutate(Date = ymd(paste(YEAR, MONT, DAYN))) %>%
     na_if(-9999) %>%
     na_if(-8888) %>%
     na_if(-7777) 
     
# condense Mutale into daily values
MutaleDaily <- MutaleWeirDataset %>%
     group_by(Date) %>%
     summarise(DAILYPRCP = sum(PRCP, na.rm = TRUE))

# compare Mutale to GPM Daily Precip
Comp <- array(NA, dim = c(max(c((nrow(MutaleDaily)), (nrow(GPMDailyPrecip)))),2))
dates <- array(NA, dim = nrow(Comp))
startdate <- min(c((as.numeric(MutaleDaily$Date[1])), (as.numeric(GPMDailyPrecip$time[1]))))

for (i in (1:nrow(Comp))) {
     dates[i] <- as_date(i + startdate - 1)
}

for (i in (1:nrow(GPMDailyPrecip))) {
     j <- as.numeric(GPMDailyPrecip$time[i]) - as.numeric(ymd("2019-02-28"))
     Comp[j,1] <- GPMDailyPrecip$mean_GPM_3IMERGDF_06_precipitationCal[i]
} 

for (i in (1:nrow(MutaleDaily))) {
     j <- as.numeric(MutaleDaily$Date[i]) - as.numeric(ymd("2019-02-28"))
     Comp[j,2] <- MutaleDaily$DAILYPRCP[i]
}       
 
Comp <- data.frame(dates,Comp)
names(Comp) <- c("date","GPM", "Mutale")


# get different satellites and different degrees to compare to this data 
# NEW SATELLITE DATA: 
# GPM2: Coordinates 25.31,-22.73,30.46,-22.12

file <- file.choose()
GPM2 <- read.csv(file, skip = 8) 

# get 2010-21 data Goedehoop-Albasini 
# ask sophia if she has daily data for Goedehoop Albasini Dam
# adjust dates on GPM2 into 1970-01-01 format



file <- file.choose()
Albasini <- read.csv(file, skip = 10, header = FALSE, stringsAsFactors = FALSE)
names(Albasini) <- c("Date","DailyRain", "Flag", "Empty")
Albasini <- select(Albasini, -Empty)

Albasini$DT <- ymd(as.numeric(Albasini$Date))
Albasini <- Albasini[1:4169,]

GPM2$DT <- ymd(as.numeric(GPM2$time))
GPM2$DT <- as_date(parse_date_time(GPM2$time, "mdy"))

Comp <- array(NA, dim = c(max(c((nrow(Albasini)), (nrow(GPM2)))),2))
dates <- array(NA, dim = nrow(Comp))
startdate <- min(c((as.numeric(Albasini$DT[1])), (as.numeric(GPM2$DT[1]))))

for (i in (1:nrow(Comp))) {
     dates[i] <- as_date(i + startdate - 1)
}

for (i in (1:nrow(GPM2))) {
     j <- as.numeric(GPM2$DT[i]) - as.numeric(startdate - 1)
     Comp[j,1] <- GPM2$mean_GPM_3IMERGDL_06_precipitationCal[i]
} 

for (i in (1:nrow(Albasini))) {
     j <- as.numeric(Albasini$DT[i]) - as.numeric(startdate - 1)
     Comp[j,2] <- Albasini$DailyRain[i]
}       

Comp <- data.frame(dates,Comp)
names(Comp) <- c("date","GPM2", "Albasini")

# find monthly mean 

GPM2Monthly <- GPM2 %>%
     mutate(month = month(DT)) %>%
     mutate(year = year(DT))

AlbasiniMonthly <- Albasini %>%
     mutate(month = month(DT))%>%
     mutate(year = year(DT))
     
AlbasiniMonthly <- select(AlbasiniMonthly, -Flag)

AlbasiniMonthly <- AlbasiniMonthly %>%
     group_by(month, year) %>%
     summarise(result = sum(DailyRain))

GPM2Monthly <- GPM2Monthly %>%
     group_by(month, year) %>%
     summarise(result = mean(mean_GPM_3IMERGDL_06_precipitationCal))

GPM2Monthly <- GPM2Monthly[-c((1:9),(22:30),(43:51),(64:72),(85:93),(106:115),(128:137),(150:159),(171:180),(192:201),(213:222),(234:243)),]
GPM2Monthly <- GPM2Monthly[-72,]
GPM2Monthly <- GPM2Monthly[-83,]

plot(GPM2Monthly$result, AlbasiniMonthly$result, xlim = c(0,20), ylim = c(0,20))

# find standard deviation
AlbasiniSD <- 

AlbasiniMean = AlbasiniMonthly$result
sd(AlbasiniMean) # 68.54552

GPM2Mean = GPM2Monthly$result
sd(GPM2Mean) # 2.117951

plot(sd(GPM2Mean), sd(Albasini)) # Does this need to be plotted/is this right?

mean(AlbasiniMonthly$result)

