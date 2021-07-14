library(readr)
# install.packages("dplyr")
library(dplyr)
# install.packages("ncdf4")
library(ncdf4)
library(lubridate)

# import GPM and Albasini data 
file <- file.choose()
GPM2 <- read.csv("/Users/hydro/Desktop/r-marie/GPM2.csv", skip = 8) 

file <- file.choose()
Albasini <- read.csv("/Users/hydro/Desktop/r-marie/Albasini.csv", skip = 10, header = FALSE, stringsAsFactors = FALSE)
names(Albasini) <- c("Date","DailyRain", "Flag")

Albasini$DT <- ymd(as.numeric(Albasini$Date))
Albasini <- Albasini[1:4169,] # remove last line which is a footer
Albasini <- select(Albasini,-Flag)

GPM2$DT <- as_date(parse_date_time(GPM2$time, "mdy"))

# lining up dates for both satellite and rain gage
Comp <- array(NA, dim = c(max(c((nrow(Albasini)), (nrow(GPM2)))),2))
dates <- array(NA, dim = nrow(Comp))
startdate <- min(c((as.numeric(Albasini$DT[1])), (as.numeric(GPM2$DT[1]))))

for (i in (1:nrow(Comp))) {
     dates[i] <- as_date(i + startdate - 1)
} # these are the dates 

for (i in (1:nrow(GPM2))) {
     j <- as.numeric(GPM2$DT[i]) - as.numeric(startdate - 1) # location for the sat. data by date
     Comp[j,1] <- GPM2$mean_GPM_3IMERGDL_06_precipitationCal[i]
} 

for (i in (1:nrow(Albasini))) {
     j <- as.numeric(Albasini$DT[i]) - as.numeric(startdate - 1) # location for rain gage
     Comp[j,2] <- Albasini$DailyRain[i]
}       

Comp <- data.frame(dates,Comp)
names(Comp) <- c("date","GPM2", "Albasini")
plot(Comp$GPM2, Comp$Albasini)

# find monthly sum and standard deviation

GPM2 <- GPM2 %>%
     mutate(month = month(DT)) %>%
     mutate(year = year(DT))

Albasini <- Albasini %>%
     mutate(month = month(DT))%>%
     mutate(year = year(DT))


AlbasiniMonthly <- Albasini %>%
     group_by(year, month) %>%
     summarise(result = sum(DailyRain, na.rm = TRUE), s = sd(DailyRain, na.rm = TRUE)) # sum of monthly rain

GPM2Monthly <- GPM2 %>%
     group_by(year, month) %>%
     summarise(result = sum(mean_GPM_3IMERGDL_06_precipitationCal), s = sd(mean_GPM_3IMERGDL_06_precipitationCal)) # sum of monthly rain

GPM2Monthly <- GPM2Monthly %>%
     filter(year > 2009) # removing years 2009 and below because Albasini is from 2010 on
GPM2Monthly <- GPM2Monthly[1:137,] # Albasini data missing for june and july

plot(GPM2Monthly$result, AlbasiniMonthly$result, xlim = c(0,500), ylim = c(0,500))

plot(GPM2Monthly$s, AlbasiniMonthly$s)
