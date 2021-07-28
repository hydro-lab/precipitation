
library(readr)
library(dplyr)
library(ncdf4)
library(lubridate)
library(ggplot2)


## GROUND-BASED STATIONS

# do medium ohio dataset which is in ANNA and ALVORDTON 
Alvordton <- read_csv("/Users/hydro/Desktop/r-marie/Alvordton.csv")
Alvordton <- Alvordton[1:4087,]
Alvordton = subset(Alvordton, select = -c(`"STATION","NAME","DATE",`, `"PRC`,`P"`, X4, X5))
Alvordton <- rename(Alvordton, DATE = X6, prcp = X7)
Alvordton$dt <- as_date(parse_date_time(Alvordton$DATE, "mdy")) # Parses date to a date number

Anna <- read_csv("/Users/hydro/Desktop/r-marie/ANNA.csv", col_names = FALSE)
Anna = subset(Anna, select = -c(X1, X2, X3, X4, X5))
Anna <- rename(Anna, DATE = X6, prcp = X7)
Anna$DT <- as_date(fast_strptime(Anna$DATE, '\"%Y-%m-%d\"'))

# REMEMBER to use na.rm 

s.date <- as.numeric(min(c(min(as_date(Alvordton$dt)), min(as_date(Anna$DT))))) # find earliest date
e.date <- as.numeric(max(c(max(as_date(Alvordton$dt)), max(as_date(Anna$DT))))) # find latest date
dt <- array(NA, dim = (e.date-s.date+1)) # preallocate arrays that contain the maximum days
AN <- dt
AL <- dt
dt <- c(s.date:e.date) # record dates
for (i in 1:nrow(Alvordton)) {
     AL[as.numeric(as_date(Alvordton$dt[i]))-s.date+1] <- Alvordton$prcp[i] # write data to new arrays by date
}
for (i in 1:nrow(Anna)) {
     AN[as.numeric(as_date(Anna$DT[i]))-s.date+1] <- Anna$prcp[i]
}

OhioMed <- data.frame(dt, AL, AN)
OhioMed <- OhioMed %>%
     mutate(OhioMed, prcp = rowMeans(select(OhioMed, AL, AN), na.rm = TRUE)) # This appears to work; but if it doesn't, the next loop should.
OhioMed$prcp <- NA
for (i in 1:nrow(OhioMed)) { # for everthing in the new sorted dataframe
     if (is.na(OhioMed$AL[i]) == FALSE) { # IF AL is a number
          if (is.na(OhioMed$AN[i]) == FALSE) { # AND AN is a number
               OhioMed$prcp[i] <- (OhioMed$AL[i] + OhioMed$AN[i]) / 2
          } else { # AL is a number but AN is not a number (NA)
               OhioMed$prcp[i] <- OhioMed$AL[i] 
          }
     } else {
          if (is.na(OhioMed$AN[i]) == FALSE) {
               OhioMed$prcp[i] <- OhioMed$AN[i]
          }
     }
}


# UPLOAD TO GITHUB!!!


## SATELLITE DATA

# GPM Medium Ohio dataset 
GPMOM <- read_csv("/Users/hydro/Desktop/r-marie/GPMOM.csv", skip = 8)

# get dates into lubridate format
GPMOM$DT <- as.numeric(mdy(GPMOM$time)) #changed this because the ground data are organized this way, OhioMed$dt

# # columns of year and month 
# GPMOM <- GPMOM %>%
#      mutate(year = year(DT))%>%
#      mutate(month = month(DT))

s.date <- as.numeric(min(c(min(as_date(OhioMed$dt)), min(as_date(GPMOM$DT))))) # find earliest date
e.date <- as.numeric(max(c(max(as_date(OhioMed$dt)), max(as_date(GPMOM$DT))))) # find latest date
dt <- array(NA, dim = (e.date-s.date+1)) # preallocate arrays that contain the maximum days
Ohio <- dt
GPM <- dt
dt <- c(s.date:e.date) # record dates
for (i in 1:nrow(OhioMed)) {
     Ohio[as.numeric(as_date(OhioMed$dt[i]))-s.date+1] <- OhioMed$prcp[i] # write data to new arrays by date
}
for (i in 1:nrow(GPMOM)) {
     GPM[as.numeric(as_date(GPMOM$DT[i]))-s.date+1] <- GPMOM$mean_GPM_3IMERGDF_06_precipitationCal[i]
}

comp <- data.frame(dt, Ohio, GPM) # daily data for averaged ground data and satellite

# plot the daily values 
#plot(OhioMed$prcp, GPMOM$mean_GPM_3IMERGDF_06_precipitationCal)

#abline(lm(GPMOM$mean_GPM_3IMERGDF_06_precipitationCal ~ OhioMed$prcp)) # line of best fit                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  

ggplot(comp, aes(x = Ohio, y = GPM)) +
      geom_point() +
      labs(x = "Ground-based Station, OH, USA", y = "Global Precipitation Mission (satellite)") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

mon <- comp %>%
      mutate(ym = (100 * year(as_date(dt))) + month(as_date(dt))) %>%
      group_by(ym) %>%
      summarize(ohio.m = sum(Ohio), ohio.s = sd(Ohio), gpm.m = sum(GPM), gpm.s = sd(GPM))
ggplot(mon, aes(x = ohio.m, y = gpm.m)) +
      geom_point() +
      labs(x = "Ground-based Station, OH, USA", y = "Global Precipitation Mission (satellite)") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))
ggplot(mon, aes(x = ohio.s, y = gpm.s)) +
      geom_point() +
      labs(x = "Ground-based Station, OH, USA", y = "Global Precipitation Mission (satellite)") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

yea <- comp %>%
      mutate(y = year(as_date(dt))) %>%
      group_by(y) %>%
      summarize(ohio.m = sum(Ohio), ohio.s = sd(Ohio), gpm.m = sum(GPM), gpm.s = sd(GPM))
ggplot(yea, aes(x = ohio.m, y = gpm.m)) +
      geom_point() +
      labs(x = "Ground-based Station, OH, USA", y = "Global Precipitation Mission (satellite)") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))
ggplot(yea, aes(x = ohio.s, y = gpm.s)) +
      geom_point() +
      labs(x = "Ground-based Station, OH, USA", y = "Global Precipitation Mission (satellite)") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

# # plot the standard deviations 
# plot(MedOhio$s, GPMOM$s) 
# abline(lm(GPMOM$s ~ MedOhio$s)) # line of best fit for standard deviation
# 
# 
# 
# # get monthly values: result = monthly precipitation values  
#  # GPMOM <- GPMOM %>%
#      #group_by(year, month) %>%
#      #summarise(result = sum(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE), s = sd(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE)) # sum of monthly rain

