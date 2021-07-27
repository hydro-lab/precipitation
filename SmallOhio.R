library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# SMALL OHIO GAGE: Adrian station
#OhioSmall <- read.csv("/Users/hydro/Desktop/r-marie/OhioSmall.csv")
OhioSmall <- read_csv("OhioSmall.csv") # for David's computer
OhioSmall$dt <- parse_date_time(OhioSmall$DATE, "mdy") # Parses date to a date number
#OhioSmall <- OhioSmall[1:4068,] # What is the purpose of this line?
     
# Ohio Small GPM file
# GPMOS <- read.csv("/Users/hydro/Desktop/r-marie/GPMOS.csv", skip = 8)
GPMOS <- read_csv("GPMOS.csv", skip = 8) # for David's Computer
GPMOS <- rename(GPMOS, date = time, prcp = mean_GPM_3IMERGDF_06_precipitationCal) # just for a shorter name
GPMOS$dt <- parse_date_time(GPMOS$date, "mdy") # Parses date to a date number
#GPMOS <- GPMOS[-c(791, 793, 794, 797, 813, 815, 817, 822, 830, 840, 845, 847, 849, 850, 853, 868, 875, 876, 877, 931, 932, 939, 960, 961, 971, 981, 999),]
#GPMOS <- GPMOS[-c(985),]
#GPMOS <- GPMOS[-c(1294, 2209, 2362),]

s.date <- as.numeric(min(c(min(as_date(OhioSmall$dt)), min(as_date(GPMOS$dt))))) # find earliest date
e.date <- as.numeric(max(c(max(as_date(OhioSmall$dt)), max(as_date(GPMOS$dt))))) # find latest date
dt <- array(NA, dim = (e.date-s.date+1)) # preallocate arrays that contain the maximum days
adrian <- dt
gpm <- dt
dt <- c(s.date:e.date) # record dates
for (i in 1:nrow(adrian)) {
      adrian[as.numeric(as_date(OhioSmall$dt[i]))-s.date+1] <- OhioSmall$PRCP[i] # write data to new arrays by date
}
for (i in 1:nrow(GPMOS)) {
      gpm[as.numeric(as_date(GPMOS$dt[i]))-s.date+1] <- GPMOS$prcp[i]
}
dailycomp <- data.frame(dt, adrian, gpm) # form single data frame, needed for ggplot2
ggplot(dailycomp, aes(x = adrian, y = gpm)) +
      geom_point() +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

# plot daily 
plot(OhioSmall$PRCP, GPMOS$mean_GPM_3IMERGDF_06_precipitationCal)

# get dates into lubridate format
OhioSmall$DT <- mdy(OhioSmall$DATE)
 
# columns of year and month 
OhioSmall <- OhioSmall %>%
     mutate(year = year(DT))%>%
     mutate(month = month(DT))


# get dates into lubridate format
GPMOS$DT <- mdy(GPMOS$time)

# columns of year and month 
GPMOS <- GPMOS %>%
     mutate(year = year(DT))%>%
     mutate(month = month(DT))

# get monthly values: result = monthly precipitation values  
OhioSmall <- OhioSmall %>%
     group_by(year, month) %>%
     summarise(result = sum(PRCP, na.rm = TRUE), s = sd(PRCP, na.rm = TRUE)) # sum of monthly rain

GPMOS <- GPMOS %>%
     group_by(year, month) %>%
     summarise(result = sum(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE), s = sd(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE)) # sum of monthly rain

# making the number of entries equal for OGPM and Ohio
OhioSmall <- OhioSmall[1:135,]

# plot the monthly values 
plot(OhioSmall$result, GPMOS$result, xlim = c(0,250), ylim = c(0,250))
abline(lm(GPMOS$result ~ OhioSmall$result)) # line of best fit 

# plot the standard deviations 
plot(OhioSmall$s, GPMOS$s, xlim = c(0,25), ylim = c(0,25)) 
abline(lm(GPMOS$s ~ OhioSmall$s)) # line of best fit for standard deviation
