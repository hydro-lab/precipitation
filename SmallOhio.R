# SMALL OHIO GAGE: Adrian station
OhioSmall <- read.csv("/Users/hydro/Desktop/r-marie/OhioSmall.csv")
OhioSmall <- OhioSmall[1:4068,]
     
# Ohio Small GPM file
GPMOS <- read.csv("/Users/hydro/Desktop/r-marie/GPMOS.csv", skip = 8)
GPMOS <- GPMOS[-c(791, 793, 794, 797, 813, 815, 817, 822, 830, 840, 845, 847, 849, 850, 853, 868, 875, 876, 877, 931, 932, 939, 960, 961, 971, 981, 999),]
GPMOS <- GPMOS[-c(985),]
GPMOS <- GPMOS[-c(1294, 2209, 2362),]

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
