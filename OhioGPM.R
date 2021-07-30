library(readr)
library(dplyr)
library(ncdf4)
library(lubridate)

# uploading Ohio GROUND based data. 8 STATIONS
# file <- file.choose()
OHIO <- read.csv("/Users/hydro/Desktop/r-marie/LargeOhio.csv")
OHIO <- rename(OHIO,  date1 = X, date2 = X.2, date3 = X.4, date4 = X.6, date5 = X.8, date6 = X.10, date7 = X.12, date8 = X.14) # just for a shorter name

OHIO <- read_csv("LargeOhio.csv", col_names = FALSE)
OHIO <- rename(OHIO, d1 = X1, p1 = X2, d2 = X3, p2 = X4, d3 = X5, p3 = X6, d4 = X7, p4 = X8, d5 = X9, p5 = X10, d6 = X11, p6 = X12, d7 = X13, p7 = X14, d8 = X15, p8 = X16) #  
OHIO <- OHIO %>%
      mutate(dt1 = as_date(d1)) %>%
      mutate(dt2 = as_date(d2)) %>%
      mutate(dt3 = as_date(d3)) %>%
      mutate(dt4 = as_date(d4)) %>%
      mutate(dt5 = as_date(d5)) %>%
      mutate(dt6 = as_date(d6)) %>%
      mutate(dt7 = as_date(d7)) %>%
      mutate(dt8 = as_date(d8))
s.date <- min(as.numeric(OHIO$dt1), as.numeric(OHIO$dt2), as.numeric(OHIO$dt3), as.numeric(OHIO$dt4), as.numeric(OHIO$dt5), as.numeric(OHIO$dt6), as.numeric(OHIO$dt7), as.numeric(OHIO$dt8), na.rm = TRUE) #     
e.date <- max(as.numeric(OHIO$dt1), as.numeric(OHIO$dt2), as.numeric(OHIO$dt3), as.numeric(OHIO$dt4), as.numeric(OHIO$dt5), as.numeric(OHIO$dt6), as.numeric(OHIO$dt7), as.numeric(OHIO$dt8), na.rm = TRUE) #     
dt <- c(s.date:e.date) # record dates
wide <- array(NA, dim = c((e.date-s.date+1), 8))
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt1[i] - s.date + 1)),1] <- OHIO$p1[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt2[i] - s.date + 1)),2] <- OHIO$p2[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt3[i] - s.date + 1)),3] <- OHIO$p3[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt4[i] - s.date + 1)),4] <- OHIO$p4[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt5[i] - s.date + 1)),5] <- OHIO$p5[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt6[i] - s.date + 1)),6] <- OHIO$p6[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt7[i] - s.date + 1)),7] <- OHIO$p7[i]
}
for (i in 1:nrow(OHIO)) {
      wide[(as.numeric(OHIO$dt8[i] - s.date + 1)),8] <- OHIO$p8[i]
}
for.averaging <- data.frame(dt,wide)
ohio.ground <- for.averaging %>%
      rowwise() %>%
      mutate(p = mean(c(X1, X2, X3, X4, X5, X6, X7, X8), na.rm = TRUE)) %>%
      mutate(d = as_date(dt)) %>%
      mutate(y = year(d), m = month(d))

y <- array(NA, dim = 32)
#y <- as.numeric(NA)
for (i in 2:32) {
     y[i] <- mean(as.numeric(x[,i]), na.rm = TRUE)
}

# get dates into lubridate format
OHIO$DT <- mdy(OHIO$date1)
OHIO$dt <- as_date(as.Date(OHIO$date1, format = "%m/%d/%y", origin = "1/1/10"))


# columns of year and month 
OHIO <- OHIO %>%
     mutate(year = year(DT))%>%
     mutate(month = month(DT))







# uploading Ohio GPM data
# file <- file.choose()
OGPM <- read.csv("/Users/hydro/Desktop/r-marie/OGPM.csv", skip = 8)

# get OGPM dates into lubridate format
OGPM$DT <- mdy(OGPM$time)

# columns of year and month
OGPM <- OGPM %>%
     mutate(year = year(DT)) %>%
     mutate(month = month(DT))

# get monthly values: result = monthly precipitation values  
OHIO <- OHIO %>%
     group_by(year, month) %>%
     summarise(result = sum(PRCP, na.rm = TRUE), s = sd(PRCP, na.rm = TRUE)) # sum of monthly rain

OGPM <- OGPM %>%
     group_by(year, month) %>%
     summarise(result = sum(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE), s = sd(mean_GPM_3IMERGDF_06_precipitationCal, na.rm = TRUE)) # sum of monthly rain

# making the number of entries equal for OGPM and Ohio
OHIO <- OHIO[1:135,]

# plot the monthly values 
plot(OHIO$result, OGPM$result, xlim = c(0,250), ylim = c(0,250))
abline(lm(OGPM$result ~ OHIO$result)) # line of best fit 

# plot the standard deviations 
plot(OHIO$s, OGPM$s, xlim = c(0,15), ylim = c(0,15))
abline(lm(OGPM$s ~ OHIO$s)) # line of best fit for standard deviation


mean(OHIO$result)
mean(OGPM$result)

# normalizing data: y = x - m / s 
w = (OHIO$result - mean(OHIO$result)) / sd(OHIO$result)
z = (OGPM$result -mean(OGPM$result)) / sd(OGPM$result)

cor(OGPM$result, OHIO$result) # correlation coefficent, r, 0.6107695 before being normalized
cor(w,z)

# find avgs daily across each station 