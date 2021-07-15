# get daily data and convert into monthly Pittsburgh precip data (ohio river)
# both groundwater and GPM, same thing as before 

library(readr)
library(dplyr)
library(ncdf4)
library(lubridate)

# uploading Ohio GROUND based data, which is from the AMESVILLE station in Ohio
file <- file.choose()
OHIO <- read.csv("/Users/hydro/Desktop/r-marie/OHIO.csv")

# remove unneccessary columns 
OHIO <- select(OHIO,-X.STATION...NAME...DATE.., -X, -X.PRC, -X.1, -P.)

# get dates into lubridate format
OHIO$DT <- mdy(OHIO$DATE)

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



