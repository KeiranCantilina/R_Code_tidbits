## Script to calculate phase of the moon from the current date
date_raw <- Sys.Date()
year <- as.numeric(substr(date_raw,1,4))
month <- as.numeric(substr(date_raw,6,7))
day <- as.numeric(substr(date_raw,9,10))



julday <- function(year, month, day) {
  
  jy <- as.integer(year)
  jm <- as.integer(month)+1
  
  if (month <= 2) {
    jy <- jy-1	
    jm <- jm + 12	
    } 
  
  jul <- floor(365.25 *jy) + floor(30.6001 * jm) + as.integer(day) + 1720995
  if (day+31*(month+12*year) >= (15+31*(10+12*1582))) {
    ja = floor(0.01 * jy)
    jul = jul + 2 - ja + floor(0.25 * ja)
  }
  
  return(jul);
}

Trig2 <- function(year,month,day) {
  n <- floor(12.37 * (year -1900 + ((1.0 * month - 0.5)/12.0)))
  RAD <- 3.14159265/180.0
  t <- n / 1236.85
  t2 <- t * t
  as <- 359.2242 + 29.105356 * n
  am <- 306.0253 + 385.816918 * n + 0.010730 * t2
  xtra <- 0.75933 + 1.53058868 * n + ((1.178e-4) - (1.55e-7) * t) * t2
  xtra <- xtra + ((0.1734 - 3.93e-4 * t) * sin(RAD * as) - 0.4068 * sin(RAD * am))
  
  i <- if(xtra > 0.0) floor(xtra) else ceiling(xtra - 1.0)
  
  j1 <- julday(year,month,day)
  jd <- (2415020 + 28 * n) + i
  return((j1-jd + 30)%%30)
}

phase <- Trig2(year,month,day)
