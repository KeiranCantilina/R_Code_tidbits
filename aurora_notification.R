## Script for Aurora notification (1 hour advance notice)

library(RCurl)
library(rio)
library(sendmailR)
library(stringr)


## If it's a new year, retrieve new sunset-sunrise tables
if(((substr(Sys.Date(),6,10))=="01-01")&(substr(Sys.time(),12,13)=="01")){
  #Sunset data retrieval
  sunset_url <- paste("http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=",substr(Sys.Date(),1,4),"&task=0&state=MN&place=St.+Paul",sep="")
  sunset_dest <- "C://Users//Keiran//Documents//sunset_temp.txt"
  sunset_csv <- download.file(sunset_url,sunset_dest,"internal",mode = "w")
  sunset_csv <- read.fwf(sunset_dest, widths = c(2,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4),header = FALSE,skip = 31,nrows = 31)
  sunset_csv_cleaned <- sunset_csv[,c(seq(1,50,2))]
  sunrise_table <- sunset_csv_cleaned[,c(1,seq(2,25,2))]
  sunset_table <- sunset_csv_cleaned[,c(1,seq(3,25,2))]
  colnames(sunrise_table) <- c("Day",sprintf("%02d",1:12))
  sunrise_table$Day <- c(sprintf("%02d",1:31))
  colnames(sunset_table) <- c("Day",sprintf("%02d",1:12))
  sunrise_table$Day <- c(sprintf("%02d",1:31))
  
  export(sunset_table, "C://Users//Keiran//Documents//sunset_table.csv")
  export(sunrise_table, "C://Users//Keiran//Documents//sunrise_table.csv")
  
}

## Destination file
destfile <- "C://Users//Keiran//Documents//Aurora_temp.csv"

## Download file
url <- "http://services.swpc.noaa.gov/text/wing-kp.txt"
csv <- download.file(url,destfile,"internal",mode = "w")
csv <- import(destfile)

colnames(csv) <- c("YR_now","MO_now","DAY_now","UTC_now","YR_1HR","MO_1HR","DAY_1HR","UTC_1HR","Kp_1HR","YR_4HR","MO_4HR","DAY_4HR","UTC_4HR","Kp_4HR","USAF_Kp")
rows <- nrow(csv)

data <- csv[rows,]


#Time format correction
data[,13] <- sprintf("%04d",data[,13])
data[,4] <- sprintf("%04d",data[,4])
data[,8] <- sprintf("%04d",data[,8])


#Time calculations
time_now_UTC <- as.POSIXlt(Sys.time(), tz = "gmt")
time_now_UTC <- as.POSIXct(time_now_UTC, tz = "GMT")
time_1hr_UTC <- as.POSIXct(paste(data[,5],"-",sprintf("%02d",data[,6]),"-",sprintf("%02d",data[,7])," ",substr(sprintf("%02s",data[,8]),1,2),":",substr(sprintf("%02s",data[,8]),3,4),":00",sep=""), tz="GMT")
time_1hr_local <- as.POSIXlt(time_1hr_UTC, tz= "America/Chicago")
time_4hr_UTC <- as.POSIXct(paste(data[,10],"-",sprintf("%02d",data[,11]),"-",sprintf("%02d",data[,12])," ",substr(sprintf("%02s",data[,13]),1,2),":",substr(sprintf("%02s",data[,13]),3,4),":00",sep=""),tz="GMT")
time_4hr_local <- as.POSIXlt(time_4hr_UTC, tz= "America/Chicago")


Kp_1hr <- data[,9]
Kp_4hr <- data[,14]

difference_1 <- round(difftime(time_1hr_UTC,time_now_UTC),digits = 2)
difference_4 <- round(difftime(time_4hr_UTC,time_now_UTC),digits = 2)


## Provides cloud cover data
csv_weather <- getURL("https://www.aviationweather.gov/metar/data?ids=KMSP&format=raw&hours=0&taf=off&layout=off&date=0",ssl.verifypeer=0L, followlocation=1L)
temp <- read.csv(text=csv_weather)
temp <- as.character(temp[43,])
overcast <- as.character(str_extract(temp,"OVC\\d\\d\\d"))
broken_clouds <- as.character(str_extract(temp,"BKN\\d\\d\\d"))
overcast_logic <- grepl("OVC\\d\\d\\d",temp)
broken_clouds_logic <- grepl("BKN\\d\\d\\d",temp)
cloud_message <- paste(" -- Cloud Conditions -- \n","\nOvercast: ",overcast_logic,"\nBroken: ",broken_clouds_logic,sep="")


## Determines if it's dark outside(TRUE if dark, FALSE if light)

## Retrieve Sunrise-Sunset Tables
sunrise_table <- import("C://Users//Keiran//Documents//sunrise_table.csv")
sunset_table <- import("C://Users//Keiran//Documents//sunset_table.csv")

sunset <- paste(as.character(Sys.Date())," 16:00:00",sep="")
sunrise <- paste(as.character(Sys.Date())," 06:00:00",sep="")

time_logic_1 <- time_1hr_local>sunset|time_1hr_local<sunrise
time_logic_4 <- time_4hr_local>sunset|time_4hr_local<sunrise


## Kp threshold logic
Kp_4hr_logic <- Kp_4hr>=5
Kp_1hr_logic <- Kp_1hr>=5

## Total logic (Kp thresh + "is it dark outside")
total_1hr_logic <- (time_logic_1==TRUE)&(Kp_1hr_logic==TRUE)
total_4hr_logic <- (time_logic_4==TRUE)&(Kp_4hr_logic==TRUE)


## Conditional stuff
message4 <- ""
message1 <- ""
message_link <- "More information at: http://www.swpc.noaa.gov/products/wing-kp"

if(total_4hr_logic==TRUE){
  message4 <- paste("Kp value of ",Kp_4hr," expected in ",as.numeric(difference_4)," ",units(difference_4)," at ",time_4hr_local," CST",sep="")
}

if(total_1hr_logic==TRUE){
  message1 <- paste("Kp value of ",Kp_1hr," expected in ",as.numeric(difference_1)," ",units(difference_1)," at ",time_1hr_local," CST",sep="")
}
  
  
if(total_4hr_logic==TRUE|total_1hr_logic==TRUE){
  
  ##Email stuff
  from <- "<keirancantilina@gmail.com>"
  to <- "<keirancantilina@gmail.com>"
  subject <- "Aurora Notification"
  body <- c(message1,"\n",message4,"\n",cloud_message,"\n",message_link)
  sendmail_options(smtpServer = "aspmx.l.google.com", smtpPort = 25)
  sendmail(from, to, subject, body, control=list(sendmail_options))
}
  
