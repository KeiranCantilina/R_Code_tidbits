## Script for scraping web data when dropdown menus are involved

##This script specifically withdraws data from NOAA Western Lake Erie Station #8 at 
## https://www.glerl.noaa.gov/res/HABs_and_Hypoxia/rtMonSQL.php for 2014-2016 summers

library(rvest)
library(RCurl)
library(rio)

##Working directory
dir <- "C:/Users/Keiran/Desktop/Ren/"

#Empty Dataframes
wind_speed_data <- data.frame(0,0)
colnames(wind_speed_data) <- c("V1","V2")

wind_direction_data <- data.frame(0,0)
colnames(wind_direction_data) <- c("V1","V2")

phosphorus_data <- data.frame(0,0)
colnames(phosphorus_data) <- c("V1","V2")

chlorophyll_a_data <- data.frame(0,0)
colnames(chlorophyll_a_data) <- c("V1","V2")

phycocyanin_data <- data.frame(0,0)
colnames(phycocyanin_data) <- c("V1","V2")


## Create vector with dates
year_vector <- c("2014","2015","2016")
month_vector <- c("06","07","08","09","10")
day_vector <- "30"
date_vector <- c()

for(i in 1:3){
  yr <- year_vector[i]
  for(j in 1:5){
    mo <- month_vector[j]
    date_vector <- c(date_vector, paste(yr,mo,day_vector, sep="-"))
  }
}

## Remove missing month in 2014
date_vector <- date_vector[2:27]
  
  
  
##Iterate through date vector and download all the raw html
for(k in 1:12){
  
  session <- html_session("https://www.glerl.noaa.gov/res/HABs_and_Hypoxia/rtMonSQL.php")
  page <- read_html(session)
  form <- html_form(page)[[2]]
  form$fields$centerDate$value <- date_vector[k]
  form$fields$pmDays$value <- 15
  
  ##Write html to file as buffer
  output <- submit_form(session, form)
  output_html <- read_html(output)
  write_html(output_html, "C:/Users/Keiran/Desktop/test.html")
  con <- file("C:/Users/Keiran/Desktop/test.html")
  
  ## Read html lines
  line <- readLines(con, n=250)
  data <- line[177:187]
  
  wind_speed_raw <- data[4]
  wind_direction_raw <- data[5]
  phosphorus_raw <- data[8]
  chlorophyll_a_raw <- data[9]
  phycocyanin_raw <- data[10]
  
  ## Get rid of leading tabs and stuff
  wind_speed_raw <- gsub("^.*?: ","",wind_speed_raw)
  wind_direction_raw <- gsub("^.*?: ","",wind_direction_raw)
  phosphorus_raw <- gsub("^.*?: ","",phosphorus_raw)
  chlorophyll_a_raw <- gsub("^.*?: ","",chlorophyll_a_raw)
  phycocyanin_raw <- gsub("^.*?: ","",phycocyanin_raw)
  
  ##wind speed processing
  test <- gsub("\\],\\[","|",wind_speed_raw)
  test <- gsub("\\[", "", test)
  test <- gsub("\\]", "", test)
  df <- t(read.table(textConnection(test), header = FALSE, sep="|" ))
  df2 <- read.table(textConnection(df), header=FALSE, sep=",")
  wind_speed_data <- rbind(wind_speed_data, df2)
  
  ##wind direction processing
  test <- gsub("\\],\\[","|",wind_direction_raw)
  test <- gsub("\\[", "", test)
  test <- gsub("\\]", "", test)
  df <- t(read.table(textConnection(test), header = FALSE, sep="|" ))
  df2 <- read.table(textConnection(df), header=FALSE, sep=",")
  wind_direction_data <- rbind(wind_direction_data, df2)
  
  ##Phosphorus processing
  test <- gsub("\\],\\[","|",phosphorus_raw)
  test <- gsub("\\[", "", test)
  test <- gsub("\\]", "", test)
  df <- t(read.table(textConnection(test), header = FALSE, sep="|" ))
  df2 <- read.table(textConnection(df), header=FALSE, sep=",")
  phosphorus_data <- rbind(phosphorus_data, df2)
  
  ##Chlorophyll a processing
  test <- gsub("\\],\\[","|",chlorophyll_a_raw)
  test <- gsub("\\[", "", test)
  test <- gsub("\\]", "", test)
  df <- t(read.table(textConnection(test), header = FALSE, sep="|" ))
  df2 <- read.table(textConnection(df), header=FALSE, sep=",")
  chlorophyll_a_data <- rbind(chlorophyll_a_data, df2)
  
  ##Phytocyanin processing
  test <- gsub("\\],\\[","|",phycocyanin_raw)
  test <- gsub("\\[", "", test)
  test <- gsub("\\]", "", test)
  df <- t(read.table(textConnection(test), header = FALSE, sep="|" ))
  df2 <- read.table(textConnection(df), header=FALSE, sep=",")
  phycocyanin_data <- rbind(phycocyanin_data, df2)
  
}


write.csv(na.omit(wind_speed_data), file=paste(dir,"Wind_speed.csv", sep=""))
write.csv(na.omit(wind_direction_data), file=paste(dir,"Wind_direction.csv", sep=""))
write.csv(na.omit(phosphorus_data), file=paste(dir,"phosphorus.csv", sep=""))
write.csv(na.omit(chlorophyll_a_data), file=paste(dir,"chlorophyll_a.csv", sep=""))
write.csv(na.omit(phycocyanin_data), file=paste(dir,"phycocyanin.csv", sep=""))




