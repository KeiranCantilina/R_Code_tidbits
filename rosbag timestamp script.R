library(tidyr)
library(readr)


path <- "R:\\BME\\SurgicalRobotics\\Personnel\\Keiran Cantilina\\Rosbags\\clutch_record_TEST\\_slash_dvrk_slash_footpedals_slash_clutch.csv"

clutch_file <- read.csv(path, header=TRUE, sep=",")
clutch_file$FormattedTime <- formatC(clutch_file$secs+ (clutch_file$nsecs/1000000000),digits = 21, format = "f")

clutch_file$buttons <- parse_number(as.character(clutch_file$buttons))

timestamps <- c()
first_time_run <- 1;
for (i in 1:length(clutch_file$buttons)){
  if(clutch_file$buttons[i] == 1 && first_time_run == 1){
    timestamps <- append(timestamps, clutch_file$FormattedTime[i])
    first_time_run <- 0
  }
  else if(first_time_run != 1){
    timestamps <- append(timestamps, clutch_file$FormattedTime[i])
  }
}

first_time_run <- 1
timestamp_type <- 1
string_contents <- ""
string_beginning <- "rosbag filter clutch_record_TEST_PSM.bag clutch_record_CROPPED.bag \""
for (i in 1:length(timestamps)){
  if(first_time_run == 1){
    string_contents <- paste(string_contents,"((t.to_sec() > 0) && (t.to_sec() < ",timestamps[1],"))", sep="")
    timestamp_type <- 0
    first_time_run <- 0
  }
  else if(timestamp_type == 0){
    string_contents <- paste(string_contents,"||((t.to_sec() > ",timestamps[i],")",sep="")
    timestamp_type <- 1
  }
  else{
    string_contents <- paste(string_contents, " && (t.to_sec() < ", timestamps[i],"))",sep="")
    timestamp_type <- 0
  }
}
string_contents <- paste(string_contents, ")", sep="")
string_contents <- paste(string_beginning, string_contents, "\"", sep="" )

## return(string_contents)


