options(digits.secs = 6)

old_vel <- 0
new_vel <- 0.000008

vel_gap <- new_vel-old_vel
max_accel <- 10
fri_packet_interval <- 0.002
start_time <- Sys.time()
gap_time <- abs(vel_gap)/max_accel
adjusted_gap_time <- (pi/2)*gap_time
intervals <- ceiling(adjusted_gap_time/fri_packet_interval)

end_time <- Sys.time()

path_vector <- c(NA)


sign = sign(vel_gap)




for (i in 1:intervals){
  path_vector[i] <- -(vel_gap/2)*cos((pi)*(i/intervals))+(0.5*vel_gap)+old_vel
}
  


plot(path_vector)
duration <- end_time - start_time


## Rederiving gap time adjustment constant

# intervals <- ceiling(gap_time/fri_packet_interval)
# path_vector <- c()
# 
# for (i in 0:intervals){
#   path_vector[i] <- -(vel_gap/2)*cos((pi)*(i/intervals))+(0.5*vel_gap)+old_vel
# }
# 
# plot(path_vector)
# 
# ((path_vector[length(path_vector)/2]-path_vector[(length(path_vector)/2)-1])/fri_packet_interval)/max_accel


## Logistic Function S-curve
# path_vector2 <- c()
# for (i in 1:intervals){
#   path_vector2[i] <- new_vel/(1+exp(-0.7*(i-intervals/2)))
# }
# 
# plot(path_vector2)





