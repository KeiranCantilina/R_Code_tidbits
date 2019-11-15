old_vel <- 10
new_vel <- -10

vel_gap <- new_vel-old_vel
max_accel <- 800
fri_packet_interval <- 0.002

gap_time <- abs(vel_gap)/max_accel
adjusted_gap_time <- (pi/2)*gap_time
intervals <- ceiling(adjusted_gap_time/fri_packet_interval)
path_vector <- c()

sign = sign(vel_gap)

for (i in 0:intervals){
  path_vector[i] <- -(vel_gap/2)*cos((pi)*(i/intervals))+(0.5*vel_gap)+old_vel
}
  


plot(path_vector)



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











