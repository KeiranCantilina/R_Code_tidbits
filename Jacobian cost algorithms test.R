joint_limit <- 165
f <- 1

## cost <- exp(abs(current_joint)-joint_limit)*f
cost <- c()

for (i in 1:170){
  cost[i] <- exp(i-joint_limit)*f
}

cost_vel <- c()
for (i in 80:105){
  cost_vel[i-79] <- 0.00006*exp(8.6*0.1*i)

}


plot(cost_vel)
