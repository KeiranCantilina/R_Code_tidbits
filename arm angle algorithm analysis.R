library(progress)

root <- "D:/VisualStudio Projects/EigenTest/ROBOTER/"
output_matrix <- read.csv(paste(root, "output_matrix.csv",sep=""))
output_array <- read.table(paste(root, "output_array.txt",sep=""),sep="\n")
problem_positions <- data.frame()
pb <- progress_bar$new(
  format = " calculating [:bar] :percent eta: :eta",
  total = 78124, clear = FALSE, width = 60)
index <- 1;
for (i in 1:78124){
  pb$tick()
  if(output_array[i,1]==1){
    problem_positions[index,1:7] <- output_matrix[i,1:7]
    index <- index + 1
  }
  
}

# Calculate status
for (i in 26307){
  problem_positions$Status[i] <- (problem_positions[i,2] < 0) + 2 * (problem_positions[i,4] < 0) + 4 * (problem_positions[i,6] < 0)
}

hist(problem_positions$Status, breaks = c(-0.5,0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5))