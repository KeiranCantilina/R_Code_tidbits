library(knitr)
library(dplyr)
library(ggplot2)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(tidyr)


points <- read.csv("D://AutoCAD//Simple Circle.csv", header = TRUE,sep = ",")

n <- length(points$ID)
ggplot(points, aes(points$Position.X, points$Position.Y))+geom_point()

distance <- as.matrix(stats::dist(select(points, Position.X, Position.Y), diag = TRUE, upper = TRUE))
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

model <- MIPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1) %>%
  
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  
  # minimize travel distance
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%
  
  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%
  
  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  #
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
model

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0) 

paths <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(trip_id = row_number()) %>% 
  gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(points, by = c("idx_val" = "ID"))

ggplot(points, aes(Position.X, Position.Y)) + 
  geom_point() + 
  geom_line(data = paths, aes(group = trip_id)) + 
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))

write.csv(paths, file = "D://Path_output.csv")

