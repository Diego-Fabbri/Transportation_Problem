#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Transportation Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set supply
S <- c(150, 20, 130)

#Set demands
d <- c(135, 75, 45, 45)

#Set costs
cost <- matrix(c(5, 2, 3, 9,
                 7, 1, 12, 4,
                 8, 15, 19, 2), nrow = 3, byrow = TRUE)


#Set number of suppliers
m <- nrow(cost)

#Set number of demand centers
n <- ncol(cost)

#Feasibility Condition
total_demand <- sum(d)
total_supply <- sum(S)

if(total_demand <= total_supply) {
  condition <- TRUE
} else{
  condition <- FALSE
}

#Build model

if(condition == TRUE){
  
Model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:m , j = 1:n, type = "integer", lb = 0) %>% #define variables
  set_objective(sum_expr(x[i, j]*cost[i, j], i = 1:m , j = 1:n), "min") %>%   #define objective function
  add_constraint(sum_expr(x[i, j], j = 1:n) <= S[i], i = 1:m) %>% #define constraints
  add_constraint(sum_expr(x[i, j], i = 1:m) == d[j] , j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary

##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

## X variables
for (r in 1:m) {
  print(paste("From Supply center",r))
  for (c in 1:n) {
    tmp <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == r , j == c) %>%
      select(value)
    
    if (tmp != 0) {
      print(paste("---->to demand center",c,"flow",tmp,"and reated cost is",cost[r,c]*tmp))
    }
  }
}
} else {print(paste("Feasibility condition does not hold"))}

