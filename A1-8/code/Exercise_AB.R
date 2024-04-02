# DEPENDENCIES
getwd()

# TO DO ------------------------------------------------------------------------

# HEADER -----------------------------------------------------------------------

# Assignment 1

# SOURCING --------------------------------------------------------------------- 

source("./code/__packages.R")

# Exercise A

# Load the Boston dataset
data(Boston)

# Selecting a subset of covariates for the model
covariates <- Boston[, 1:5]

# Selecting the dependent variable
# property prices
dependent_variable <- Boston$medv

# Create the function for property price prediction
pp_pred <- function(dependent_variable, covariates) {
  # Combine the dependent variable and covariates into a data frame
  data <- data.frame(dependent_variable, covariates)
  
  # Perform Ordinary Least Squares (OLS) regression
  model <- lm(dependent_variable ~ ., data)
  
  # Extract coefficients
  coefficients <- coef(model)
  
  # Extract standard errors of coefficients
  se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate t-values
  t_values <- coefficients / se
  
  # Calculate p-values
  p_values <- 2 * pt(abs(t_values), df = df.residual(model), lower.tail = FALSE)
  
  # Calculate 95% confidence intervals
  conf_int <- confint.default(model)
  
  # Store results in a data frame
  results <- data.frame(
    "Coefficients" = coefficients,
    "Standard Errors" = se,
    "t-values" = t_values,
    "p-values" = p_values,
    "Confidence Intervals (95%)" = conf_int
  )
  
  return(results)
  
}

# Call the property_price_prediction function
result <- pp_pred(dependent_variable, covariates)
print(result)

#---------------------------------------------------------------------------------------------------------------------

# Exercise B.1

# Define the edges
edges <- c("A", "B", "A", "C", "A", "D", "C", "B", "D", "A", "D", "B", "D", "C", "E", "D", "E", "F", "F", "D")

# Create the graph
g <- graph(edges, directed = TRUE)

plot(g, edge.arrow.size = 0.5, vertex.label.cex = 1.5, vertex.size = 30)

# Get the adjacency matrix
adj_matrix <- as_adjacency_matrix(g)

# Convert the row names and column names to node labels
rownames(adj_matrix) <- V(g)$name
colnames(adj_matrix) <- V(g)$name

# Print the adjacency matrix
print(adj_matrix)

# Real world example of supply chain network where there is rarely any reciprocity: Construction of Airplanes. 
# Main vertices is the firm constructing the airplane (B), where all manufactured parts end up (high in-degree, low out-degree). 
# On the other hand, among manufactures of airplane parts little reciprocity might occur as they need specialized part to 
# finish their own parts (D and A).
reciprocity(g)

#---------------------------------------------------------------------------------------------------------------------

# Exercise B.2
# Degree of agents
id <- cbind(1,3,2,3,0,1)
od <- cbind(3,0,1,3,2,1)

degree_table <- rbind(id, od)

colnames(degree_table) <- c("A","B","C","D","E","F")
rownames(degree_table) <- c("In", "Out")

rec_g <- reciprocity(g)
trans_g <- transitivity(g)

degree_table

# B and D have the most links directed towards them while E has none directed towards it. Hence, B and D are the most central "buyers", E the least.
# A and D have the most links directed towards others while B has none directed towards others. Hence A and D are the most central "suppliers" and B the least.

# Eigen-centrality: the measure depicts an agent’s centrality within a network
# proportional to the sum of her links to other agents, weighted by their own centrality.
ec <- eigen_centrality(g) %>% `$`(vector)
ec

# D is the most central vertices, while E and F are the least central ones.

##### Exercise B.2.1: How would centrality change if you considered a row-normalized network instead?
# Compute row sums for normalization
#adj_matrix <- as.data.frame(as.matrix(adj_matrix)) # if not working use: as.matrix()
row_sums <- rowSums(adj_matrix, dims = 1)

# Normalize the adjacency matrix
w <- adj_matrix / row_sums

w[is.na(w)] <- 0

print(round(w,2))

# Degree of agents
# Define a function to calculate the sum of a specified column
sum_column <- function(matrix_data, column_name) {
  # Find the index of the specified column name
  column_index <- which(colnames(matrix_data) == column_name)
  
  # Check if the column exists
  if (length(column_index) == 0) {
    stop("Column not found in the matrix.")
  }
  
  # Calculate the sum of the specified column
  column_sum <- sum(matrix_data[, column_index])
  
  return(column_sum)
}
# Define a function to calculate the sum of a specified row
sum_row <- function(matrix_data, row_name) {
  # Find the index of the specified row name
  row_index <- which(rownames(matrix_data) == row_name)
  
  # Check if the row exists
  if (length(row_index) == 0) {
    stop("Row not found in the matrix.")
  }
  
  # Calculate the sum of the specified row
  row_sum <- sum(matrix_data[row_index, ])
  
  return(row_sum)
}


id <- cbind(sum_column(w, "A"),sum_column(w, "B"),sum_column(w, "C"),sum_column(w, "D"),sum_column(w, "E"),sum_column(w, "F"))
od <- cbind(sum_row(w, "A"),sum_row(w, "B"),sum_row(w, "C"),sum_row(w, "D"),sum_row(w, "E"),sum_row(w, "F"))

degree_table_norm <- rbind(id, od)

colnames(degree_table_norm) <- c("A","B","C","D","E","F")
rownames(degree_table_norm) <- c("In", "Out")

round(degree_table_norm, 2)

# As expected based on the slides, the out-degree of the agents are equalized to 1.
# Also, the most central buyers are still B and D.



##### Exercise B.2.2: How would the network change if you removed or added a specific agent?

# Define the edges
edges <- c("A", "B", "A", "C", "A", "D", "C", "B", "D", "A", "D", "B", "D", "C", "E", "D")

# Create the graph
g1 <- graph(edges, directed = TRUE)

plot(g1, edge.arrow.size = 0.5, vertex.label.cex = 1.5, vertex.size = 30)

# Get the adjacency matrix
adj_matrix1 <- as_adjacency_matrix(g1)

# Convert the row names and column names to node labels
rownames(adj_matrix1) <- V(g1)$name
colnames(adj_matrix1) <- V(g1)$name

# Print the adjacency matrix
print(adj_matrix1)

id_g1 <- cbind(1,3,2,2,0)
od_g1 <- cbind(3,0,1,3,1)

degree_table_g1 <- rbind(id_g1, od_g1)

colnames(degree_table_g1) <- c("A","B","C","D","E")
rownames(degree_table_g1) <- c("In", "Out")

degree_table_g1

rec_g <- reciprocity(g)
trans_g <- transitivity(g)

rec_g1 <- reciprocity(g1)
trans_g1 <- transitivity(g1)

rec <- cbind(rec_g,rec_g1)
trans <- cbind(trans_g, trans_g1)

rectrans_table <- rbind(rec, trans)

colnames(rectrans_table) <- c("Network Complete","Network Removed")
rownames(rectrans_table) <- c("Reciprocity", "Transivity")

round(rectrans_table,2)

##### continue here with analysis from above and how reciprocity changes


### Exercise B.3
# For our real world example we choose level of experience as the agent characteristic and
# number of defects as the response variable. The coefficient beta we choose to be -0.8 indicating
# that more experience leads to less defects. We assign some values to the other parameters gamma, lamda
# and sigma^2 as indicated in the code.

# Assigning values for simulation:
# For replicability
set.seed(1234)

# Parameters
lambda <- 0.4  # Parameter for the network effect
beta <- -1     # Coefficient for the direct effect of X
theta <- 0.5   # Coefficient for the network-lagged effect of X
gamma <- 0.8

reps <- 1000
estimate <- vector("numeric", reps)

for (i in 1:reps) {
  x <- rnorm(6)
  W <- adj_matrix
  errs <- rnorm(N, 0, 1)
  Wx <- W %*% x %*% theta
  S = diag(N) - lambda * W
  y = solve(S, Wx * gamma + x * beta + errs)
  x <- as.matrix(x)
  y <- as.matrix(y)
  model_1 <- lm(y ~ x)
  estimate[i] <- coef(model_1)["x"]
}

estimate_mean <- mean(estimate)
print(estimate_mean)

# The liner-in-means model estimate depicts a larger negative relationship between level of experience
# and number of defects than we initially set (-0.8). Hence, there is a bias present.


save.image("Workspace_AB.RData")
