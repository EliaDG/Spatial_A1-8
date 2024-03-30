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
property_price_prediction <- function(dependent_variable, covariates) {
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
result <- property_price_prediction(dependent_variable, covariates)
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

# B and C have the most links directed towards them while E has none directed towards it. Hence, B and C are the most central "buyers", E the least.
# A and C have the most links directed towards others while B has none directed towards others. Hence A and C are the most central "suppliers" and B the least.



##### Exercise B.2.1: How would centralities change if you considered a row-normalized network instead?
# Compute row sums for normalization
adj_matrix <- as.data.frame(adj_matrix)
row_sums <- rowSums(adj_matrix, dims = 1)

# Normalize the adjacency matrix
w <- adj_matrix / row_sums

w[is.na(w)] <- 0

print(w)

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

degree_table_norm

# As expected based on the slides, the out-degree of the agents are equalized to 1.
# Also, the most central buyers are now B and D instead of B and C. Clearly the row-normalization leads to a distortion.
# is it possible to have a degree > 1?

##### Exercise B.2.2: How would the network change if you removed or added a specific agent?

# Define the edges
edges <- c("A", "B", "A", "C", "A", "D", "C", "B", "D", "A", "D", "B", "D", "C", "E", "D")

# Create the graph
g1 <- graph(edges, directed = TRUE)

plot(g1, edge.arrow.size = 0.5, vertex.label.cex = 1.5, vertex.size = 30)

# Get the adjacency matrix
adj_matrix <- as_adjacency_matrix(g1)

# Convert the row names and column names to node labels
rownames(adj_matrix) <- V(g1)$name
colnames(adj_matrix) <- V(g1)$name

# Print the adjacency matrix
print(adj_matrix)

rec_g1 <- reciprocity(g1)
trans_g1 <- transitivity(g1)

rec <- cbind(rec_g,rec_g1)
trans <- cbind(trans_g, trans_g1)

rectrans_table <- rbind(rec, trans)

colnames(rectrans_table) <- c("Network Complete","Network Removed")
rownames(rectrans_table) <- c("Reciprocity", "Transivity")

rectrans_table

id_g1 <- cbind(1,3,3,2,0,1)
od_g1 <- cbind(3,0,3,1,2,1)

degree_table_g1 <- rbind(id, od)

colnames(degree_table_g1) <- c("A","B","C","D","E","F")
rownames(degree_table_g1) <- c("In", "Out")

rec_g <- reciprocity(g)
trans_g <- transitivity(g)

degree_table_g1

##### continue here with analysis from above and how reciprocity changes


### Exercise B.3
# Step 1: Generate agent characteristics from a standard normal distribution
num_agents <- 6  # Number of agents
agent_characteristics <- rnorm(num_agents)  # Simulate agent characteristics from a standard normal distribution

# Step 3: Define a linear-in-means model
# Let's assume the response variable depends on the agent characteristics and network structure
# Here, we'll use a simple linear model where the response variable is a linear combination of agent characteristics and network structure
# For simplicity, let's assume the network structure affects the response as an additional additive term

w


resp1 <- 0.5 * agent_characteristics + 0.2 * degree(g)
lm_model_1 <- lm(resp1 ~ agent_characteristics + degree(g))

lm_model_1


resp2 <- 0.2*agent_characteristics + 0.5*degree(g)
lm_model_2 <- lm(resp2 ~ agent_characteristics + degree(g))

lm_model_2

resp3 <- 0.7 * agent_characteristics + 0.4 * degree(g)
lm_model_3 <- lm(resp3 ~ agent_characteristics + degree(g))

lm_model_3


resp4 <- 0.9*agent_characteristics + 0.38*degree(g)
lm_model_4 <- lm(resp4 ~ agent_characteristics + degree(g))

lm_model_4





###
# Compute centrality measures
degree_centralities <- degree(g)
betweenness_centralities <- betweenness(g)
closeness_centralities <- closeness(g)

# Combine centrality measures into a data frame
centrality_data <- data.frame(
  "Node" = V(g)$name,
  "Degree Centrality" = degree_centralities,
  "Betweenness Centrality" = betweenness_centralities,
  "Closeness Centrality" = closeness_centralities
)

# Print the centrality data
print(centrality_data)

#Interpretation

# Degree Centrality:
  # - Node A and Node B have a degree centrality of 3, indicating that they are connected to 3 other nodes in the network.
  # - Node C has a degree centrality of 4, indicating that it is connected to 4 other nodes.
  # - Node D has the highest degree centrality of 5, indicating that it is connected to 5 other nodes.
  # - Node E has a degree centrality of 3, similar to Nodes A and B.
  # - Node F has the lowest degree centrality of 2, indicating that it is connected to only 2 other nodes.

# Betweenness Centrality:
  # - Nodes A, B, and F have a betweenness centrality of 0, indicating that they lie on no shortest paths between other nodes.
  # - Node C has a betweenness centrality of 1.0, suggesting that it lies on all shortest paths between other nodes in the network.
  # - Node D has the highest betweenness centrality of 3.5, indicating that it lies on many shortest paths between other nodes.
  # - Node E has a betweenness centrality of 0.5, indicating that it lies on fewer shortest paths compared to Node D but more than Nodes A, B, and F.

# Closeness Centrality:
  # - Nodes A, B, and F have the lowest closeness centrality values, indicating that they are relatively far from other nodes in terms of geodesic distance.
  # - Node C has a higher closeness centrality compared to Nodes A, B, and F, but lower compared to Nodes D and E.
  # - Node D has the highest closeness centrality, suggesting that it is relatively close to other nodes in the network.
  # - Node E has a closeness centrality similar to Nodes A, B, and F, but lower than Node D.

save.image("Workspace_AB.RData")
