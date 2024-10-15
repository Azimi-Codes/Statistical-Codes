attach(GSI_Data_set)

library(dplyr)

# Normalization function
normalize <- function(x) {
  if (max(x) != min(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(rep(0, length(x))) # Handle case when all values are the same
  }
}

# Normalize each variable in the dataset
normalized_data <- GSI_Data_set %>%
  select(RE, ET, PT, NT, RT, AT, TC) %>%
  mutate(across(everything(), normalize))


# Calculate entropy for each indicator
calculate_entropy <- function(normalized_vector) {
  n <- length(normalized_vector)
  proportions <- normalized_vector / sum(normalized_vector)
  proportions <- proportions[proportions > 0] # Remove zero values to avoid log(0)

entropy <- - (1 / log(n)) * sum(proportions * log(proportions))
  return(entropy)
}


entropy_values <- sapply(normalized_data, calculate_entropy)

weights <- (1 - entropy_values) / sum(1 - entropy_values)

GSI <- rowSums(normalized_data * weights)

head(GSI_Data_set)  
write.csv(GSI_Data_set, "D:/USQ docs/Scanned Documents/Papers/DEA Paper/GSI_Data_Set_with_GSI.csv", row.names = FALSE)  
