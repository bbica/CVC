# Install and load necessary packages
library(tidyverse)
library(caret)
library(randomForest)
#install.packages("randomForest")
# Generate a synthetic dataset with enhanced features
set.seed(123)
n <- 1000
variant_ids <- paste0("rs",round(rnorm(n = n, mean = 1110233)))
data <- tibble(
  variant_id = variant_ids,
  allele_frequency = runif(n, 0, 1),
  CADD = sample(round(rnorm(n = n, mean = 10))),
  conservation_score = runif(n, 0, 1),
  functional_annotation = sample(c("intron", "exon", "promoter", "UTR"), n, replace = TRUE),
  predicted_impact = runif(n, 0, 1),
  disease_related = sample(c(0, 1), n, replace = TRUE)
)

# Convert functional_annotation to factor
data$functional_annotation <- as.factor(data$functional_annotation)
data$disease_related <- as.factor(data$disease_related)
#Save .CSV
write.csv(data, "data_training.csv")
saveRDS(data, "data_training.RDS")
# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$disease_related, p = .8,
                                  list = FALSE,
                                  times = 1)
train_data <- data[trainIndex,]
test_data  <- data[-trainIndex,]

# Train random forest model
set.seed(123)
model <- randomForest(disease_related ~ allele_frequency + conservation_score + functional_annotation + predicted_impact,
                      data = train_data,
                      ntree = 1000,
                      mtry = 3,
                      importance = TRUE)

# Make predictions
predictions <- predict(model, newdata = test_data)

# Confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$disease_related)
print(conf_matrix)

# Model accuracy
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 2)))

# Variable importance
importance <- importance(model)
varImpPlot(model)

# Example of new data
new_data <- tibble(
  variant_id = paste0("rs", c(1001, 1002)),
  allele_frequency = c(0.2, 0.9),
  CADD = c(20,3 ),
  conservation_score = c(0.8, 0.3),
  functional_annotation = factor(c("exon", "intron"), levels = levels(data$functional_annotation)),
  predicted_impact = c(0.9, 0.4)

)

# Make predictions
new_predictions <- predict(model, new_data)

# Add predictions to new_data
new_data <- new_data %>% mutate(disease_related = new_predictions)

# Display the results
print(new_data)


saveRDS(model, "model.RDS" )
write.csv(new_data[,c(1:6)], "new_data.csv",row.names = FALSE  )







