# Load necessary libraries
library(dplyr)

#Reading the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)


# Check for missing values in the dataset
missing_values <- colSums(is.na(data))
print(missing_values)


# Identify unique values in the "Home Status" column
unique(data$Home.Status)
# Clean the "Home Status" column by replacing corrupted values
data$Home.Status <- gsub("\\?+", "T", data$Home.Status)  
data$Home.Status <- gsub("^MORTAGE$", "MORTGAGE", data$Home.Status)
# Verify the cleaned "Home Status" values
unique(data$Home.Status)


# Calculate the percentage of missing values for each column
missing_percent <- colSums(is.na(data)) / nrow(data) * 100
# Convert to a dataframe for better readability
missing_df <- data.frame(Column = names(missing_percent), Missing_Percentage = missing_percent)
# Print the missing values percentage
print(missing_df)


# Sort by highest missing percentage
missing_df <- missing_df[order(-missing_df$Missing_Percentage), ]
# Display columns with missing values (greater than 0%)
missing_df <- missing_df[missing_df$Missing_Percentage > 0, ]
print(missing_df)


# Recalculate percentage of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
# Convert to a data frame for better readability
missing_df <- data.frame(Column = names(missing_percentage), 
                         Missing_Percentage = missing_percentage)
# Filter only columns with missing values
missing_df <- missing_df[missing_df$Missing_Percentage > 0, ]
# Print the updated missing values percentage
print(missing_df)


# Drop rows where Postal Code is missing
data <- data[!is.na(data$Postal.Code), ]

# Drop rows where Designation is missing
data <- data[!is.na(data$Designation), ]


# Recalculate percentage of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
# Convert to a data frame for better readability
missing_df <- data.frame(Column = names(missing_percentage), 
                         Missing_Percentage = missing_percentage)
# Filter only columns with missing values
missing_df <- missing_df[missing_df$Missing_Percentage > 0, ]
# Print the updated missing values percentage
print(missing_df)


# Define the mapping
experience_map <- c("<1yr" = 1, "1yrs" = 2, "2yrs" = 3, "3yrs" = 4, "4yrs" = 5,
                    "5yrs" = 6, "6yrs" = 7, "7yrs" = 8, "8yrs" = 9, "9yrs" = 10,
                    ">10yrs" = 11)
data$Experience <- experience_map[data$Experience]
# Convert to numeric for analysis
data$Experience <- as.numeric(data$Experience)
# Verify encoding
table(data$Experience)
head(data)


# Load necessary libraries
library(ggplot2)
# Select a numeric column to visualize (e.g., Yearly.Income)
ggplot(data, aes(x = Yearly.Income)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1.2) +  # Add density curve
  ggtitle("Distribution of Yearly Income") +
  xlab("Yearly Income") +
  ylab("Density") +
  theme_minimal()


# Load necessary libraries
library(moments)   # For skewness calculation
# Select only numeric columns
numeric_cols <- data[, sapply(data, is.numeric)]
# Calculate skewness for each numeric column
skewness_values <- sapply(numeric_cols, skewness, na.rm = TRUE)
# Display skewness values
print(skewness_values)


# KNN Imputation 
library(cluster)  
# Select relevant numeric features and remove rows with NA values
data_clean <- na.omit(data[, c("Experience", "Debt.to.Income", "Lend.Amount", "Present.Balance")])
# Check if data_clean is empty after removing NA values
if (nrow(data_clean) == 0) {
  stop("No complete rows available for clustering. Check missing values.")
} else {
  # Perform K-means clustering
  set.seed(42)
  kmeans_result <- kmeans(data_clean, centers = 3)
  # Add cluster labels back to the original dataset
  data$Cluster <- NA  
  data$Cluster[match(rownames(data_clean), rownames(data))] <- kmeans_result$cluster
  # Check median Yearly.Income per cluster
  cluster_income <- aggregate(Yearly.Income ~ Cluster, data = data, FUN = median, na.rm = TRUE)
  # Print the result
  print(cluster_income)
}
# Step 1: Calculate median Yearly.Income per cluster
median_income <- aggregate(Yearly.Income ~ Cluster, data = data, FUN = median, na.rm = TRUE)
# Step 2: Create a mapping of Cluster ??? Median Income
median_income_map <- setNames(median_income$Yearly.Income, median_income$Cluster)
# Step 3: Impute missing Yearly.Income based on cluster median
data$Yearly.Income[is.na(data$Yearly.Income)] <- median_income_map[data$Cluster[is.na(data$Yearly.Income)]]


# Assign missing clusters using KNN (or nearest available cluster based on features)
library(DMwR)
# Perform KNN imputation on Cluster column
data$Cluster <- knnImputation(data[, c("Experience", "Debt.to.Income", "Lend.Amount", "Present.Balance", "Cluster")])$Cluster
# Recalculate median income per cluster after fixing missing clusters
median_income <- aggregate(Yearly.Income ~ Cluster, data = data, FUN = median, na.rm = TRUE)
# Recreate the mapping of Cluster ??? Median Yearly Income
median_income_map <- setNames(median_income$Yearly.Income, median_income$Cluster)
# Impute missing Yearly.Income values using new Cluster assignments
data$Yearly.Income[is.na(data$Yearly.Income)] <- median_income_map[data$Cluster[is.na(data$Yearly.Income)]]

# Check missing values again
colSums(is.na(data))


# Recalculate median income per cluster after KNN imputation
median_income <- aggregate(Yearly.Income ~ Cluster, data = data, FUN = median, na.rm = TRUE)
# Update Cluster ??? Median Income mapping
median_income_map <- setNames(median_income$Yearly.Income, median_income$Cluster)
# Use ifelse() to avoid mismatches
data$Yearly.Income <- ifelse(is.na(data$Yearly.Income) & !is.na(data$Cluster), 
                             median_income_map[as.character(data$Cluster)], 
                             data$Yearly.Income)
# Check missing values again
colSums(is.na(data))
# Drop the 'Cluster' column
data <- subset(data, select = -Cluster)
# Verify that the column is removed
head(data)


# Remove rows with missing 'Total.Unpaid.CL' or 'Unpaid.Amount'
data <- data %>% filter(!is.na(Total.Unpaid.CL) & !is.na(Unpaid.Amount))
# Verify missing values after dropping
colSums(is.na(data))

# Calculate the percentage of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
# Convert to a data frame for better readability
missing_df <- data.frame(Column = names(missing_percentage), 
                         Missing_Percentage = missing_percentage)
# Filter only columns with missing values
missing_df <- missing_df[missing_df$Missing_Percentage > 0, ]
# Print the updated missing values percentage
print(missing_df)


# Impute missing Debt.to.Income values
data$Debt.to.Income[is.na(data$Debt.to.Income)] <- 
  (data$Total.Unpaid.CL[is.na(data$Debt.to.Income)] + data$Unpaid.Amount[is.na(data$Debt.to.Income)]) / 
  data$Yearly.Income[is.na(data$Debt.to.Income)]
# Verify missing values again
colSums(is.na(data))
head(data)


library(dplyr)
# Standardize the Validation column
data <- data %>%
  mutate(Validation = case_when(
    Validation == "Vfied" ~ "Verified",
    Validation == "Source Verified" ~ "Verified",
    Validation == "Not Vfied" ~ "Not Verified",
    TRUE ~ Validation
  ))
# Verify changes
table(data$Validation)
head(data)


# Function to map state abbreviations to full names
lookup_state <- function(state_code) {
  state_map <- list(
    "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
    "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
    "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho",
    "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas",
    "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
    "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi",
    "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada",
    "NH" = "New Hampshire", "NJ" = "New Jersey", "NM" = "New Mexico", "NY" = "New York",
    "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma",
    "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
    "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah",
    "VT" = "Vermont", "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia",
    "WI" = "Wisconsin", "WY" = "Wyoming"
  )
  ifelse(state_code %in% names(state_map), state_map[[state_code]], state_code)
}
# Apply function to the 'State' column
data$State <- sapply(data$State, lookup_state)
# Verify changes
table(data$State)


# Function to convert Roman numerals to integers
roman_to_integer <- function(roman) {
  roman_map <- c("I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5, 
                 "VI" = 6, "VII" = 7)
  
  return(roman_map[match(roman, names(roman_map))]) 
}
# Apply the function to GGGrade column
data$GGGrade <- as.numeric(roman_to_integer(data$GGGrade))
# Verify the transformation
print(table(data$GGGrade))  
head(data)


#ONE-HOT ENCODING
library(dummies)
# Apply One-Hot Encoding to 'Duration' column
data <- dummy.data.frame(data, names = "Duration", sep = "_")
# Apply One-Hot Encoding to 'Claim.Type' column
data <- dummy.data.frame(data, names = "Claim.Type", sep = "_")
# Rename the dummy columns properly
colnames(data) <- gsub("Claim.Type_1", "Claim.Type_I", colnames(data))
colnames(data) <- gsub("Claim.Type_2", "Claim.Type_J", colnames(data))
# Apply One-Hot Encoding to 'Home.Status' column
data <- dummy.data.frame(data, names = "Home.Status", sep = "_")
# Print the updated dataframe
head(data)


###############################
write.csv(data, "cleaned_dataset.csv", row.names = FALSE)
# Check the current working directory
getwd()
# Save the file in a specific location (modify the path accordingly)
write.csv(data, "C:/Users/Mohammed.muhaz/Documents/cleaned_dataset4.csv", row.names = FALSE)
###############################

#MODELLING
# Load necessary libraries
library(caret)
library(randomForest)
library(xgboost)
library(rpart)
library(dplyr)
library(ggplot2)

# Drop irrelevant columns
data_mod <- data %>% select(-c(ID, Asst_Reg, Postal.Code,Reason, File.Status))

# Convert categorical variables to factors
data_mod$Loan.No.Loan <- as.factor(data_mod$Loan.No.Loan)
data_mod <- data_mod %>% mutate_if(is.character, as.factor)

# Extract numeric features for PCA
numeric_data <- select_if(data_mod, is.numeric)

# Standardize the data
preProc <- preProcess(numeric_data, method = c("center", "scale"))
scaled_data <- predict(preProc, numeric_data)

# Perform PCA
pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Check variance explained by each component
explained_var <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_var <- cumsum(explained_var)

# Plot Scree plot to determine the number of components to keep
ggplot(data.frame(PC = 1:length(explained_var), Variance = cumulative_var), aes(x = PC, y = Variance)) +
  geom_line() +
  geom_point() +
  ggtitle("Scree Plot: Cumulative Variance Explained") +
  ylab("Cumulative Variance") +
  xlab("Principal Components") +
  theme_minimal()

# Decide number of components to keep (e.g., those explaining 90% variance)
num_components <- min(which(cumulative_var >= 0.90))  # Adjust based on scree plot
print(paste("Selected Number of Components:", num_components))

# Transform data using selected principal components
pca_data <- data.frame(pca_model$x[, 1:num_components])

# Add the target variable back
pca_data$Loan.No.Loan <- data_mod$Loan.No.Loan

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(pca_data$Loan.No.Loan, p = 0.8, list = FALSE)
trainData <- pca_data[trainIndex, ]
testData <- pca_data[-trainIndex, ]

# Logistic Regression
log_model <- glm(Loan.No.Loan ~ ., data = trainData, family = binomial)
log_pred <- predict(log_model, testData, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)
log_accuracy <- mean(log_pred_class == testData$Loan.No.Loan)

# Decision Tree
dt_model <- rpart(Loan.No.Loan ~ ., data = trainData, method = "class")
dt_pred <- predict(dt_model, testData, type = "class")
dt_accuracy <- mean(dt_pred == testData$Loan.No.Loan)

# Random Forest
rf_model <- randomForest(Loan.No.Loan ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
rf_accuracy <- mean(rf_pred == testData$Loan.No.Loan)

# XGBoost
train_matrix <- model.matrix(Loan.No.Loan ~ ., data = trainData)[,-1]
test_matrix <- model.matrix(Loan.No.Loan ~ ., data = testData)[,-1]

dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(trainData$Loan.No.Loan) - 1)
dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(testData$Loan.No.Loan) - 1)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic", verbose = 0)
xgb_pred <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)
xgb_accuracy <- mean(xgb_pred_class == testData$Loan.No.Loan)

# Compare Accuracy
accuracy_results <- data.frame(Model = c("Logistic Regression", "Decision Tree", "Random Forest", "XGBoost"),
                               Accuracy = c(log_accuracy, dt_accuracy, rf_accuracy, xgb_accuracy))
print(accuracy_results)





#####################################
#VISUALISATIONS
# Load necessary library
library(ggplot2)

#HISTOGRAMS
# Histogram for Lend.Amount
ggplot(data, aes(x = Lend.Amount)) +
  geom_histogram(binwidth = 5000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loan Amounts", x = "Lend Amount", y = "Count") +
  theme_minimal()

# Histogram for Interest.Charged
ggplot(data, aes(x = Interest.Charged)) +
  geom_histogram(binwidth = 1, fill = "seagreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Interest Charged", x = "Interest Charged", y = "Count") +
  theme_minimal()


#BARCHARTS
data2 <- read.csv(file.choose(), stringsAsFactors = FALSE)
# Bar chart for Home.Status
ggplot(data2, aes(x = Home.Status)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Home Status", x = "Home Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar chart for Reason
ggplot(data2, aes(x = Reason)) +
  geom_bar(fill = "forestgreen", color = "black", alpha = 0.7) +
  labs(title = "Reasons for Taking Loans", x = "Loan Reason", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar chart for State
ggplot(data, aes(x = State)) +
  geom_bar(fill = "darkorange", color = "black", alpha = 0.7) +
  labs(title = "Loan Distribution by State", x = "State", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#HEATMAP
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Select relevant numerical columns including Loan.No.Loan
num_vars <- data[, c("Yearly.Income", "Lend.Amount", "Interest.Charged", 
                     "Debt.to.Income", "Unpaid.Amount", "Usage.Rate", "Loan.No.Loan")]

# Compute correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

# Convert to long format for ggplot heatmap
melted_cor <- melt(cor_matrix)

# Heatmap using ggplot
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Chart
library(ggplot2)
# Count the number of records at each stage
application_count <- nrow(data2)
verification_count <- sum(data2$Validation == "Verified", na.rm = TRUE)
loan_approval_count <- sum(data2$Loan.No.Loan == 1, na.rm = TRUE)
loan_repayment_count <- sum(data2$Loan.No.Loan == 1 & data2$File.Status == "fully paid", na.rm = TRUE)
application_count
verification_count
loan_approval_count
loan_repayment_count
# Create a data frame for visualization
funnel_data <- data.frame(
  Stage = c("Application", "Verification", "Loan Approval", "Loan Repayment"),
  Count = c(application_count, verification_count, loan_approval_count, loan_repayment_count)
)
# Calculate cumulative counts
funnel_data$Cumulative <- cumsum(funnel_data$Count)
# Plot chart
ggplot(funnel_data, aes(x = Stage, y = Count, fill = Stage)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Loan Processing Chart", x = "Stage", y = "Number of Applications")

############
#Model Evaluation
# Load necessary libraries
library(caret)

# Logistic Regression
log_model <- glm(Loan.No.Loan ~ ., data = trainData, family = binomial)
log_pred <- predict(log_model, testData, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)
log_conf_matrix <- confusionMatrix(factor(log_pred_class), factor(testData$Loan.No.Loan))
log_accuracy <- log_conf_matrix$overall["Accuracy"]
log_precision <- log_conf_matrix$byClass["Precision"]
log_recall <- log_conf_matrix$byClass["Recall"]
log_f1 <- log_conf_matrix$byClass["F1"]

# Decision Tree
dt_model <- rpart(Loan.No.Loan ~ ., data = trainData, method = "class")
dt_pred <- predict(dt_model, testData, type = "class")
dt_conf_matrix <- confusionMatrix(factor(dt_pred), factor(testData$Loan.No.Loan))
dt_accuracy <- dt_conf_matrix$overall["Accuracy"]
dt_precision <- dt_conf_matrix$byClass["Precision"]
dt_recall <- dt_conf_matrix$byClass["Recall"]
dt_f1 <- dt_conf_matrix$byClass["F1"]

# Random Forest
rf_model <- randomForest(Loan.No.Loan ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
rf_conf_matrix <- confusionMatrix(factor(rf_pred), factor(testData$Loan.No.Loan))
rf_accuracy <- rf_conf_matrix$overall["Accuracy"]
rf_precision <- rf_conf_matrix$byClass["Precision"]
rf_recall <- rf_conf_matrix$byClass["Recall"]
rf_f1 <- rf_conf_matrix$byClass["F1"]

# XGBoost
train_matrix <- model.matrix(Loan.No.Loan ~ ., data = trainData)[,-1]
test_matrix <- model.matrix(Loan.No.Loan ~ ., data = testData)[,-1]

dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(trainData$Loan.No.Loan) - 1)
dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(testData$Loan.No.Loan) - 1)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic", verbose = 0)
xgb_pred <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)
xgb_conf_matrix <- confusionMatrix(factor(xgb_pred_class), factor(testData$Loan.No.Loan))
xgb_accuracy <- xgb_conf_matrix$overall["Accuracy"]
xgb_precision <- xgb_conf_matrix$byClass["Precision"]
xgb_recall <- xgb_conf_matrix$byClass["Recall"]
xgb_f1 <- xgb_conf_matrix$byClass["F1"]

# Compare Model Performance
metrics_results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "XGBoost"),
  Accuracy = c(log_accuracy, dt_accuracy, rf_accuracy, xgb_accuracy),
  Precision = c(log_precision, dt_precision, rf_precision, xgb_precision),
  Recall = c(log_recall, dt_recall, rf_recall, xgb_recall),
  F1_Score = c(log_f1, dt_f1, rf_f1, xgb_f1)
)

# Print Results
print(metrics_results)
