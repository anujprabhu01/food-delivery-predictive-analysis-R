#Author: Anuj Prabhu
#Class: DAT301 TTh 12:00-1:15pm
#Date: 29 March 2024

# Project 1 (source r file)

# Background

#This dataset contains information about orders placed to a food ordering platform. Each observation
#contains demographic and location factors about the customer, as well as whether the customer left positive
#or negative feedback after the order, and whether the order was successful or not.

#library imports
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggmap)
library(sf)
library(caTools)
library(plotly)
library(vcd) #Visualizing Categorical Data
library(psych)
library(rstatix)

#current working directory
getwd()

#Loading the Dataset

online_foods <- read.csv("./data/onlinefoods.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
#View(online_foods)

#general summary
head(online_foods)
summary(online_foods)
str(online_foods)
colnames(online_foods)
nrow(online_foods)
ncol(online_foods)

#summary for each variable
table(online_foods$Age)
table(online_foods$Gender)
table(online_foods$Marital.Status)
table(online_foods$Occupation)
table(online_foods$Monthly.Income)
table(online_foods$Educational.Qualifications)
table(online_foods$Family.size)
table(online_foods$Output)
table(online_foods$Feedback)

#understand "X" variable better
unique_values <- online_foods %>% 
  distinct(X)
unique_values

# Cleaning the Dataset

#custom colnames
colnames(online_foods)[7] <- "Family.Size"
colnames(online_foods)[8] <- "Latitude"
colnames(online_foods)[9] <- "Longitude"
colnames(online_foods)[10] <- "Pin.Code"
colnames(online_foods)

online_foods <- online_foods %>%
  select(-X) #removing "X" variable from df because no background has been provided about this attribute and it is not self-explanatory.
#colnames(online_foods)
#ncol(online_foods)

#check for NA values in dataset
has_na <- any(is.na(online_foods))
has_na #no NA values in dataset

#Excluding "Prefer not to say" Marital Status values from analysis (handling outlying data points)
online_foods$Marital.Status <- as.character(online_foods$Marital.Status)
online_foods <- online_foods %>%
  filter(online_foods$Marital.Status != "Prefer not to say")
#table(online_foods$Marital.Status)
online_foods$Marital.Status <- as.factor(online_foods$Marital.Status)

#Combining like factors
#table(online_foods$Occupation)
online_foods$Occupation <- as.character(online_foods$Occupation)
online_foods$Occupation <- case_when(
  online_foods$Occupation %in% c("Employee", "Self Employeed") ~ "Employed",
  #online_foods$Occupation == "House wife" ~ "Housewife",
  TRUE ~ as.character(online_foods$Occupation) # Keep other values unchanged
)
#table(online_foods$Occupation)
online_foods$Occupation <- as.factor(online_foods$Occupation)

#table(online_foods$Monthly.Income)
online_foods$Monthly.Income <- as.character(online_foods$Monthly.Income)
online_foods$Monthly.Income <- case_when(
  online_foods$Monthly.Income %in% c("10001 to 25000", "25001 to 50000") ~ "Average Monthly Income",
  online_foods$Monthly.Income == "More than 50000" ~ "High Monthly Income",
  online_foods$Monthly.Income %in% c("No Income", "Below Rs.10000") ~ "Low/No Monthly Income",
  TRUE ~ as.character(online_foods$Monthly.Income) # Keep other values unchanged
)
#table(online_foods$Monthly.Income)
online_foods$Monthly.Income <- as.factor(online_foods$Monthly.Income)

#table(online_foods$Educational.Qualifications)
online_foods$Educational.Qualifications <- as.character(online_foods$Educational.Qualifications)
online_foods$Educational.Qualifications <- case_when(
  online_foods$Educational.Qualifications %in% c("Post Graduate", "Ph.D") ~ "Higher Education",
  online_foods$Educational.Qualifications %in% c("School", "Uneducated") ~ "Lower/No Education",
  TRUE ~ as.character(online_foods$Educational.Qualifications)
)
#table(online_foods$Educational.Qualifications)
online_foods$Educational.Qualifications <- as.factor(online_foods$Educational.Qualifications)

#table(online_foods$Output)
online_foods$Output <- as.character(online_foods$Output)
online_foods$Output <- case_when(
  online_foods$Output == "No" ~ "Unsuccessful",
  online_foods$Output == "Yes" ~ "Successful",
  TRUE ~ as.character(online_foods$Output)
)
#table(online_foods$Output)
online_foods$Output <- as.factor(online_foods$Output)

str(online_foods)

#reordering factors according to customs levels
levels(online_foods$Marital.Status)
online_foods$Marital.Status <- factor(online_foods$Marital.Status,
                                      levels = c("Single", "Married"))
levels(online_foods$Occupation)
online_foods$Occupation <- factor(online_foods$Occupation,
                                  levels = c("House wife", "Student", "Employed"))
levels(online_foods$Monthly.Income)
online_foods$Monthly.Income <- factor(online_foods$Monthly.Income,
                                      levels = c("Low/No Monthly Income", "Average Monthly Income",
                                                 "High Monthly Income"))
levels(online_foods$Educational.Qualifications)
online_foods$Educational.Qualifications <- factor(online_foods$Educational.Qualifications,
                                                  levels = c("Lower/No Education", "Graduate",
                                                             "Higher Education"))
levels(online_foods$Output)
online_foods$Output <- factor(online_foods$Output,
                                      levels = c("Unsuccessful", "Successful"))
str(online_foods)
summary(online_foods)

#EDA (Exploratory Data Analysis)

#main problem: want to understand how feedback by customers after receiving their order (positive or negative)
#and the output of the order (successful or unsuccessful) is affected by demographic and locational factors.

#DEMOGRAPHIC FACTORS: Age, Gender, Marital.Status, Occupation, Monthly.Income, Educational.Qualifications, Family.Size

#LOCATIONAL FACTORS: Latitude, Longitude, Pin.Code

#DEPENDENT VARIABLES: Output, Feedback

## General insights about the data

### Insights for ORDER FEEDBACK
#distribution of Feedback by Age
# chi_sq_result <- chisq.test(table(online_foods$Age, online_foods$Feedback))
# chi_sq_result
log_model <- glm(Feedback ~ Age, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Age, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Age", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
ggplotly(
  ggplot(online_foods, aes(x = Age, fill = Feedback)) +
           geom_density(alpha = 0.5) +
           labs(fill = "Feedback")
  )
#FEEDBACK ~ AGE: STRONG PREDICTOR

#distribution of Feedback by Gender
ggplot(online_foods, aes(x = Gender, fill = Feedback)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Feedback by Gender", y = "Proportion", fill = "Order Feedback")
log_model <- glm(Feedback ~ Gender, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Gender, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Gender", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
phi_coefficient <-  phi(table(online_foods$Feedback, online_foods$Gender))
phi_coefficient
#FEEDBACK ~ GENDER: WEAK PREDICTOR

#distribution of Feedback by Marital Status
ggplot(online_foods, aes(x = Marital.Status, fill = Feedback)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Feedback by Marital Status", y = "Proportion", fill = "Order Feedback")
log_model <- glm(Feedback ~ Marital.Status, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Marital.Status, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Marital Status", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
phi_coefficient <-  phi(table(online_foods$Feedback, online_foods$Marital.Status))
phi_coefficient #phi coefficient only for association between binary variables
#FEEDBACK ~ MARITAL.STATUS: STRONG PREDICTOR

#distribution of Feedback by Occupation
ggplot(online_foods, aes(x = Occupation, fill = Feedback)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Feedback by Occupation", y = "Proportion", fill = "Order Feedback")
log_model <- glm(Feedback ~ Occupation, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Occupation, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Occupation", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
#manual calculations
cont_table <- table(online_foods$Occupation, online_foods$Feedback)
chi_sq_result <- chisq.test(cont_table)
chi_sq_result
n <- sum(cont_table)  # Total number of observations
k <- min(dim(cont_table)) - 1  # Minimum of rows and columns - 1
chi_sq <- chi_sq_result$statistic  # Chi-square statistic from the test
cramers_v <- sqrt(chi_sq / (n * k))
cramers_v
#rstatix calculations
chisq_test(cont_table)
cramer_v(cont_table)
#FEEDBACK ~ OCCUPATION: STRONG PREDICTOR

#distribution of Feedback by Monthly Income
ggplot(online_foods, aes(x = Monthly.Income, fill = Feedback)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Feedback by Monthly Income", y = "Proportion", fill = "Order Feedback")
log_model <- glm(Feedback ~ Monthly.Income, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Monthly.Income, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Monthly Income", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
cont_table <- table(online_foods$Monthly.Income, online_foods$Feedback)
chisq_test(cont_table)
cramer_v(cont_table)
#FEEDBACK ~ MONTHTY INCOME: WEAK PREDICTOR

#distribution of Feedback by Educational Qualifications
ggplot(online_foods, aes(x = Educational.Qualifications, fill = Feedback)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Feedback by Educational Qualifications", y = "Proportion", fill = "Order Feedback")
log_model <- glm(Feedback ~ Educational.Qualifications, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Educational.Qualifications, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Educational Qualifications", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
cont_table <- table(online_foods$Educational.Qualifications, online_foods$Feedback)
chisq_test(cont_table)
cramer_v(cont_table)
#FEEDBACK ~ EDUCATIONAL QUALIFICATIONS: STRONG PREDICTOR (?)

#distribution of Feedback by Family Size
ggplotly(
  ggplot(online_foods, aes(x = Family.Size, fill = Feedback)) +
    geom_density(alpha = 0.5) +
    labs(fill = "Feedback")
)
log_model <- glm(Feedback ~ Family.Size, data = online_foods, family = binomial)
summary(log_model)
predicted_feedback <- predict(log_model, type = "response")
ggplot(online_foods, aes(x = Family.Size, y = predicted_feedback)) +
  geom_point() +  # Scatter plot of predicted probabilities
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression line
  labs(x = "Family Size", y = "Predicted Probability of Positive Feedback", title = "Logistic Regression Model") +
  theme_minimal()
cont_table <- table(online_foods$Family.Size, online_foods$Feedback)
chisq_test(cont_table)
#FEEDBACK ~ FAMILY SIZE: WEAK PREDICTOR

### Insights for ORDER OUTPUT
#distribution of Order Output by Age
ggplotly(
  ggplot(online_foods, aes(x = Age, fill = Output)) +
    geom_density(alpha = 0.5) +
    labs(fill = "Output")
)

#distribution of Order Output by Gender
ggplot(online_foods, aes(x = Gender, fill = Output)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Output by Gender", y = "Proportion", fill = "Order Output")

#distribution of Order Output by Marital Status
ggplot(online_foods, aes(x = Marital.Status, fill = Output)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Output by Marital Status", y = "Proportion", fill = "Order Output")

#distribution of Order Output by Occupation
ggplot(online_foods, aes(x = Occupation, fill = Output)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Output by Occupation", y = "Proportion", fill = "Order Output")

#distribution of Order Output by Monthly Income
ggplot(online_foods, aes(x = Monthly.Income, fill = Output)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Output by Monthly Income", y = "Proportion", fill = "Order Output")

#distribution of Order Output by Educational Qualifications
ggplot(online_foods, aes(x = Educational.Qualifications, fill = Output)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Order Output by Educational Qualifications", y = "Proportion", fill = "Order Output")

#distribution of Order Output by Family Size
ggplotly(
  ggplot(online_foods, aes(x = Family.Size, fill = Output)) +
    geom_density(alpha = 0.5) +
    labs(fill = "Output")
)

### Regression Analysis using Logistic Regression

# Set seed for reproducibility
set.seed(123)

# Splitting data into training (80%) and testing (20%) sets
train_index <- sample(1:nrow(online_foods), 0.8 * nrow(online_foods))
train_data <- online_foods[train_index, ]
test_data <- online_foods[-train_index, ]

## Logistic Regression Model for Feedback

#Bad Model
feedback_model <- glm(Feedback ~ ., data = train_data, family = binomial)

# Summary of the model
summary(feedback_model)

# Predictions on the test set
feedback_predictions <- predict(feedback_model, newdata = test_data, type = "response")

# Convert probabilities to classes (Positive/Negative)
feedback_predictions_class <- ifelse(feedback_predictions > 0.5, "Positive", "Negative")

# Confusion Matrix for Feedback
feedback_confusion_matrix <- table(Actual = test_data$Feedback, Predicted = feedback_predictions_class)
print("Confusion Matrix for Feedback:")
print(feedback_confusion_matrix)

# Extracting values from the confusion matrix
true_negative <- feedback_confusion_matrix[1, 1]
false_positive <- feedback_confusion_matrix[1, 2]
false_negative <- feedback_confusion_matrix[2, 1]
true_positive <- feedback_confusion_matrix[2, 2]

# Calculate accuracy
accuracy <- (true_positive + true_negative) / sum(feedback_confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate precision
precision <- true_positive / (true_positive + false_positive)
cat("Precision:", precision, "\n")

# Calculate recall (sensitivity)
recall <- true_positive / (true_positive + false_negative)
cat("Recall (Sensitivity):", recall, "\n")

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

#Better Model
feedback_model <- glm(Feedback ~ . 
                      - Output - Latitude - Longitude - Pin.Code, 
                      data = train_data, family = binomial)

# Summary of the model
summary(feedback_model)

# Predictions on the test set
feedback_predictions <- predict(feedback_model, newdata = test_data, type = "response")

# Convert probabilities to classes (Positive/Negative)
feedback_predictions_class <- ifelse(feedback_predictions > 0.5, "Positive", "Negative")

# Confusion Matrix for Feedback
feedback_confusion_matrix <- table(Actual = test_data$Feedback, Predicted = feedback_predictions_class)
print("Confusion Matrix for Feedback:")
print(feedback_confusion_matrix)

# Extracting values from the confusion matrix
true_negative <- feedback_confusion_matrix[1, 1]
false_positive <- feedback_confusion_matrix[1, 2]
false_negative <- feedback_confusion_matrix[2, 1]
true_positive <- feedback_confusion_matrix[2, 2]

# Calculate accuracy
accuracy <- (true_positive + true_negative) / sum(feedback_confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate precision
precision <- true_positive / (true_positive + false_positive)
cat("Precision:", precision, "\n")

# Calculate recall (sensitivity)
recall <- true_positive / (true_positive + false_negative)
cat("Recall (Sensitivity):", recall, "\n")

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

#Even Better Model
feedback_model <- glm(Feedback ~ . 
                      - Output - Latitude - Longitude - Pin.Code - Gender - Monthly.Income - Family.Size
                      - Occupation, 
                      data = train_data, family = binomial)

# Summary of the model
summary(feedback_model)

# Predictions on the test set
feedback_predictions <- predict(feedback_model, newdata = test_data, type = "response")

# Convert probabilities to classes (Positive/Negative)
feedback_predictions_class <- ifelse(feedback_predictions > 0.5, "Positive", "Negative")

# Confusion Matrix for Feedback
feedback_confusion_matrix <- table(Actual = test_data$Feedback, Predicted = feedback_predictions_class)
print("Confusion Matrix for Feedback:")
print(feedback_confusion_matrix)

# Extracting values from the confusion matrix
#true_negative <- feedback_confusion_matrix[1, 1]
false_positive <- feedback_confusion_matrix[1, 1]
#false_negative <- feedback_confusion_matrix[2, 1]
true_positive <- feedback_confusion_matrix[2, 1]

# Calculate accuracy
accuracy <- (true_positive + 0) / sum(feedback_confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate precision
precision <- true_positive / (true_positive + false_positive)
cat("Precision:", precision, "\n")

# Calculate recall (sensitivity)
recall <- true_positive / (true_positive + 0)
cat("Recall (Sensitivity):", recall, "\n")

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

## Logistic Regression Model for Output

output_model <- glm(Output ~ . 
                    - Feedback - Latitude - Longitude - Pin.Code
                    - Educational.Qualifications - Occupation - Gender
                    - Family.Size, data = train_data, family = binomial)

# Summary of the model
summary(output_model)

# Predictions on the test set
output_predictions <- predict(output_model, newdata = test_data, type = "response")

# Convert probabilities to classes (Successful/Unsuccessful)
output_predictions_class <- ifelse(output_predictions > 0.5, "Successful", "Unsuccessful")

# Confusion Matrix for Output
output_confusion_matrix <- table(Actual = test_data$Output, Predicted = output_predictions_class)
print("Confusion Matrix for Output:")
print(output_confusion_matrix)

# Extracting values from the confusion matrix
true_negative <- output_confusion_matrix[1, 1]
false_positive <- output_confusion_matrix[1, 2]
false_negative <- output_confusion_matrix[2, 1]
true_positive <- output_confusion_matrix[2, 2]

# Calculate accuracy
accuracy <- (true_positive + true_negative) / sum(output_confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate precision
precision <- true_positive / (true_positive + false_positive)
cat("Precision:", precision, "\n")

# Calculate recall (sensitivity)
recall <- true_positive / (true_positive + false_negative)
cat("Recall (Sensitivity):", recall, "\n")

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

##Locational analysis

#understand which region orders are being placed from; we know currency is rupees but do not know country
coordinates_df <- data.frame(
  latitude = c(online_foods[which.min(online_foods$Latitude), "Latitude"],
               online_foods[which.max(online_foods$Latitude), "Latitude"],
               online_foods[which.min(online_foods$Longitude), "Latitude"],
               online_foods[which.max(online_foods$Longitude), "Latitude"]),
  longitude = c(online_foods[which.min(online_foods$Latitude), "Longitude"],
                online_foods[which.max(online_foods$Latitude), "Longitude"],
                online_foods[which.min(online_foods$Longitude), "Longitude"],
                online_foods[which.max(online_foods$Longitude), "Longitude"])
)
coordinates_df

api_key <- Sys.getenv("API_KEY")
register_google(key = api_key)
 
region_info <- apply(coordinates_df, 1, function(row) { #apply function(row) to each row (1) of coordinates_df.
  revgeocode(c(row["longitude"], row["latitude"]), output_type = "more")
})
region_info #orders are placed from Begaluru, Karnataka, India.

### Visualizing the dataset

#Explore relationships between demographic factors and ordering pattern
online_foods %>%
  ggplot(aes(x = Occupation, y = Age, fill = Gender)) +
  geom_boxplot()

#see whether unsuccessful orders are correlated to negative feedback.
ggplot(online_foods, aes(x = Output, y = Feedback)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  labs(title = "Relationship between Order Output and Order Feedback",
       x = "Output", y = "Feedback")

#Pie chart visualization of Customer Base by Occupation
# Calculate frequencies of each occupation
occupation_freq <- table(online_foods$Occupation)

# Convert frequencies to a data frame
occupation_df <- as.data.frame(occupation_freq)
colnames(occupation_df) <- c("Occupation", "Frequency")
#occupation_df
pct <- round(occupation_df$Frequency/sum(occupation_df$Frequency)*100)
lbls <- paste(occupation_df$Occupation, pct, sep = "; ")
lbls <- paste(lbls, "%", sep = "")
pie3D(occupation_df$Frequency, 
      labels = lbls, labelcex = 1,
      col = rainbow(length(lbls)),
      main = "Pie Chart of Customer Base by Occupation")