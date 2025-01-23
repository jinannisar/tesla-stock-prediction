# Importing necessary library
library(dplyr)
install.packages("car")
library(car)

# Read the dataset
library(readr)
TSLA <- read_csv("C://Users//Jinan//Documents//TSLA.csv")
View(TSLA)

# Checking the first few rows of the dataset
head(TSLA)

# Checking the last few rows of the dataset
tail(TSLA)

# Checking the structure of the dataset
str(TSLA)

# Checking for NA values
anyNA(TSLA)

# If there are NA values and you want to remove them
TSLA <- na.omit(TSLA)

# Getting a summary of the dataset
summary(TSLA)

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

TSLA$Avg_Open_Close <- (TSLA$Open + TSLA$Close) / 2
head(TSLA)

TSLA$Daily_Return <- c(NA, diff(TSLA$Close) / lag(TSLA$Close)[-length(TSLA$Close)])
head(TSLA)

TSLA$Price_Range <- TSLA$High - TSLA$Low
head(TSLA)

rolling_window <- 5 # for instance, a 5-day rolling window
TSLA$Volatility <- rollapplyr(TSLA$Daily_Return, rolling_window, sd, fill = NA)
head(TSLA)

rolling_window_50 <- 50
TSLA$MA_50 <- rollapplyr(TSLA$Close, rolling_window_50, mean, fill = NA)
head(TSLA)

TSLA$Volume_Change <- c(NA, diff(TSLA$Volume) / lag(TSLA$Volume)[-length(TSLA$Volume)])
head(TSLA)

TSLA$Momentum <- TSLA$Close - lag(TSLA$Close, 5)
head(TSLA)

TSLA$Price_to_Volume_Ratio <- (TSLA$Close - TSLA$Open) / TSLA$Volume
head(TSLA)

TSLA$Weekday <- weekdays(TSLA$Date)
head(TSLA)

TSLA$Price_Direction <- c(NA, ifelse(diff(TSLA$Close) > 0, 1, 0))
head(TSLA)

TSLA <- na.omit(TSLA)
head(TSLA)

colnames(TSLA)[colnames(TSLA) == "Adj Close"] <- "Adj_Close"
head(TSLA)

# Plotting

# 1. Time series plot for Close Price and MA_50
ggplot(TSLA, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close Price")) +
  geom_line(aes(y = MA_50, color = "50-day Moving Average")) +
  labs(title = "Tesla Close Price & 50-day Moving Average Over Time", x = "Date", y = "Price") +
  theme_minimal() +
  scale_color_manual(values = c("Close Price" = "blue", "50-day Moving Average" = "red"))

# 2. Histogram for Daily Returns
ggplot(data = subset(TSLA, !is.na(Daily_Return)), aes(x = Daily_Return)) +
  geom_histogram(fill = "blue", binwidth = 0.01, alpha = 0.7) +
  labs(title = "Histogram of Daily Returns", x = "Daily Return", y = "Frequency") +
  theme_minimal()

# 3. Boxplot for Volume by Weekday
ggplot(TSLA, aes(x = Weekday, y = Volume)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Boxplot of Volume by Weekday", x = "Weekday", y = "Volume") +
  theme_minimal()

# 4. Scatterplot for Volume vs. Daily Return
ggplot(data = subset(TSLA, !is.na(Daily_Return)), aes(x = Volume, y = Daily_Return)) +
  geom_point(col = "blue", alpha = 0.7) +
  labs(title = "Scatterplot of Volume vs. Daily Return", x = "Volume", y = "Daily Return") +
  theme_minimal()

# 5. Histogram for Price_to_Volume_Ratio
ggplot(TSLA, aes(x = Price_to_Volume_Ratio)) +
  geom_histogram(fill = "blue", alpha = 0.7, binwidth = 0.0000001) +
  labs(title = "Histogram of Price to Volume Ratio", x = "Price_to_Volume_Ratio", y = "Frequency") +
  theme_minimal()

# 6. Time Series Plot for Volume
ggplot(TSLA, aes(x = Date, y = Volume)) +
  geom_line(col = "blue") +
  labs(title = "Tesla Trading Volume Over Time", x = "Date", y = "Volume") +
  theme_minimal()

# 7. Histogram for Price_Range
ggplot(TSLA, aes(x = Price_Range)) +
  geom_histogram(fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Daily Price Range", x = "Price Range", y = "Frequency") +
  theme_minimal()

# 8. Bar Chart for Price_Direction
ggplot(TSLA, aes(x = as.factor(Price_Direction))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Bar Chart of Price Direction", x = "Price Direction", y = "Count") +
  theme_minimal()

# 9. Time Series Plot for Volatility
ggplot(TSLA, aes(x = Date, y = Volatility)) +
  geom_line(col = "blue") +
  labs(title = "Tesla Stock Volatility Over Time", x = "Date", y = "Volatility") +
  theme_minimal()

# 10. Scatterplot of Close vs. Volume
ggplot(TSLA, aes(x = Close, y = Volume)) +
  geom_point(col = "blue", alpha = 0.7) +
  labs(title = "Scatterplot of Closing Price vs. Trading Volume", x = "Closing Price", y = "Volume") +
  theme_minimal()

# 11. Time Series Plot for Avg_Open_Close
ggplot(TSLA, aes(x = Date, y = Avg_Open_Close)) +
  geom_line(col = "darkgreen") +
  labs(title = "Tesla Average Opening & Closing Price Over Time", x = "Date", y = "Average Price") +
  theme_minimal()

# 12. Scatterplot of Avg_Open_Close vs. Volume
ggplot(TSLA, aes(x = Avg_Open_Close, y = Volume)) +
  geom_point(col = "darkred", alpha = 0.7) +
  labs(title = "Scatterplot of Average Opening & Closing Price vs. Volume", x = "Average Price", y = "Volume") +
  theme_minimal()

# 13. Histogram of Momentum
ggplot(TSLA, aes(x = Momentum)) +
  geom_histogram(fill = "skyblue", alpha = 0.7, binwidth = 0.5) +
  labs(title = "Histogram of Momentum", x = "Momentum", y = "Frequency") +
  theme_minimal()

# Correlation matrix
numeric_columns <- c("Open", "High", "Low", "Close", "Volume", "Avg_Open_Close", "Daily_Return", "Price_Range", "Volatility", "MA_50", "Volume_Change", "Momentum", "Price_to_Volume_Ratio", "Price_Direction")
correlation_matrix <- cor(TSLA[,numeric_columns], use = "complete.obs")
print(correlation_matrix)

# Load necessary libraries
library(ggplot2)
library(reshape2)  # for melt function

# Assuming the correlation matrix has already been computed
# Melt the correlation matrix to long format for ggplot2
melted_corr <- melt(correlation_matrix)

# Plot heatmap
ggplot(data = melted_corr, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), color='white') +
  geom_text(aes(label=round(value, 2)), size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, size=10, hjust=1),
        axis.text.y = element_text(size=10)) +
  coord_fixed()




# Load necessary libraries
library(caret)  # for train/test split and training models
library(Metrics)  # for model evaluation

# Assuming TSLA dataframe is your data

# 1. Split data into training and testing sets
set.seed(123)  # for reproducibility
splitIndex <- createDataPartition(TSLA$Close, p = 0.8, list = FALSE)
train_data <- TSLA[splitIndex,]
test_data <- TSLA[-splitIndex,]
colnames(train_data)

# For regression: Predict 'Close' price
# 2. Train multiple models
# Linear Regression
model_lm <- lm(Close ~ . - Date, data = train_data)  # Excluding Date for the model

# Summary of the model
summary_model <- summary(model_lm)

# Print the summary
print(summary_model)

# Extract and print the F-statistic
cat("F-statistic:", summary_model$fstatistic[1], "\n")

# Extract and print the t-values for individual predictors
cat("T-values:\n")
print(coef(summary_model)[, "t value"])

# Extract and print the p-values for individual predictors
cat("P-values for individual coefficients:\n")

print(coef(summary_model)[, "Pr(>|t|)"])

# 3. Evaluate models on the test set
pred_lm <- predict(model_lm, newdata = test_data)

# RMSE for evaluation
rmse_lm <- rmse(test_data$Close, pred_lm)

# For classification: Predict 'Price_Direction' (assuming it's binary: 0 or 1)
# Linear Model (Logistic Regression)
model_logistic <- glm(Price_Direction ~ . - Date - Close, data = train_data, family = "binomial")

# Evaluate models on the test set
pred_logistic_probs <- predict(model_logistic, newdata = test_data, type = "response")
pred_logistic <- ifelse(pred_logistic_probs > 0.5, 1, 0)

# Accuracy for evaluation
acc_logistic <- sum(pred_logistic == test_data$Price_Direction) / nrow(test_data)

# 4. Compare performance metrics and select the best model

print(paste("RMSE for Linear Regression:", rmse_lm))

print(paste("Accuracy for Logistic Regression:", acc_logistic))

# Load necessary libraries
library(ggplot2)

# Assuming you already predicted using some models (e.g., model_lm for linear regression)
pred_lm <- predict(model_lm, newdata = test_data)

# 1. Time Series Plot for Actual vs. Predicted
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Actual"), size = 1) +
  geom_line(aes(y = pred_lm, color = "Predicted"), size = 1) +
  labs(title = "Actual vs Predicted Close Price", x = "Date", y = "Price") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

# 2. Scatterplot of Actual vs. Predicted
ggplot(test_data, aes(x = Close, y = pred_lm)) +
  geom_point(aes(color = Date), alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +  # Adds regression line
  labs(title = "Actual vs Predicted Close Price", x = "Actual Price", y = "Predicted Price") +
  theme_minimal()

# 3. Residual Plot
residuals_lm <- test_data$Close - pred_lm
ggplot(data.frame(Residuals = residuals_lm, Predicted = pred_lm), aes(x = Predicted, y = Residuals)) +
  geom_point(aes(color = Predicted), alpha = 0.7) +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Residuals vs Predicted", x = "Predicted", y = "Residuals") +
  theme_minimal()

# 4. Density Plot of Residuals
residuals_lm <- residuals(model_lm)
ggplot(data.frame(Residuals = residuals_lm), aes(x = Residuals)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Residuals", x = "Residuals") +
  theme_minimal()

# 5. Shapiro-Wilk Normality Test
shapiro_test <- shapiro.test(test_data$Close)
print(shapiro_test)
shapiro_test <- shapiro.test(test_data$Open)
print(shapiro_test)
