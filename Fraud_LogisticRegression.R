#why we use LR:To predict the probability of an event occurring based on a list of 
#one or more predictor variables and thereby to classify the expected outcome
#1) Data acquisition
#read the file manually(use)
Credit_Fraud<-read.csv(file.choose())

#where class is the target variable and v1 to v28(its normally distributed)

#2)EDA
#assumptions
#Dependent Variable outcomes should be mutually exclusive
#Absence of Multicollinearity and outliers
#Independence of Errors

#2.1)Check the structure of the dataset
str(Credit_Fraud)

#get the variable type
Type_Fraud <- sapply(Credit_Fraud, class)
print(Type_Fraud)

#2.2)Check for missing values in the entire dataset
missing_values_Credit_Fraud <- sum(is.na(Credit_Fraud))
missing_values_Credit_Fraud

#2.3)Check for missing values in each column
missing_values_per_column_Credit_Fraud <- colSums(is.na(Credit_Fraud))
missing_values_per_column_Credit_Fraud

#2.4)heck for any duplicate values
duplicates <- Credit_Fraud[duplicated(Credit_Fraud), ]
duplicates

#2.5)Histogram just to see the distributions and both time and amount
hist(Credit_Fraud$Time,
     main = "Distribution of Time",
     xlab = "Time",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

hist(Credit_Fraud$Amount,
     main = "Distribution of Amount",
     xlab = "Amount",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

#2.6)# Detecting outliers using boxplot for both Time and Amount
boxplot(Credit_Fraud$Time, Credit_Fraud$Amount, main="Boxplot of Time and Amount",
        names = c("Time", "Amount"), col=c("skyblue", "lightgreen"))

#2.7)checking the frequency of class(0 ,1) which can say us if data is balanced or not
# Yes,data is unbalanced and hence do OVERSAMPLING with minority

Class_new <- Credit_Fraud$Class
Class_Frequency<- table(Class_new)
print(Class_Frequency)

# Increase the size of the plot
par(mar = c(4, 4, 3, 5) + 0.1)  # Adjust margin size, increase space for legend
# Plot using a logarithmic scale
barplot(Class_Frequency, 
        main = "Frequency of Fraud and Non-fraud Classes",
        xlab = "Class",
        ylab = "Frequency",
        col = c("skyblue", "lightgreen"),
        width = 0.5,  # Adjust bar width
        beside = TRUE,  # Display bars side by side
        log = "y",  # Use logarithmic scale for y-axis
        ylim = c(1, max(Class_Frequency) * 1.5))  # Adjust ylim to show all frequencies

# Add legend to the right corner
legend("topright",  # Position the legend in the top-right corner
       legend = c("Non-fraud", "Fraud"),
       fill = c("skyblue", "lightgreen"),
       title = "Class",
       cex = 0.8)  # Adjust the size of the legend text

# Calculate the imbalance percentage
Class_Frequency<- table(Class_new)
imbalance <- (Class_Frequency[2] / sum(Class_Frequency) * 100) / (Class_Frequency[1] / sum(Class_Frequency) * 100) * 100
print(paste("Imbalance Percentage:", imbalance))

#3) Descriptive statistics
install.packages("psych")
library(psych)

#3.1)to get insights
summary(Credit_Fraud) #get mean , median, max details

describe(Credit_Fraud) #to get kurtosis, skewness, sd details

#3.2)Calculate correlation matrix
Credit_Fraud_corr <- cor(Credit_Fraud)

#plot corr matrix
install.packages("corrplot")
library(corrplot)
corrplot(Credit_Fraud_corr, method = "circle")

# Increase the size of the plotting window
options(repr.plot.width=10, repr.plot.height=8)

# Plot heatmap
heatmap(Credit_Fraud_corr, 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Choose color palette
        scale = "none",  # Scale the data within each column
        symm = TRUE,  # Show symmetrical correlation matrix
        mar = c(2, 2),
        main = "Correlation Heatmap of Fraud Data")  # Add some margin space

#3.3)Normalising all the columns except the target variable 'Class'
#extracting
Credit_Fraud_Normalise<- Credit_Fraud[, !(names(Credit_Fraud) %in% c("Class"))]

# Perform normalization using scale function
Credit_Fraud_Normalize <- scale(Credit_Fraud_Normalise)

# Combine the normalized data with the target variable 'Class'
Credit_Fraud_Normalized <- cbind(Class = Credit_Fraud$Class, Credit_Fraud_Normalize)

# View the updated dataset
head(Credit_Fraud_Normalized) #first 6 rows

#3.4)OverSampling
# Load the ROSE package
install.packages("ROSE")
library(ROSE)

Credit_Fraud_Normalized  <- data.frame(Credit_Fraud_Normalized)

# Perform SMOTE for oversampling and Random Undersampling for undersampling
Credit_Fraud_Oversample <- ovun.sample(Class ~ ., data = Credit_Fraud_Normalized, method = "both", N = nrow(Credit_Fraud_Normalized), seed = 256)$data
# Print the value counts of the resampled 'Class'
table(Credit_Fraud_Oversample)

#4)Modeling
#Model 1) - with all the variables

# Run logistic regression
Logic_Fraud_1 <- glm(Class ~ ., data = Credit_Fraud_Oversample, family = binomial)

# Summary of the model
summary(Logic_Fraud_1) #we get some summary in console

# Make predictions using the fitted model
predictions_1 <- ifelse(predict(Logic_Fraud_1, type = "response") > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix_1 <- table(predictions_1, Credit_Fraud_Oversample$Class)

# Print confusion matrix
print(confusion_matrix_1)

# Load the caret package
library(caret)

# Create confusion matrix object
confusion_matrix_1 <- confusionMatrix(factor(predictions_1), factor(Credit_Fraud_Oversample$Class))

# Print the confusion matrix
print(confusion_matrix_1)

# Print summary statistics
print(confusion_matrix_1$overall)

#Model 2) - After removal of outliers
#outlier detection and removal

# Check the Old distribution
hist(Credit_Fraud_Oversample$Amount, main="Distribution of Amount before outliers")

# Calculate the quartiles
Q1 <- quantile(Credit_Fraud_Oversample$Amount, 0.25)
Q3 <- quantile(Credit_Fraud_Oversample$Amount, 0.75)

# Calculate the interquartile range (IQR)
IQR <- Q3 - Q1

# Define the upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
Credit_Fraud_Outliers <- Credit_Fraud_Oversample$Amount[Credit_Fraud_Oversample$Amount < lower_bound | Credit_Fraud_Oversample$Amount > upper_bound]

# Remove outliers
Fraud_new <- Credit_Fraud_Oversample[!(Credit_Fraud_Oversample$Amount %in% Credit_Fraud_Outliers), ]

# Check the new distribution
hist(Fraud_new$Amount, main="Distribution of Amount after removing outliers")

#Create logistic regression model
Logic_Fraud_2 <- glm(Class ~ ., data = Fraud_new, family = binomial)

#Summary of the model
summary(Logic_Fraud_2)

#Make predictions using the fitted model
predictions_2 <- ifelse(predict(Logic_Fraud_2, type = "response") > 0.5, 1, 0)

#Create confusion matrix
confusion_matrix_2 <- table(predictions_2, Fraud_new$Class)

#Print confusion matrix
print(confusion_matrix_2)

# Load the caret package
library(caret)

# Create confusion matrix object
confusion_matrix_2 <- confusionMatrix(factor(predictions_2), factor(Fraud_new$Class))

# Print the confusion matrix
print(confusion_matrix_2)

# Print summary statistics
print(confusion_matrix_2$overall)


#Model 3) - Principal Component Analysis (PCA)- dimensionality reduction technique 
#before starting with PCA , do few tests
# 4.3.1) Check sample size
str(Fraud_new)

# 4.3.2) Perform Bartlett's test of Sphericity
Bartlett_Fraud <- bartlett.test(Fraud_new)
Bartlett_Fraud$p.value #0

# 4.3.3) Calculate KMO(Kaiser-Meyer-Olkin) Measure
KMO_Fraud <- psych::KMO(Fraud_new)
KMO_Fraud$MSA  # Measure of Sampling Adequacy #0.7784395

# 4.3.4) we can proceed with PCA
PCA_Credit_Fraud <- princomp(Fraud_new, cor = TRUE)
PCA_Credit_Fraud

# Summary of PCA
summary(PCA_Credit_Fraud)

# Calculate percentage of variance explained
Variance <- PCA_Credit_Fraud$sdev^2 / sum(PCA_Credit_Fraud$sdev^2) * 100

# 4.3.5) Scree plot
screeplot(PCA_Credit_Fraud, type = "line", main = "Scree Plot")
# Add percentage labels
text(x = 1:length(Variance), y = PCA_Credit_Fraud$sdev^2, 
     labels = paste(round(Variance, 2), "%"), pos = 3, cex = 0.8)


# 4.3.6) Cumulative plot
#Calculate the cumulative proportion of variance explained
cumulative_Variance <- cumsum(PCA_Credit_Fraud$sdev^2 / sum(PCA_Credit_Fraud$sdev^2))

# Plot the cumulative scree plot
plot(1:length(cumulative_Variance), cumulative_Variance, type = "b", 
     main = "Cumulative Scree Plot", xlab = "Number of Components", 
     ylab = "Cumulative Proportion of Variance Explained")


#4.3.7) Model building

# Extract PCA scores from the princomp object
PCA_Credit_scores <- predict(PCA_Credit_Fraud)

# Combine PCA scores with the target variable
PCA_Credit_data <- cbind(PCA_Credit_scores, Class = Fraud_new$Class)

# Convert PCA_data to a data frame
PCA_Credit_Fraud <- as.data.frame(PCA_Credit_data)

# Create logistic regression model with PCA-transformed data
Logic_PCA <- glm(Class ~ ., data = PCA_Credit_Fraud , family = binomial)

#Summary of the model
summary(Logic_PCA)

#Make predictions using the fitted model
predictions_3 <- ifelse(predict(Logic_PCA, type = "response") > 0.5, 1, 0)

#Create confusion matrix
confusion_matrix_3 <- table(predictions_3, Fraud_new$Class)

#Print confusion matrix
print(confusion_matrix_3)

# Load the caret package
library(caret)

# Create confusion matrix object
confusion_matrix_3 <- confusionMatrix(factor(predictions_3), factor(Fraud_new$Class))

# Print the confusion matrix
print(confusion_matrix_3)

# Print summary statistics
print(confusion_matrix_3$overall)


#5)Evaluation

#Confusion Matrix 1:
#Accuracy: 94.86%
#Kappa: 89.72%

#Confusion Matrix 2:
#Accuracy: 95.14%
#Kappa: 90.24%

#Confusion Matrix 3:
#Accuracy: 100.00%
#Kappa: 100.00%



