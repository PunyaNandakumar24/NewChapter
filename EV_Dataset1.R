#Crisp DM method
#1) Business Understanding 
#Goal:A Data Analytics Approach to analyse "What are the factors influencing electric vehicle sales 
#in Washington, USA between 2021 and 2024, and which models have contributed the most to these sales?"

#2)Data Understanding
#2.1)Data aquisition

#read the file manually
EV<-read.csv(file.choose())

#2.2)Data and Datasets exploration(Explaining the types of variables and data set structure)

sapply(EV, class) #gives the column names and data type 
str(EV)  #explores my data eg :166800 obs. of  17 variables

#3) DATA Pre-Processing

#3.1)Filtering of the data 
library(dplyr) #dplyr used for data manipulation
#filter based on year to reduce the data set

EV<- EV %>% filter(Model.Year %in% c(2024, 2023, 2022, 2021))

str(EV)  # 101026 obs. of  17 variables

# multiple columns removal as they are not contributing to my analysis
EV<- EV[,!names(EV) %in% c("Base.MSRP","Legislative.District","DOL.Vehicle.ID",
                           "Vehicle.Location","Electric.Utility","X2020.Census.Tract")]

str(EV) #explores my data eg :101026 obs. of  11 variables

EV_1 <-  EV

#3.2)Checking and filtering the data based on top 15 model based on sales
#Create a vector containing the list of EV models
ev_models <- c("BMW", "RIVIAN", "TESLA", "FORD", "CHEVROLET", "MITSUBISHI", "NISSAN", "AUDI", "JEEP", 
               "VOLVO", "KIA", "TOYOTA", "HYUNDAI", "VOLKSWAGEN", "SUBARU", "CHRYSLER", "LUCID", "MINI", 
               "POLESTAR", "LINCOLN", "MERCEDES-BENZ", "LEXUS", "HONDA", "PORSCHE", "ALFA ROMEO", "DODGE", 
               "LAND ROVER", "MAZDA", "CADILLAC", "GENESIS", "JAGUAR", "FISKER", "GMC")

# Count the number of sales for each EV model
sales_count <- table(EV$Make)

# Sort the sales count in descending order
sorted_sales <- sort(sales_count, decreasing = TRUE)

# Get the top 15 selling EV models
top_15_models <- names(sorted_sales)[1:15]

# Print the top 15 selling EV models
print(top_15_models)

EV_1 <- EV_1 %>% filter(Make %in% top_15_models)

# Plot the top 15 models
library(ggplot2)
ggplot(EV_1, aes(x = factor(Model), fill = factor(Make))) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~., scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Top 15 Electric Vehicle Models",
       x = "Model",
       y = "Count") +
  scale_fill_discrete(name = "Make")

str(EV_1)

#3.3)Convert categorical variables to factors 
EV_1[, c('City','County','State', 'Make', 'Electric.Vehicle.Type','Model','VIN..1.10.','Model.Year')] <- 
  lapply(EV_1[, c('City','County','State', 'Make', 'Electric.Vehicle.Type','Model','VIN..1.10.','Model.Year')], factor)

str(EV_1)

# Convert factors to numerical 
EV_1[, c('City','County','State', 'Make', 'Electric.Vehicle.Type','Model','VIN..1.10.','Model.Year')] <- 
  lapply(EV_1[, c('City','County','State', 'Make', 'Electric.Vehicle.Type','Model','VIN..1.10.','Model.Year')], as.numeric)

str(EV_1)

#download the filtered file in CSV format
write.csv(EV_1, "EV_1.csv", row.names = FALSE)

#3.4)you can check for missing values
colSums(is.na(EV_1)) 

#3.5)Checking the distribution of EV cars
install.packages("viridis")
library(viridis)
library(ggplot2)

# Set the size of the plot
options(repr.plot.width=11, repr.plot.height=5)

# Create the count plot
ggplot(EV_1, aes(x = `Model.Year`, fill = `Model.Year`)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Distribution of Cars in Various Years",
       x = "Model Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)  # Hide the legend
#EV vehicles distribution is increasing as per the graph 
#year 2024 is low because almost half of the year is still left

#4) EDA and feature selection(which is most common value in the list, do the values of a varaible vary a lot 
# are there any strange values like outliers)
#4.1)Descriptive analysis
install.packages("psych")
library(psych)
describe(EV_1)

#4.2)As postal code, state has kurtosis value high , we are omiting those columns
library(dplyr)
str(EV_1)

# Identify columns with high kurtosis values
high_kurtosis_columns <- c("Postal.Code", "State")  

# Remove columns with high kurtosis values from the dataset
EV_1 <- EV_1 %>%
  select(-one_of(high_kurtosis_columns))

#4.3)electric range has right skewness 
# plotted graph to show the right skewness of electric.range
library(ggplot2)    # For advanced plotting
ggplot(EV_1, aes(x = Electric.Range)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Electric Range",
       x = "Electric Range",
       y = "Density") +
  theme_minimal()

#4.4)plotting pie chart for EVT
#In Electric Vehicle Type more than 83% vehicles are BEV i.e Battery Electric Vehicle 
#and 16% vehicles are PHEV i.e Plug-in Hybrid Electric Vehicle
type_counts <- table(EV_1$Electric.Vehicle.Type)

# Convert the table to a data frame
type_counts_df <- as.data.frame(type_counts)

# Rename the columns for clarity
colnames(type_counts_df) <- c("Electric.Vehicle.Type", "Count")

# Sort the data frame by count (descending order)
type_counts_df <- type_counts_df[order(-type_counts_df$Count), ]
type_counts_df$Electric.Vehicle.Type <- ifelse(type_counts_df$Electric.Vehicle.Type == 1, "BEV", "PHEV")

# Calculate percentage values
type_counts_df$Percentage <- type_counts_df$Count / sum(type_counts_df$Count) * 100

ggplot(type_counts_df, aes(x = "", y = Percentage, fill = Electric.Vehicle.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Electric Vehicle Types") +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste(Electric.Vehicle.Type, " (", round(Percentage, 1), "%)", sep = "")), 
            position = position_stack(vjust = 0.5))

#4.5) BEV  and PHEV consists of vehicles which 
#are Clean Alternative Fuel Vehicle Eligible and also vehicles whose eligibility 
#is unknown as battery range has not been researched.

#Set the size of the plot
options(repr.plot.width=30, repr.plot.height=20)

ggplot(EV_1, aes(x = `Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#4.6)Identifying the outliers

# Select the columns you want to create boxplots 
columns_to_plot_EV <- c("City","County","Model.Year","Make","Electric.Vehicle.Type","Model")

# Create boxplots using ggplot2
boxplot_data_EV <- EV_1[, columns_to_plot_EV,drop=FALSE]

# Reshape data for ggplot2
boxplot_data_long_EV <- reshape2::melt(boxplot_data_EV)

# Create boxplot
ggplot(boxplot_data_long_EV, aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot for Specific Columns",
       x = "Columns",
       y = "Values")

#As we can see the Model, EV type, County have some outliers 
#but can't remove them because they are considered to considrable outliers

#4.8)Handling outliers
#4.8.1)Z score method
# Calculate Z-scores for each numeric column
z_scores_EV <- scale(EV_1[, sapply(EV_1, is.numeric)])

# Define threshold for outlier detection (typically z-score > 3 or < -3)
threshold <- 3

# Identify rows with z-scores greater than the threshold in any column
outliers_EV <- apply(abs(z_scores_EV) > threshold, 1, any)

# Remove outliers from the dataset
EV_2 <- EV_1[!outliers_EV, ]

# Check the dimensions of the dataset after removing outliers
dim(EV_2) # could see 390 rows have been removed

#4.8.1)Cooks distance
model_EV <- lm(Electric.Range ~ Model.Year + Make + Model + Electric.Vehicle.Type, data = EV_2)

# Calculate Cook's distance
cooksd_EV <- cooks.distance(model_EV)

# Visualize Cook's distance
library(ggplot2)
cooksd_df <- data.frame(Observation = 1:length(cooksd_EV), CooksDistance = cooksd_EV)
ggplot(cooksd_df, aes(x = Observation, y = CooksDistance)) +
  geom_point(color = "skyblue", size = 2) +
  geom_hline(yintercept = 4/length(cooksd_EV), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance Plot", 
       x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()

# Identify influential observations
influential_obs <- cooksd_EV > 4/length(cooksd_EV)

# Print influential observations
print(paste("Number of influential observations:", sum(influential_obs)))
#"Number of influential observations: 9370"

# influential observations are removed
EV_3 <- EV_2[!influential_obs, ]

str(EV_3) #86375 obs. of  9 variables

#4.9)Correlation coefficient and collinearity
#taking only numerical variables
EV_3<- EV_3[, sapply(EV_3, is.numeric)]

# Compute correlation matrix for numeric variables
EV_3_cor <- cor(EV_3)
print(EV_3_cor) #Electric.Vehicle.Type and Electric.Range have very high correlation coefficients (close to 1) with each other.
#Hence Range is removed to avoid redundancy.

# Remove the "Electric.Range" column from the dataset
EV_4 <- EV_3[, !names(EV_3) %in% "Electric.Range"]

# Print the structure of the modified dataset
str(EV_4)

# Write the modified dataset to a CSV file
EV_4<-read.csv(file.choose())
write.csv(EV_4, "EV_4.csv", row.names = FALSE)

#4.10)Normalaisation 
# Normalization using the scale() function
normalized_data <- as.data.frame(scale(EV_4))

# Print the normalized data
print(normalized_data)

#5.1)Train Test Split
library(caret)

# Remove the "Electric Vehicle Type" column from the dataset
x <- EV_4[, !names(EV_4) %in% "Electric.Vehicle.Type"]

# Extract the "Electric Vehicle Type" column as the target variable
y <- EV_4$`Electric.Vehicle.Type`

# Set the seed for reproducibility
set.seed(100)

# Split the data into training and testing sets
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
x_train <- x[train_indices, ]
x_test <- x[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Standardize the training and testing data
x_train <- scale(x_train)
x_test <- scale(x_test)

# Get the dimensions of x_train
dim(x_train)

# Get the dimensions of x_test
dim(x_test)

#5.2) KNN method
library(class)

# Create the KNN classifier
knn_EV <- knn(train = x_train, test = x_test, cl = y_train)

# Fit the KNN model and make predictions
y_pred_EV <- knn(train = x_train, test = x_test, cl = y_train)

# Calculate accuracy score
accuracy <- mean(y_pred_EV == y_test)
accuracy # is 0.99

# Evaluate performance
conf_matrix_EV <- table(y_test, y_pred_EV)
str(conf_matrix_EV)

#summary including accuracy, sensitivity, specificity, and other performance metric
summary <- confusionMatrix(conf_matrix_EV)
print(summary) #Kappa : 0.9316

precision <- conf_matrix_EV[2, 2] / sum(conf_matrix_EV[, 2])
recall <- conf_matrix_EV[2, 2] / sum(conf_matrix_EV[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print evaluation metrics
cat("Accuracy:", accuracy, "\n") #0.991201 
cat("Precision:", precision, "\n") #0.9342618 
cat("Recall:", recall, "\n") #0.9384443
cat("F1 Score:", f1_score, "\n") #0.9363484

#This is an excellent result, need to check if its balanaced or imbalanced

#5.3) Adaboost
# Install and load the 'ada' package
install.packages("ada")
library(ada)

# Create the AdaBoost classifier
ada <- ada(y_train ~ ., data = as.data.frame(x_train))

# Make predictions with the AdaBoost model
y_pred_ada <- predict(ada, newdata = as.data.frame(x_test))

# Calculate accuracy score
accuracy_ada <- mean(y_pred_ada == y_test)
accuracy_ada # 0.9885381

# Evaluate performance
conf_matrix_ada <- table(y_test, y_pred_ada)
str(conf_matrix_ada)

#summary including accuracy, sensitivity, specificity, and other performance metric
summary_ada <- confusionMatrix(conf_matrix_ada)
print(summary_ada) #0.9134   kappa  

# Convert y_test and y_pred to numeric
y_test_numeric <- as.numeric(as.character(y_test))
y_pred_numeric <- as.numeric(as.character(y_pred_ada))

# Calculate Mean Absolute Percentage Error (MAPE)
MAPE_ada <- mean(abs((y_test_numeric - y_pred_numeric) / y_test_numeric)) * 100
MAPE_ada #0.9725224


#5.4) Decision Tree
library(rpart)

# Create the decision tree classifier
dtc_EV <- rpart(y_train ~ ., data = as.data.frame(x_train), method = "class",
             control = rpart.control(cp = 0, maxdepth = 7))


# Make predictions with the decision tree model
y_pred_dtc <- predict(dtc_EV, newdata = as.data.frame(x_test), type = "class")

# Calculate accuracy score
accuracy_dtc <- mean(y_pred_dtc == y_test)
accuracy_dtc # is 0.9896

# Evaluate performance
conf_matrix_dtc <- table(y_test, y_pred_dtc)
str(conf_matrix_dtc)

#summary including accuracy, sensitivity, specificity, and other performance metric
summary_dtc <- confusionMatrix(conf_matrix_dtc)
print(summary_dtc) #0.921 kappa

y_test_numeric <- as.numeric(as.character(y_test))
y_pred_numeric <- as.numeric(as.character(y_pred_dtc))
MAPE_dtc <- mean(abs((y_test_numeric - y_pred_numeric) / y_test_numeric)) * 100
MAPE_dtc # 0.8663939

#6)Evalutaion

#KNN has 
#accuracy(0.9912),
#kappa value(0.9316),
#sensitivity(0.9954), 
#Precision(0.9342618) ,
#Recall(0.9384443) and
#F1 Score(0.9363484)

#Adaboost  has 
#accuracy(0.9885), 
#kappa value(0.9314),
#sensitivity(0.9963), 
#MAPE(0.9725224)

#decision tree has 
#accuracy(0.9896), 
#kappa value(0.921),
#sensitivity(0.9963) , 
#MAPE(0.8663939)

library(ggplot2)

# Create a data frame for the model performance
model_names <- c("KNN", "Adaboost", "Decision Tree")
accuracy_values <- c(0.9912, 0.9885, 0.9896)
kappa_values <- c(0.9316, 0.9314, 0.921)
sensitivity_values <- c(0.9954, 0.9963, 0.9963)

performance_df <- data.frame(Model = model_names,
                             Accuracy = accuracy_values,
                             Kappa = kappa_values,
                             Sensitivity = sensitivity_values)

# Melt the data for visualization
library(reshape2)
performance_melted <- melt(performance_df, id.vars = "Model")

# Plot
ggplot(performance_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performances of EV",
       y = "Value",
       fill = "Metric") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +  # Blue, orange, green colors
  theme_minimal()


#KNN performs marginally
#better than the other models in terms of accuracy, kappa value, and sensitivity.
