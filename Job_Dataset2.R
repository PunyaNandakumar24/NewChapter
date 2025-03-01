#Crisp DM method
#1) Business Understanding 
#Goal:A Data Analytics Approach to analyze job 
#postings and trends to identify in-demand skills, industries, and job titles

#2)Data Understanding
#2.1)Data Aquisition

#read the file manually(use)
Jobs<-read.csv(file.choose())
str(Jobs)
Skill<-read.csv(file.choose())
str(Skill)

#2.2)Data and Datasets exploration(Explaining the types of variables and data set structure)

sapply(Jobs, class) #explain
sapply(Skill, class)

#3) DATA Pre-Processing

#3.1)Filtering of the data 
library(dplyr) #dplyr used for data manipulation
#filter based on country 

Job_us<- Jobs %>% filter(search_country %in% c('United States'))

Job_us<- Job_us[1:20000, ]

Skill<-Skill[1:20000, ]

#combining 2 data sets where it was 20k objects with 16 variables
CombinedSet <- cbind(Job_us, Skill)

#3.2)you can check for missing values
colSums(is.na(CombinedSet)) # no missing values

#3.3) check if any rows are duplicated
duplicate_rows <- CombinedSet[duplicated(CombinedSet), ]
duplicate_rows# no duplicated rows

#3.4)Duplicate columns
# Check for duplicate column names
duplicate_cols <- names(CombinedSet)[duplicated(names(CombinedSet))]

if (length(duplicate_cols) > 0) {
  print(paste("Duplicate columns found:", paste(duplicate_cols, collapse = ", ")))
} else {
  print("No duplicate columns found.")
}

# Remove duplicate columns
CombinedSet_unique <- CombinedSet[!duplicated(names(CombinedSet))]

write.csv(CombinedSet_unique, "Combinedset_unique.csv", row.names = FALSE)

#3.4)explore and visualize top 15 job titles
library(ggplot2)

# Create a data frame with top job titles and their frequencies
top_job_titles <- head(sort(table(CombinedSet_unique$job_title), decreasing = TRUE), 15)
job_df <- data.frame(job_title = names(top_job_titles), frequency = as.numeric(top_job_titles))

# Print the top 15 job titles
print(job_df)

# Plot
ggplot(job_df, aes(x = reorder(job_title, -frequency), y = frequency, fill = job_title)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # You can choose a different color palette
  labs(title = "Top 15 Job Titles",
       x = "Job Titles",
       y = "Number of Job Listings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none") +
  coord_flip()

#3.5)Explore and visualize top 15 companies
# Get the top 15 companies by count
top_companies <- head(names(sort(table(CombinedSet_unique$company), decreasing = TRUE)), 15)

# Subset the data to include only the top companies
df_top_companies <- subset(CombinedSet_unique, company %in% top_companies)

# Plot
ggplot(df_top_companies, aes(x = reorder(company, -table(company)[company]))) +
  geom_bar(aes(fill = company), width = 0.5) +  # Use fill aesthetic here
  theme_minimal() +
  labs(title = "Top 15 Companies",
       x = "Companies",
       y = "Number of Job Listings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the top 15 companies
print(df_top_companies)

#3.6)Explore job levels
job_level_distribution <- table(CombinedSet_unique$job_level)
# Calculate percentages
percentages <- prop.table(job_level_distribution) * 100

# Create pie chart with percentages
pie(job_level_distribution, 
    labels = paste(names(job_level_distribution), ": ", round(percentages, 1), "%", sep = ""), 
    main = "Distribution of Job Levels", 
    col = rainbow(length(job_level_distribution)))

#3.7)top 10 skills
# Combine all job_skills into a single string
Skills_1 <- tolower(paste(CombinedSet_unique$job_skills, collapse = ","))

# Split the combined string into a vector of individual skills
Skills_2 <- strsplit(Skills_1, ", ")[[1]]

# Print the first 10 skills and the total number of skills
print(head(Skills_2, 10))
print(length(Skills_2))

# Create a table of skill counts
skill_counts <- table(Skills_2)

# Get the most common 15 skills
common_skills <- head(sort(skill_counts, decreasing = TRUE), 15)

# Print the common skills
print(common_skills)

# Convert common_skills to a data frame for plotting
common_skills_df <- as.data.frame(common_skills)

# Rename the columns
colnames(common_skills_df) <- c("Skill", "Count")

# Plot the bar chart
library(ggplot2)
ggplot(common_skills_df, aes(x = reorder(Skill, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 15 Required Skills",
       x = "Skills",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  theme_minimal()

#3.7)Convert all columns to factors

CombinedSet_uniqueA  <- CombinedSet_unique
CombinedSet_uniqueA[] <- lapply(CombinedSet_uniqueA, factor)
str(CombinedSet_uniqueA)

# Convert factor variables to numeric
CombinedSet_uniqueB  <- CombinedSet_uniqueA
CombinedSet_uniqueB[] <- lapply(CombinedSet_uniqueB, as.numeric)
str(CombinedSet_uniqueB)

#to check the class or type of variable
sapply(CombinedSet_uniqueB,class)

summary(CombinedSet_uniqueB)

#4) EDA and feature selection(which is most common value in the list, 
#do the values of a varaible vary a lot 
# are there any strange values like outliers)
#4.1)Descriptive analysis
install.packages("psych")
library(psych)
describe(CombinedSet_uniqueB)

#proceeding to ignore  got_summary,got_ner,is_being_worked,search_country they do not provide relevant 
#information or do not contribute to the specific analysis you are conducting

DatasetJOB <- subset(CombinedSet_uniqueB, select = 
                       -c(got_summary, got_ner, is_being_worked,search_country,last_processed_time,first_seen))

describe(DatasetJOB)

#4.2)job_type,job_level has positive kurtosis
#a kurtosis value of 224.18 suggests that the distribution of job types is highly peaked with heavy tails,
#meaning there are some extreme values or outliers present in the data. 

library(e1071)

# Calculate kurtosis for job_level

job_level_kurtosis <- kurtosis(DatasetJOB$job_level)

# Create a data frame with the kurtosis values
kurtosis_df <- data.frame(job_level = "job_level", kurtosis = job_level_kurtosis)

# Plot kurtosis
ggplot(kurtosis_df, aes(x = job_level, y = kurtosis)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Kurtosis for job_level", x = "Variable", y = "Kurtosis") +
  theme_minimal()


#4.3)Identifying outliers:
# Selecting whole data set to check for outliers and Set the size of the plot
options(repr.plot.width=10, repr.plot.height=6)

# Create boxplots using ggplot2
outlier_job <- DatasetJOB

# Reshape data for ggplot2
boxplot_data_long_job <- reshape2::melt(outlier_job)

# Create boxplot
library(ggplot2)
ggplot(boxplot_data_long_job, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot for Each Column",
       x = "Column",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#as job_level and job_type has postive kurtosis and outliers
#we are proceeding to ignore the colummns as they are not 
#contributing much to the model

JOBfinal <- subset(DatasetJOB, select = 
                     -c(job_type,job_level))

str(JOBfinal)

#4.4)Cooks distance
Model_job <- lm(job_skills ~ job_title + company + search_city + search_position, data = JOBfinal)

# Calculate Cook's distance
cooksd_job <- cooks.distance(Model_job)

# Visualize Cook's distance
library(ggplot2)
cooksd_df_job <- data.frame(Observation = 1:length(cooksd_job), CooksDistance = cooksd_job)
ggplot(cooksd_df_job, aes(x = Observation, y = CooksDistance)) +
  geom_point(color = "skyblue", size = 2) +
  geom_hline(yintercept = 4/length(cooksd_job), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance Plot", 
       x = "Observation Index", y = "Cook's Distance") +
  theme_minimal()

# Identify influential observations
influential_obs_job <- cooksd_job > 4/length(cooksd_job)

# Print influential observations
print(paste("Number of influential observations:", sum(influential_obs_job)))
#"Number of influential observations: 263"

# Dremove influential observations
JOB_Finaldata <- JOBfinal[!influential_obs_job, ]

str(JOB_Finaldata) #19737 obs. of  7 variables:

#4.5)Correlation coefficient and collinearity

# Compute correlation matrix for numeric variables
JOB_Finaldata_cor <- cor(JOB_Finaldata)
JOB_Finaldata_cor #Job Link and Job Title have a very strong positive correlation of approximately 0.98.
#Hence job_link is removed

# Remove the "job_link " column from the dataset
JOB_Finaldata <- JOB_Finaldata[, !names(JOB_Finaldata) %in% "job_link"]

# Print the structure of the modified dataset
str(JOB_Finaldata)#19737 obs. of  6 variables:

# Write the modified dataset to a CSV file
JOB_Finaldata<-read.csv(file.choose())
write.csv(JOB_Finaldata, "JOB_Finaldata_for test.csv", row.names = FALSE)

#5)Model evaluation
#5.1)Train Test Split
library(caret)
# Remove the "job skills" column from the dataset
x <- JOB_Finaldata[, !names(JOB_Finaldata) %in% "job_skills"]

# Extract the "job skills" column as the target variable
y <- JOB_Finaldata$`job_skills`

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

#Linear Regression
# Load the required libraries
library(caret)

# Convert y_train to numeric if it's a factor
if (is.factor(y_train)) {
  y_train <- as.numeric(as.character(y_train))
}


# Train the linear regression model
lm_model <- lm(y_train ~ ., data = data.frame(y_train, x_train))


# Predict on the test data
lm_predictions <- predict(lm_model, newdata = data.frame(x_test))

# Evaluate the performance of the model
lm_rmse <- sqrt(mean((lm_predictions - y_test)^2))  # RMSE (Root Mean Squared Error)
lm_rmse #5693.027

# Calculate MAE
lm_mae <- mean(abs(lm_predictions - y_test))
lm_mae #4937.741


# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv",    # Cross-validation method
                     number = 10,      # Number of folds
                     verboseIter = TRUE)  # Print iteration progress
#Train the linear regression model with cross-validation
lm_model_cv <- train(x = x_train, 
                     y = y_train,
                     method = "lm",   # Linear Regression method
                     trControl = ctrl)

# Print the cross-validated results
print(lm_model_cv)

#  RMSE      Rsquared      MAE    
# 5706.545  0.0003985503  4933.803

#5.2)XGboost
# Load the required libraries
install.packages("xgboost")
library(xgboost)
library(caret)

# Split the data into predictors (X) and target variable (y)
X <- JOB_Finaldata[, !(names(JOB_Finaldata) %in% c("job_skills"))]
y <- JOB_Finaldata$job_skills

# Split the data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Convert the data into DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, nrounds = 10, verbose = FALSE)


# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror", # For regression tasks
  eval_metric = "rmse" # Root Mean Squared Error as evaluation metric
)

# Train the XGBoost model
xgb_model_job <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)

# Make predictions on the test set
predictions_job <- predict(xgb_model_job, dtest)

# Evaluate the model(XGBoost, accuracy isn't typically used as an evaluation metric)
RMSE <- sqrt(mean((predictions_job - y_test)^2))
RMSE # 5945.083

# Calculate MAE
MAE <- mean(abs(predictions_job - y_test))
MAE  #5100.182



#6) plotting performance model
library(ggplot2)

# Create the data frame
model_names <- c("XGBoost", "Linear Regression")
rmse_values <- c(RMSE, lm_rmse)
mae_values <- c(MAE, lm_mae)
performance_df <- data.frame(Model = model_names, RMSE = rmse_values, MAE = mae_values)

# Reshape the data for plotting
library(reshape2)
performance_melted <- melt(performance_df, id.vars = "Model")

# Plot
ggplot(performance_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison for JOB ",
       y = "Value",
       fill = "Metric") +
  scale_fill_manual(values = c("#0072B2", "#009E73")) +  # Blue and green colors
  theme_minimal()

#although the linear regression model has slightly better RMSE and MAE values,
#its low R-squared suggests that it may not capture the underlying patterns in the data effectively.
#Therefore, in this case, the XGBoost model may be considered better overall
#for predicting job skills based on the provided features.

