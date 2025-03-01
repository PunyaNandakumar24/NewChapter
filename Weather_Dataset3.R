#Crisp DM method
#1) Business Understanding 
#Goal:A Data Analytics Approach to weather details

#2)Data Understanding
#2.1)Data Aquisition

#read the file manually(use)
Weather<-read.csv(file.choose())
str(Weather)

#2.2)Data and Datasets exploration(Explaining the types of variables and data set structure)

sapply(Weather, class) #explain

#3) DATA Pre-Processing

#3.1)Filtering of the data 
library(dplyr) #dplyr used for data manipulation(use)

str(Weather)  # 36450 obs. of  41 variables

#3.2)you can check for missing values:(use)
colSums(is.na(Weather)) # no missing values

#3.3) check if any rows are duplicated:use
duplicate_rows <- Weather[duplicated(Weather), ]
duplicate_rows# no duplicated rows

#3.4)Duplicate columns
# Check for duplicate column names
duplicate_cols <- names(Weather)[duplicated(names(Weather))]
duplicate_cols #no

#3.5)gauge chart is a type of chart ("indicator") and sets various properties such as the initial value,
#mode (display mode), title, and gauge configuration.
# Load the required library
install.packages("plotly")
library(plotly)

# Create the gauge chart
gauge_chart <- plot_ly(
  type = "indicator",
  value = Weather$temperature_celsius[1],
  mode = "gauge+number",
  title = list(text = "Temperature (Celsius)"),
  gauge = list(
    axis = list(range = c(NULL, 40)),
    bar = list(color = "darkblue"),
    steps = list(
      list(range = c(0, 10), color = "lightgray"),
      list(range = c(10, 20), color = "gray")
    )
  )
)

# Show the gauge chart
gauge_chart

#3.6)Temperature map

# Create the scatter map plot
mapbox_plot <- plot_ly(Weather, 
                       lat = ~latitude,  # Latitude data
                       lon = ~longitude, # Longitude data
                       text = ~location_name,  # Hover text
                       color = ~temperature_celsius, # Color data
                       type = "scattermapbox") %>%
  layout(title = "Temperature Map",  # Chart title
         mapbox = list(
           style = "open-street-map"  # Mapbox style
         ))

# Show the map plot
mapbox_plot


#3.7)Temperature and humidity
# Create the scatter plot
scatter_plot <- plot_ly(Weather, 
                        x = ~temperature_celsius,   # X-axis: Temperature
                        y = ~humidity,             # Y-axis: Humidity
                        color = ~country,          # Color by country
                        type = "scatter",          # Scatter plot type
                        mode = "markers") %>%
  layout(title = "Temperature vs Humidity")  # Chart title

# Show the scatter plot
scatter_plot

#3.8)top and bottom 10 cities with max and min temperature
# Create a list of all unique city names
cities <- unique(Weather$location_name)

# Initialize lists to store the maximum and minimum temperatures for each city
max_temperatures <- list()
min_temperatures <- list()

# Iterate through the cities and calculate the maximum and minimum temperatures
for (city in cities) {
  city_data <- subset(Weather, location_name == city)
  max_temp <- max(city_data$temperature_celsius, na.rm = TRUE)
  min_temp <- min(city_data$temperature_celsius, na.rm = TRUE)
  max_temperatures[[city]] <- max_temp
  min_temperatures[[city]] <- min_temp
}

# Sort the cities by maximum and minimum temperatures
top_10_max <- sort(unlist(max_temperatures), decreasing = TRUE)[1:10]
bottom_10_min <- sort(unlist(min_temperatures))[1:10]

# Create data frames for top 10 maximum and bottom 10 minimum temperatures
top_10_max_df <- data.frame(City = names(top_10_max), Temperature = top_10_max)
bottom_10_min_df <- data.frame(City = names(bottom_10_min), Temperature = bottom_10_min)

# Define custom colors for the bars
top_chart_color <- 'rgb(75, 136, 230)'   # Blue
bottom_chart_color <- 'rgb(255, 87, 51)'  # Red

# Create bar chart function
create_bar_chart <- function(Weather, title, color) {
  fig <- plot_ly(Weather, x = ~City, y = ~Temperature, type = 'bar', marker = list(color = color, line = list(color = 'black', width = 1))) %>%
    layout(title = title, xaxis = list(categoryorder = 'total ascending'), showlegend = FALSE)
  return(fig)
}

# Create interactive bar charts for the top 10 and bottom 10 cities with custom colors
top_chart <- create_bar_chart(top_10_max_df, 'Top 10 Cities with maximum temperature', top_chart_color)
top_chart
bottom_chart <- create_bar_chart(bottom_10_min_df, 'Bottom 10 Cities with minimum temperature ', bottom_chart_color)
bottom_chart

#3.9)The scatter plot visualizes the relationship between two variables: "visibility_km" 
#on the x-axis and "air_quality_PM2.5" on the y-axis.
#Load the required library
library(ggplot2)

# Create the scatter plot
scatter_plot <- ggplot(Weather, aes(x = visibility_km, y = air_quality_PM2.5)) +
  geom_point() +  # Add points
  labs(title = "Visibility vs Air Quality", x = "Visibility (km)", y = "Air Quality (PM2.5)")

# Print the scatter plot
print(scatter_plot)


#4) EDA and feature selection(which is most common value in the list, 
#do the values of a varaible vary a lot 
# are there any strange values like outliers)
#4.1)Descriptive analysis
install.packages("psych")
library(psych)

## removing multiple columns as they do not take part in my research question or they do not provide relevant 
#information or do not contribute to the specific analysis you are conducting
Weather<- Weather[,!names(Weather) %in% c("last_updated_epoch","last_updated","wind_mph","wind_kph","wind_degree","wind_direction","pressure_mb",
"pressure_in","sunrise","sunset","moonrise","moonset","moon_phase","moon_illumination","precip_mm","precip_in","feels_like_celsius","feels_like_fahrenheit")]


describe(Weather)

#4.2)air_quality_Carbon_Monoxide,air_quality_Sulphur_dioxide,air_quality_PM2.5,air_quality_PM10 has the heaviest tailed kurtosis
#meaning there are some extreme values or outliers present in the data. 

library(e1071)

# Calculate kurtosis for air_quality_Carbon_Monoxide

Weather_kurtosis <- kurtosis(Weather$air_quality_Carbon_Monoxide)

# Create a data frame with the kurtosis values
kurtosis_df <- data.frame(air_quality_Carbon_Monoxide = "air_quality_Carbon_Monoxide", kurtosis = Weather_kurtosis)

# Plot kurtosis
ggplot(kurtosis_df, aes(x = air_quality_Carbon_Monoxide, y = kurtosis)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Kurtosis for air_quality_Carbon_Monoxide", x = "Variable", y = "Kurtosis") +
  theme_minimal()

Weather<- Weather[,!names(Weather) %in% c("air_quality_Carbon_Monoxide","air_quality_Sulphur_dioxide","air_quality_PM2.5"
,"air_quality_PM10")]
str(Weather)


#4.3)identify  outliers:
# Selecting whole data set to check for outliers

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=6)

# Create boxplots using ggplot2
outlier <- Weather

# Reshape data for ggplot2
boxplot_data_long <- reshape2::melt(outlier)

# Create boxplot
library(ggplot2)
ggplot(boxplot_data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot for Each Column of weather",
       x = "Column",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#4.4)ignoring the character variables as they are not contributing much and have outliers
library(dplyr)

# Select only numeric variables
Weather_subset <- Weather %>%
  select(where(is.numeric))

# Convert all non-numeric columns to numeric
Weather_subset <- Weather_subset %>%
  mutate_if(~ !is.numeric(.), as.numeric)

# Convert integer columns to numeric
Weather_subset<- Weather_subset %>%
  mutate_if(is.integer, as.numeric)

# View the structure of the new subset
str(Weather_subset)

Weath <- Weather_subset

#4.5) Handling outliers
#4.5.1) Calculate z-scores for each numerical column
z_scores <- scale(Weath[, sapply(Weath, is.numeric)])

# Define threshold for outlier detection
threshold <- 3

# Identify rows with z-scores greater than the threshold
outliers <- apply(abs(z_scores) > threshold, 1, any)

# Remove outliers from the dataset
Weather_filtered <- Weath[!outliers, ]

#4.5.2)# Calculate the interquartile range (IQR) for each numeric variable
Q1 <- apply(Weather_filtered, 2, quantile, probs = 0.25)
Q3 <- apply(Weather_filtered, 2, quantile, probs = 0.75)
IQR_values <- Q3 - Q1

# Define the multiplier to determine the range for outliers
multiplier <- 1.5

# Determine the lower and upper bounds for outliers
lower_bounds <- Q1 - multiplier * IQR_values
upper_bounds <- Q3 + multiplier * IQR_values

# Identify outliers based on the bounds
outliers_IQR <- sapply(1:ncol(Weather_filtered), function(i) {
  outliers_lower <- Weather_filtered[, i] < lower_bounds[i]
  outliers_upper <- Weather_filtered[, i] > upper_bounds[i]
  outliers_lower | outliers_upper
})

# Remove outliers from the dataset
Weather_no_outliers <- Weather_filtered[!apply(outliers_IQR, 1, any), ]

str(Weather_no_outliers) #16259 obs. of  15 variables:
Weather_Final <- Weather_no_outliers

#4.6)Correlation coefficient and collinearity

install.packages("psych")
library(psych)
pairs.panels(Weather_Final[,c(2,3,6,7,8,9)]) 

# Compute correlation matrix for numeric variables
Weather_Final_cor <- cor(Weather_Final)
Weather_Final_cor

#values on digaonal reprsent hsitogarm of each variable
#Top right say about corelation 
#LBotton left say about colinearity

#5)Model evaluation
#5.1)SVM
# Load the required library
install.packages("e1071")
library(e1071)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(Weather_Final), 0.7 * nrow(Weather_Final))  # 70% train, 30% test
train_data <- Weather_Final[train_index, ]
test_data <- Weather_Final[-train_index, ]

# Fit the SVM model
svm_model <- svm(temperature_celsius ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)

# Make predictions on the test set
predictions <- predict(svm_model, test_data)

# Evaluate the model
rmse <- sqrt(mean((test_data$temperature_celsius - predictions)^2))
r_squared <- cor(test_data$temperature_celsius, predictions)^2

# Print the evaluation metrics
print(paste("RMSE:", rmse)) #[1] "RMSE: 9.50345233576514"
print(paste("R-squared:", r_squared)) #[1] "R-squared: 0.0620943839093147"

#5.2)Gradient boost
install.packages("xgboost")
library(xgboost)

# Example assuming your target variable is 'temperature_celsius'
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -which(names(train_data) == "temperature_celsius")]), 
                      label = train_data$temperature_celsius)

# Example with default parameters
xgb_model <- xgboost(data = dtrain, nrounds = 10)

# Example predicting on test data
dtest <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "temperature_celsius")]))
predictions <- predict(xgb_model, dtest)

# Calculate RMSE
rmse_gb <- sqrt(mean((test_data$temperature_celsius - predictions)^2))

# Calculate R-squared
r_squared_gb <- cor(test_data$temperature_celsius, predictions)^2

# Print evaluation metrics
print(paste("RMSE:", rmse_gb)) 
print(paste("R-squared:", r_squared_gb))


#6) plotting performance model
# Define model names and performance metrics
model_names <- c("SVM", "Gradient Boost")
rmse_values <- c(9.503, 0.562)  # Update with your actual RMSE values
r_squared_values <- c(0.0621, 0.9999)  # Update with your actual R-squared values

# Create a data frame with model names and performance metrics
performance_df <- data.frame(Model = model_names, RMSE = rmse_values, R_squared = r_squared_values)

# Plot RMSE and R-squared
library(ggplot2)

# Melt the data frame for plotting
library(reshape2)
melted_performance <- melt(performance_df, id.vars = "Model")

# Plot
ggplot(melted_performance, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison for Weather",
       y = "Value", fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("RMSE" = "skyblue", "R_squared" = "orange"))


#Gradient Boost model is better in terms of both RMSE and R-squared values.