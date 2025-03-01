# Load necessary libraries
library(ggplot2)
library(lubridate)
library(caret)
library(reshape2)
library(cluster)
library(factoextra)
library(tidyverse)

# 1) Business Understanding
# Goal: Customer segmentation on gardening business

# 2) Data Understanding
## 2.1) Data Acquisition
mdg <- read.csv(file.choose())

## 2.2) Data Exploration
sapply(mdg, class)  # Check column names and data types
str(mdg)  # Structure of the dataset (e.g., 6569 obs. of 13 variables)

# 3) Data Pre-Processing
#3.1)Convert 'Date' to Date format using explicit format
mdg$Date <- as.Date(mdg$Date, format = "%d/%m/%y")

# Check if the conversion was successful
print(head(mdg$Date))

## 3.2) Data Visualization

# 3.2.1) Aggregate sales by Date
sales_trend <- mdg %>%
  group_by(Date) %>%
  summarize(Total_Value = sum(Total.value.of.sale), .groups = 'drop')

sales_trend$Date <- as.Date(sales_trend$Date, format = "%Y-%m-%d")

# Plot Sales Trend Over Time
ggplot(sales_trend, aes(x = Date, y = Total_Value)) +
  geom_line(color = "blue") +  # Create a line plot
  labs(title = "Sales Trend Over Time", x = "Date", y = "Total Sales Value") +
  theme_minimal()

# 3.2.2) Aggregate sales by month
sales_trend$Month <- floor_date(sales_trend$Date, "month")
monthly_sales <- sales_trend %>%
  group_by(Month) %>%
  summarize(Total_Value = sum(Total_Value), .groups = 'drop')

# Plot Monthly Sales Trend
ggplot(monthly_sales, aes(x = Month, y = Total_Value)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.2.3) Aggregate sales by year
sales_trend$Year <- year(sales_trend$Date)
yearly_sales <- sales_trend %>%
  group_by(Year) %>%
  summarize(Total_Value = sum(Total_Value), .groups = 'drop')

# Plot Yearly Sales Trend
ggplot(yearly_sales, aes(x = Year, y = Total_Value)) +
  geom_line(color = "blue") +
  labs(title = "Yearly Sales Trend", x = "Year", y = "Total Sales Value") +
  theme_minimal()

#3.2.4) Sales by Customer Age Group
mdg$Age_Group <- cut(mdg$Age, breaks = seq(0, 100, by = 10), right = FALSE,
                     labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-"))

age_sales <- mdg %>%
  group_by(Age_Group) %>%
  summarize(Total_Value = sum(Total.value.of.sale), .groups = 'drop')

ggplot(age_sales, aes(x = Age_Group, y = Total_Value, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Customer Age Group", x = "Age Group", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.2.5) Sales Distribution by Marital Status
marital_sales <- mdg %>%
  group_by(Marital.status) %>%
  summarize(Total_Value = sum(Total.value.of.sale), .groups = 'drop')

ggplot(marital_sales, aes(x = Marital.status, y = Total_Value, fill = Marital.status)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales Distribution by Marital Status", x = "Marital Status", y = "Total Sales Value") +
  theme_minimal()

#3.2.6) Top 10 Products by Total Quantity Sold
quantity_sold <- mdg %>%
  group_by(Product) %>%
  summarize(Total_Quantity = sum(Quantity.Sold), .groups = 'drop') %>%
  top_n(10, Total_Quantity) %>%
  arrange(desc(Total_Quantity))

ggplot(quantity_sold, aes(x = reorder(Product, Total_Quantity), y = Total_Quantity, fill = Product)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Total_Quantity), 
            hjust = -0.2, 
            color = "black", 
            size = 4, 
            fontface = "bold") +
  labs(title = "Top 10 Products by Total Quantity Sold", 
       x = NULL, 
       y = "Total Quantity Sold") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), # Remove labels from x-axis
        axis.ticks.x = element_blank(), # Remove ticks from x-axis
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(), # No title on y-axis
        axis.text.y = element_text(size = 10, face = "bold"), # Adjust left-side product names
        legend.position = "none", # Remove legend
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  scale_fill_brewer(palette = "Paired") +
  coord_flip()


#3.3.7) Total Sales by Location - Top 10 Locations
location_sales <- mdg %>%
  group_by(Location) %>%
  summarize(Total_Value = sum(Total.value.of.sale), .groups = 'drop') %>%
  slice_max(order_by = Total_Value, n = 10)

ggplot(location_sales, aes(x = reorder(Location, -Total_Value), y = Total_Value, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Total Sales by Location", x = "Location", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.3.8) Sales Value by Customer Type
customer_type_sales <- mdg %>%
  group_by(Type.of.Customer) %>%
  summarize(Total_Value = sum(Total.value.of.sale), .groups = 'drop')

ggplot(customer_type_sales, aes(x = reorder(Type.of.Customer, -Total_Value), y = Total_Value, fill = Type.of.Customer)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales Value by Type of Customer", x = "Type of Customer", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.3.9) Boxplot of Sales Value by Age Group
ggplot(mdg, aes(x = Age_Group, y = Total.value.of.sale, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Sales Value Distribution by Age Group", x = "Age Group", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3.2) Convert categorical variables to factors
New_mdg <- mdg %>%
  mutate(
    Marital.status = factor(Marital.status),
    Location = factor(Location),
    Type.of.Customer = factor(Type.of.Customer),
    Product = factor(Product),
    Preferred.Category = factor(Preferred.Category)
  )

# Check the structure of the dataset to confirm the changes
str(New_mdg)

## 3.3) Check for missing values
colSums(is.na(New_mdg))

## 3.4) Check for Duplicates
# Check for duplicate rows in the entire dataset
duplicates <- New_mdg[duplicated(New_mdg), ]
print(duplicates)

# Count the total number of duplicate rows
num_duplicates <- sum(duplicated(New_mdg))
print(num_duplicates) #8 rows removed

# Remove duplicate rows
New_mdg_unique <- New_mdg %>%
  distinct() #6569 observations to 6561 obervations

# 4) Outlier Detection and Removal
## Step 1: Calculate Z-scores
New_mdg_unique$Z_Score <- scale(New_mdg_unique$Total.value.of.sale)

## Identify outliers using Z-score (threshold > 3 or < -3)
mdg_no_z_outliers <- New_mdg_unique %>%
  dplyr::filter(abs(Z_Score) <= 3)

## Step 2: Calculate IQR
Q1 <- quantile(mdg_no_z_outliers$Total.value.of.sale, 0.25)
Q3 <- quantile(mdg_no_z_outliers$Total.value.of.sale, 0.75)
IQR_value <- IQR(mdg_no_z_outliers$Total.value.of.sale)

## Define outlier thresholds using IQR
lower_threshold <- Q1 - 1.5 * IQR_value
upper_threshold <- Q3 + 1.5 * IQR_value

## Step 3: Remove outliers using IQR
mdg_final <- mdg_no_z_outliers %>%
  dplyr::filter(Total.value.of.sale >= lower_threshold & Total.value.of.sale <= upper_threshold)

## Remove the Z_Score column as it's no longer needed
mdg_final <- mdg_final %>%
  select(-Z_Score)

# Print original and final row counts
original_row_count <- nrow(New_mdg_unique)
cat("Original row count before outlier detection: ", original_row_count, "\n")

row_count_after_z_score <- nrow(mdg_no_z_outliers)
cat("Row count after Z-score outlier detection: ", row_count_after_z_score, "\n")

final_row_count <- nrow(mdg_final)
cat("Row count after IQR-based outlier detection: ", final_row_count, "\n")

# Step 4: Save the cleaned dataset to a CSV file
output_file <- "cleaned_mdg_data.csv"
write.csv(mdg_final, file = output_file, row.names = FALSE)

# Boxplot of Sales Value by Age Group After Outlier Removal
ggplot(mdg_final, aes(x = Age_Group, y = Total.value.of.sale, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Sales Value Distribution by Age Group", x = "Age Group", y = "Total Sales Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5) Feature Normalization (Min-Max Scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to relevant numeric columns
mdg_final <- mdg_final %>%
  mutate(
    Total.value.of.sale = normalize(Total.value.of.sale),
    Price = normalize(Price),
    Quantity.Sold = normalize(Quantity.Sold)
  )

str(mdg_final)

# 6) RFM Analysis
#6.1) RFM calculation
rfm_data <- mdg_final %>%
  group_by(Customer.ID) %>%
  summarise(
    Recency = as.numeric(difftime(Sys.Date(), max(Date), units = "days")),
    Frequency = n(),
    Monetary = sum(Total.value.of.sale)
  )

# Define the output file name for RFM data
output_file_rfm <- "rfm_data.csv"
write.csv(rfm_data, file = output_file_rfm, row.names = FALSE)

#6.2)Score Customers based on RFM metrics
rfm_data_score <- rfm_data %>%
  mutate(
    R_Score = ntile(-Recency, 5),  # Lower recency is better (hence negative)
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = R_Score + F_Score + M_Score  # Total RFM score
  )

# Save the RFM data with scores
output_file_rfm_score <- "rfm_data_score.csv"
write.csv(rfm_data_score, file = output_file_rfm_score, row.names = FALSE)

#6.3) Segment Customers
rfm_data_score_segments <- rfm_data_score %>%
  mutate(
    Segment = case_when(
      RFM_Score >= 9 ~ "Best Customers",
      RFM_Score >= 6 ~ "Loyal Customers",
      RFM_Score >= 4 ~ "Potential Customers",
      TRUE ~ "At Risk Customers"
    )
  )

# Save the RFM segments data
output_file_rfm_segments <- "rfm_data_segments.csv"
write.csv(rfm_data_score_segments, file = output_file_rfm_segments, row.names = FALSE)

# Load necessary libraries for visualization
library(ggplot2)
library(dplyr)

#7.4) RFM visualisations
#7.4.1) Visualize RFM Segments
rfm_segment_counts <- rfm_data_score_segments %>%
  group_by(Segment) %>%
  summarise(Total_Customers = n(), .groups = 'drop')

# Bar plot for number of customers in each segment
ggplot(rfm_segment_counts, aes(x = Segment, y = Total_Customers, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Customers in Each RFM Segment", x = "Customer Segment", y = "Total Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7.4.2) Visualize Recency, Frequency, and Monetary by Segment
# Load the tidyr package
library(tidyr)

rfm_segment_metrics <- rfm_data_score_segments %>%
  group_by(Segment) %>%
  summarise(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = starts_with("Avg"), names_to = "Metric", values_to = "Value")

# Bar plot for average Recency, Frequency, and Monetary by Segment
ggplot(rfm_segment_metrics, aes(x = Segment, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RFM Metrics by Customer Segment", x = "Customer Segment", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7.4.3) Visualize RFM Score Distribution
ggplot(rfm_data_score, aes(x = RFM_Score)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of RFM Scores", x = "RFM Score", y = "Number of Customers") +
  theme_minimal()

#7.4.4) Visualize Average Monetary Value by Segment
ggplot(rfm_segment_metrics %>% filter(Metric == "Avg_Monetary"), aes(x = Segment, y = Value, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Monetary Value by Customer Segment", x = "Customer Segment", y = "Average Monetary Value") +
  theme_minimal()

#8) Feature selection for clustering process
# Perform PCA on the numeric features

numeric_features <- mdg_final %>% select(where(is.numeric))

pca_result <- prcomp(numeric_features, scale. = TRUE)

# Plot variance explained by each principal component
fviz_eig(pca_result)

# Choose components that capture at least 90% of the variance
pca_data <- as.data.frame(pca_result$x[, 1:which(cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) >= 0.9)[1]])

######Clustering models
#9) K means 

#9.1) Determine Optimal Number of Clusters

#9.1.a)Elbow method(The Elbow method evaluates the total within-cluster 
#sum of squares (WSS) for different values of k)

# Calculate WSS for a range of k values
wss <- sapply(1:10, function(k) {
  kmeans(pca_data, centers = k, nstart = 25)$tot.withinss
})

# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K")


#9.1.b)Silhouette Analysis (The silhouette width measures how similar each point is to 
#its own cluster compared to other clusters.)

# Calculate average silhouette width for different values of k
library(cluster)
avg_sil_width <- sapply(2:10, function(k) {
  km_res <- kmeans(pca_data, centers = k, nstart = 25)
  silhouette_score <- silhouette(km_res$cluster, dist(pca_data))
  mean(silhouette_score[, 3])
})

# Plot silhouette widths
plot(2:10, avg_sil_width, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal K")

#9.2) Run K-Means Clustering
# List to store clustering results and silhouette scores
cluster_results <- list()
silhouette_scores <- c()

# Range of K values to try based on Elbow and Silhouette analysis
k_values <- c(2, 3, 4, 6, 7)

# Loop over each K value and run K-means
for (k in k_values) {
  set.seed(123)  # for reproducibility
  kmeans_result <- kmeans(pca_data, centers = k, nstart = 25)
  
  # Store clustering result
  cluster_results[[as.character(k)]] <- kmeans_result
  
  # Calculate silhouette score
  library(cluster)
  silhouette_score <- silhouette(kmeans_result$cluster, dist(pca_data))
  avg_silhouette <- mean(silhouette_score[, 3])
  silhouette_scores <- c(silhouette_scores, avg_silhouette)
}

# Combine K values and silhouette scores into a data frame for easy viewing
results_df <- data.frame(
  K = k_values,
  Silhouette_Score = silhouette_scores
)
# Print results
print(results_df)

# Find the optimal K based on the highest silhouette score
optimal_k <- k_values[which.max(silhouette_scores)]
cat("Optimal K based on silhouette scores:", optimal_k, "\n")

# Run final K-means clustering with the optimal K
set.seed(123)
final_kmeans_result <- kmeans(pca_data, centers = optimal_k, nstart = 25)

# clusters if you want to visualize
library(factoextra)
fviz_cluster(final_kmeans_result, data = pca_data,
             geom = "point", ellipse.type = "convex",
             main = paste("K-means Clustering with K =", optimal_k))

#9.3) Davies-Bouldin
# Load necessary libraries
library(cluster)   # For silhouette calculation if needed
library(factoextra) # For WSS calculation

# Define custom Davies-Bouldin function
davies.bouldin <- function(data, cluster_labels) {
  clusters <- unique(cluster_labels)
  n_clusters <- length(clusters)
  
  # Calculate centroids for each cluster
  centroids <- lapply(clusters, function(c) colMeans(data[cluster_labels == c, , drop=FALSE]))
  # Calculate average distance within each cluster (cluster compactness)
  s <- sapply(clusters, function(c) {
    cluster_points <- data[cluster_labels == c, , drop=FALSE]
    mean(dist(cluster_points, method = "euclidean"))
  })
  
  db_index <- 0
  for (i in 1:n_clusters) {
    max_r <- 0
    for (j in 1:n_clusters) {
      if (i != j) {
        # Distance between centroids of clusters i and j
        d_ij <- dist(rbind(centroids[[i]], centroids[[j]]), method = "euclidean")[1]
        # Ratio of average distances within clusters to distance between clusters
        r_ij <- (s[i] + s[j]) / d_ij
        if (r_ij > max_r) max_r <- r_ij
      }
    }
    db_index <- db_index + max_r
  }
  db_index / n_clusters
}

# Initialize lists to store results
davies_bouldin_scores <- c()
wss_scores <- c()

# Range of K values to try based on analysis (Elbow and Silhouette)
k_values <- c(2, 3, 4, 6, 7)

# Loop over each K value and run K-means
for (k in k_values) {
  set.seed(123)  # for reproducibility
  kmeans_result <- kmeans(pca_data, centers = k, nstart = 25)
  
  # Calculate WSS for current K (within-cluster sum of squares)
  wss_scores <- c(wss_scores, kmeans_result$tot.withinss)
  
  # Calculate Davies-Bouldin index for current K
  db_index <- davies.bouldin(pca_data, kmeans_result$cluster)
  davies_bouldin_scores <- c(davies_bouldin_scores, db_index)
}

# Create the evaluation data frame with K, WSS, and DBI
evaluation_df <- data.frame(
  K = k_values,
  WSS = wss_scores,
  Davies_Bouldin_Index = davies_bouldin_scores
)

# Print the evaluation data frame
print(evaluation_df)

# Identify the optimal K based on the lowest Davies-Bouldin index
optimal_k_db <- k_values[which.min(davies_bouldin_scores)]
cat("Optimal K based on Davies-Bouldin index:", optimal_k_db, "\n")

#Optimal K based on Davies-Bouldin index: 6 

# Set the optimal K based on Davies-Bouldin index
optimal_k <- 6

# Run K-means clustering with the optimal K
set.seed(123)  # for reproducibility
final_kmeans_result <- kmeans(pca_data, centers = optimal_k, nstart = 25)

# Visualize the clustering result
library(factoextra)
fviz_cluster(final_kmeans_result, data = pca_data,
             geom = "point", ellipse.type = "convex",
             main = paste("K-means Clustering with K =", optimal_k))

#9.4) # Add cluster labels to original data
pca_data_with_clusters <- cbind(pca_data, Cluster = final_kmeans_result$cluster)

# Calculate mean values of each variable in original data per cluster
cluster_summary <- aggregate(. ~ Cluster, data = pca_data_with_clusters, FUN = mean)
print(cluster_summary)


#9.4) Evaluate Cluster Quality
# Assuming `final_kmeans_result` is the final clustering result with K = 6

# 1. Calculate Silhouette Scores for Each Cluster
library(cluster)

# Compute silhouette scores
silhouette_values <- silhouette(final_kmeans_result$cluster, dist(pca_data))
avg_silhouette_score <- mean(silhouette_values[, 3])  # Average silhouette score for all clusters

# Print the average silhouette score for overall evaluation
cat("Average Silhouette Score for K =", optimal_k, ":", avg_silhouette_score, "\n")

# 2. Plot Silhouette Width for Each Cluster
fviz_silhouette(silhouette_values) +
  labs(title = paste("Silhouette Plot for K =", optimal_k))

# This plot helps in visually assessing the cohesion and separation of each cluster. A higher silhouette width indicates better cluster quality.

# 3. Per-Cluster WSS (Within-Cluster Sum of Squares)
# Calculate WSS for each cluster
wss_per_cluster <- sapply(1:optimal_k, function(i) {
  sum(dist(pca_data[final_kmeans_result$cluster == i, ])^2) / 
    nrow(pca_data[final_kmeans_result$cluster == i, ])
})

# Print WSS for each cluster
cat("WSS for each cluster with K =", optimal_k, ":\n")
print(wss_per_cluster)

# 4. Recalculate Davies-Bouldin Index for Final Clustering (K = 6)
db_index_final <- davies.bouldin(pca_data, final_kmeans_result$cluster)
cat("Davies-Bouldin Index for final clustering (K =", optimal_k, "):", db_index_final, "\n")

# Summary of cluster quality metrics
quality_summary <- data.frame(
  Cluster = 1:optimal_k,
  WSS_Per_Cluster = wss_per_cluster,
  Silhouette_Width = silhouette_values[, 3][match(1:optimal_k, final_kmeans_result$cluster)]
)

print("Cluster Quality Summary")
print(quality_summary)

##1)Re-run K-means clustering for a different K (e.g., K = 4 or K = 5)
alternative_k <- 4  

# Run K-means clustering
set.seed(123)
alternative_kmeans_result <- kmeans(pca_data, centers = alternative_k, nstart = 25)

# Calculate Silhouette Score for new K
silhouette_values_alt <- silhouette(alternative_kmeans_result$cluster, dist(pca_data))
avg_silhouette_score_alt <- mean(silhouette_values_alt[, 3])
cat("Average Silhouette Score for K =", alternative_k, ":", avg_silhouette_score_alt, "\n")

# Calculate Davies-Bouldin Index for new K
db_index_alt <- davies.bouldin(pca_data, alternative_kmeans_result$cluster)
cat("Davies-Bouldin Index for final clustering (K =", alternative_k, "):", db_index_alt, "\n")

# Plot silhouette for new K
fviz_silhouette(silhouette_values_alt) +
  labs(title = paste("Silhouette Plot for K =", alternative_k))

# Calculate WSS for each cluster
wss_per_cluster_alt <- sapply(1:alternative_k, function(i) {
  sum(dist(pca_data[alternative_kmeans_result$cluster == i, ])^2) / 
    nrow(pca_data[alternative_kmeans_result$cluster == i, ])
})

# Print WSS per cluster
cat("WSS for each cluster with K =", alternative_k, ":\n")
print(wss_per_cluster_alt)


##2)Re-run K-means clustering for a different K (e.g., K = 4 or K = 5)
alternative_k <- 5

# Run K-means clustering
set.seed(123)
alternative_kmeans_result <- kmeans(pca_data, centers = alternative_k, nstart = 25)

# Calculate Silhouette Score for new K
silhouette_values_alt <- silhouette(alternative_kmeans_result$cluster, dist(pca_data))
avg_silhouette_score_alt <- mean(silhouette_values_alt[, 3])
cat("Average Silhouette Score for K =", alternative_k, ":", avg_silhouette_score_alt, "\n")

# Calculate Davies-Bouldin Index for new K
db_index_alt <- davies.bouldin(pca_data, alternative_kmeans_result$cluster)
cat("Davies-Bouldin Index for final clustering (K =", alternative_k, "):", db_index_alt, "\n")

# Plot silhouette for new K
fviz_silhouette(silhouette_values_alt) +
  labs(title = paste("Silhouette Plot for K =", alternative_k))

# Calculate WSS for each cluster
wss_per_cluster_alt <- sapply(1:alternative_k, function(i) {
  sum(dist(pca_data[alternative_kmeans_result$cluster == i, ])^2) / 
    nrow(pca_data[alternative_kmeans_result$cluster == i, ])
})

# Print WSS per cluster
cat("WSS for each cluster with K =", alternative_k, ":\n")
print(wss_per_cluster_alt)

#Code to Save and Use Final Clustering for k=6
#Add final cluster labels to the preprocessed data
final_data_with_clusters <- cbind(mdg_final, Cluster = final_kmeans_result$cluster)

# Save as a CSV for further analysis
write.csv(final_data_with_clusters, "final_clustering_results_K6.csv", row.names = FALSE)

str(final_data_with_clusters)

#9.5.2)Profiling Each Cluster
# Calculate RFM metrics if not already present
rfm_data <- mdg_final %>%
  group_by(Customer.ID) %>%
  summarise(
    Recency = as.numeric(difftime(Sys.Date(), max(Date), units = "days")),
    Frequency = n(),
    Monetary = sum(Total.value.of.sale)
  )

# Merge RFM metrics with final_data_with_clusters
final_data_with_clusters <- final_data_with_clusters %>%
  left_join(rfm_data, by = "Customer.ID")

# Now, calculate the mean or median for key features within each cluster
cluster_profile <- final_data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Age = mean(Age, na.rm = TRUE),
    Avg_Total_Sales_Value = mean(Total.value.of.sale, na.rm = TRUE),
    Avg_Quantity_Sold = mean(Quantity.Sold, na.rm = TRUE),
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_Monetary = mean(Monetary, na.rm = TRUE),
    Total_Customers = n()
  )

# Display the cluster profile
print(cluster_profile)

# Visualize cluster profiles for better interpretation
# Average Total Sales Value by Cluster
ggplot(cluster_profile, aes(x = as.factor(Cluster), y = Avg_Total_Sales_Value, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Total Sales Value by Cluster", x = "Cluster", y = "Average Sales Value") +
  theme_minimal()

# Average Age by Cluster
ggplot(cluster_profile, aes(x = as.factor(Cluster), y = Avg_Age, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Age by Cluster", x = "Cluster", y = "Average Age") +
  theme_minimal()

# Average Frequency by Cluster
ggplot(cluster_profile, aes(x = as.factor(Cluster), y = Avg_Frequency, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Frequency by Cluster", x = "Cluster", y = "Average Frequency") +
  theme_minimal()

# Average Monetary Value by Cluster
ggplot(cluster_profile, aes(x = as.factor(Cluster), y = Avg_Monetary, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Monetary Value by Cluster", x = "Cluster", y = "Average Monetary Value") +
  theme_minimal()

#10) Enhanced Clustering Analysis
#10.1) Experiment with Different Algorithms
# Load required libraries
library(cluster)      # For hierarchical clustering and silhouette calculation
library(dbscan)       # For DBSCAN
library(mclust)       # For Gaussian Mixture Models (GMM)
library(factoextra)   # For visualization
library(tidyverse)    # For data manipulation and visualization

#10.1.1) Hierarchical Clustering
# Compute dissimilarity matrix
dist_matrix <- dist(pca_data)

# Perform hierarchical clustering with Ward's method
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Cut tree into 6 clusters (for consistency with K-means)
hc_clusters <- cutree(hc_result, k = 6)

# Visualize dendrogram
plot(hc_result, main = "Dendrogram of Hierarchical Clustering", xlab = "", sub = "")
rect.hclust(hc_result, k = 6, border = "red")

# Add cluster labels to pca_data for analysis
pca_data$hc_cluster <- hc_clusters


#10.1.2) DBSCAN (Density-Based Spatial Clustering of Applications with Noise)
# Choose epsilon and minPts based on your data distribution (you may need to experiment with values)
dbscan_result <- dbscan(pca_data, eps = 0.5, minPts = 5)

# Plot DBSCAN clustering result
fviz_cluster(dbscan_result, data = pca_data, geom = "point", ellipse = FALSE, main = "DBSCAN Clustering")

# Add DBSCAN cluster labels to pca_data for analysis
pca_data$dbscan_cluster <- dbscan_result$cluster



#10.1.3) Gaussian Mixture Models (GMM)
# Fit GMM using Mclust package
gmm_result <- Mclust(pca_data, G = 6)  # G = 6 means we are trying to fit 6 components

# Plot GMM clustering result
fviz_mclust(gmm_result, "classification", geom = "point", main = "GMM Clustering")

# Add GMM cluster labels to pca_data for analysis
pca_data$gmm_cluster <- gmm_result$classification

#11) Evalation
#11.1) Calculation
# K-means Silhouette Score
kmeans_silhouette <- silhouette(kmeans_result$cluster, dist(pca_data))
avg_kmeans_sil <- mean(kmeans_silhouette[, 3])
cat("Average Silhouette Score for K-means:", avg_kmeans_sil, "\n")

# Hierarchical Clustering Silhouette Score
hc_silhouette <- silhouette(hc_clusters, dist(pca_data))
avg_hc_sil <- mean(hc_silhouette[, 3])
cat("Average Silhouette Score for Hierarchical Clustering:", avg_hc_sil, "\n")

# DBSCAN does not provide a silhouette score directly due to noise points; however, you can calculate it on clusters only
dbscan_silhouette <- silhouette(dbscan_result$cluster[dbscan_result$cluster > 0], dist(pca_data[dbscan_result$cluster > 0, ]))
avg_dbscan_sil <- mean(dbscan_silhouette[, 3], na.rm = TRUE)
cat("Average Silhouette Score for DBSCAN (excluding noise):", avg_dbscan_sil, "\n")

# Gaussian Mixture Models (GMM) Silhouette Score
gmm_silhouette <- silhouette(gmm_result$classification, dist(pca_data))
avg_gmm_sil <- mean(gmm_silhouette[, 3])
cat("Average Silhouette Score for GMM:", avg_gmm_sil, "\n")

#11.2)Visualise the SIl score
# Required library
library(factoextra)

# 1. K-means Silhouette Plot
fviz_silhouette(kmeans_silhouette) +
  labs(title = "Silhouette Plot for K-means Clustering",
       x = "Clusters", y = "Silhouette Width")

# 2. Hierarchical Clustering Silhouette Plot
fviz_silhouette(hc_silhouette) +
  labs(title = "Silhouette Plot for Hierarchical Clustering",
       x = "Clusters", y = "Silhouette Width")

# 3. DBSCAN Silhouette Plot (Excluding Noise Points)
fviz_silhouette(dbscan_silhouette) +
  labs(title = "Silhouette Plot for DBSCAN Clustering (Excluding Noise)",
       x = "Clusters", y = "Silhouette Width")

# 4. Gaussian Mixture Models (GMM) Silhouette Plot
fviz_silhouette(gmm_silhouette) +
  labs(title = "Silhouette Plot for Gaussian Mixture Models",
       x = "Clusters", y = "Silhouette Width")


# 11.3) Plotting
# Create a data frame with the clustering algorithms and their silhouette scores
silhouette_data <- data.frame(
  Algorithm = c("K-means", "Hierarchical Clustering", "DBSCAN", "GMM"),
  Silhouette_Score = c(0.2784522, 0.4766735, 0.5881333, 0.5021256)
)

# Plot the silhouette scores
library(ggplot2)
ggplot(silhouette_data, aes(x = Algorithm, y = Silhouette_Score, fill = Algorithm)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = round(Silhouette_Score, 3)), vjust = -0.3, size = 4) +
  labs(
    title = "Comparison of Silhouette Scores for Clustering Algorithms",
    x = "Clustering Algorithm",
    y = "Average Silhouette Score"
  ) +
  ylim(0, 0.7) +
  theme_minimal()