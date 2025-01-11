library(arules)
library(ggplot2)

# Load datasets
orders <- read.csv("instacart/order_products__prior.csv", sep=",", fileEncoding="UTF-8-BOM")
products <- read.csv("instacart/products.csv", sep=",", fileEncoding="UTF-8-BOM")

# Inspect the datasets
head(orders)
head(products)

# EDA --------------------------------------
# Get dataset dimensions
cat("Orders Dataset: ", dim(orders)[1], "rows and", dim(orders)[2], "columns\n")
cat("Products Dataset: ", dim(products)[1], "rows and", dim(products)[2], "columns\n")

# Check for missing values in orders
cat("Missing values in Orders dataset:\n")
colSums(is.na(orders))

# Check for missing values in products
cat("Missing values in Products dataset:\n")
colSums(is.na(products))

# Summary of Orders dataset
summary(orders)

# Count unique order IDs and products
cat("Unique Orders:", length(unique(orders$order_id)), "\n")
cat("Unique Products in Orders:", length(unique(orders$product_id)), "\n")

# Distribution of `add_to_cart_order`
ggplot(orders, aes(x = add_to_cart_order)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Add-to-Cart Order", x = "Add to Cart Position", y = "Frequency")

# Summary of Products dataset
summary(products)

# Count unique products, aisles, and departments
cat("Unique Products:", length(unique(products$product_id)), "\n")
cat("Unique Aisles:", length(unique(products$aisle_id)), "\n")
cat("Unique Departments:", length(unique(products$department_id)), "\n")

# Top 10 Departments
products %>%
  group_by(department_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(as.factor(department_id), count), y = count)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Departments by Product Count", x = "Department ID", y = "Product Count")

# Print number of duplicates in Orders dataset
cat("Number of duplicate rows in Orders dataset:", sum(duplicated(orders)), "\n")

# Print number of duplicates in Products dataset
cat("Number of duplicate rows in Products dataset:", sum(duplicated(products)), "\n")




# Top 10 frequently purchased products
top_products <- orders %>%
  group_by(product_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

# Merge with Products dataset to get product names
top_products <- merge(top_products, products, by = "product_id")

# Plot top products
ggplot(top_products, aes(x = reorder(product_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Most Purchased Products", x = "Product Name", y = "Frequency")



# Join Orders with Products to analyze department popularity
orders_with_products <- merge(orders, products, by = "product_id")

# Department-wise order counts
department_counts <- orders_with_products %>%
  group_by(department_id) %>%
  summarise(order_count = n()) %>%
  arrange(desc(order_count))

# Plot department popularity
ggplot(department_counts, aes(x = reorder(as.factor(department_id), order_count), y = order_count)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +
  labs(title = "Orders by Department", x = "Department ID", y = "Order Count")



# Proportion of reordered items
reorder_proportion <- orders %>%
  group_by(reordered) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot reorder percentage
ggplot(reorder_proportion, aes(x = factor(reordered), y = percentage, fill = factor(reordered))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FFB3BA", "#BAE1FF")) +
  labs(title = "Proportion of Reordered Items", x = "Reordered (0 = No, 1 = Yes)", y = "Percentage") +
  theme_minimal()

# KDD --------------------------------------
# Select relevant columns
orders_selected <- orders %>% select(order_id, product_id, reordered)
products_selected <- products %>% select(product_id, product_name, aisle_id, department_id)

# Inspect selected data
head(orders_selected)
head(products_selected)

# Check for missing values
colSums(is.na(orders_selected))
colSums(is.na(products_selected))

# Example: Remove rows with missing product names
products_selected <- na.omit(products_selected)

# Remove duplicate rows
orders_selected <- orders_selected %>% distinct()
products_selected <- products_selected %>% distinct()

# Merge orders with product details
merged_data <- merge(orders_selected, products_selected, by = "product_id")

# Inspect merged data
head(merged_data)

# Group products by order_id
transactions_data <- merged_data %>%
  group_by(order_id) %>%
  summarise(items = paste(product_name, collapse = ", ")) %>%
  ungroup()



# Apply Apriori algorithm
frequent_itemsets <- apriori(transactions_data, parameter = list(supp = 0.0003, target = "frequent itemsets"))

# View results
inspect(frequent_itemsets[1:10])

#APRIORI
# Generate rules from frequent itemsets
rules <- apriori(transactions_data, parameter = list(supp = 0.0003, conf = 0.7, target = "rules"))

# View top 10 rules
inspect(sort(rules, by = "lift")[1:10])

#------------------------------------------------------------
#setwd("C:/Users/Celina Chintya/Downloads/fim4r/fim4r")
#install.packages(".", repos = NULL, type = "source")
library(fim4r)

transactions_data <- as.list(transactions_data)
apr <- fim4r(transactions_data, supp = 0.0003, conf = 0.7, target = "rules", method="apriori")



