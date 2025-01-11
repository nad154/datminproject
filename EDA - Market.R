products <- read.csv("retail/assignment_data.csv", sep=";", na.strings= c(""))

install.packages("lubridate")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# View the first few rows
head(products)

# Get dataset dimensions
cat("Products Dataset: ", dim(products)[1], "rows and", dim(products)[2], "columns\n")

# Get dataset structure (column types)
str(products)

# Summary statistics (including min, max, mean)
summary(products)

# Check for missing values
colSums(is.na(products))

#Convert-------------
# Convert Date to Date type
products$Date <- dmy_hm(products$Date)

# Convert Price and Quantity to numeric if necessary
products$Price <- as.numeric(gsub(",", ".", products$Price))

# Convert CustomerID to character
products$CustomerID <- as.character(products$CustomerID)

# Inspect changes
str(products)

# Calculate total sales per transaction
products <- products %>%
  mutate(TotalSales = Quantity * Price)

# Get summary statistics for total sales
summary(products$TotalSales)

# Number of unique products
unique_products <- length(unique(products$Itemname))

# Number of unique customers
unique_customers <- length(unique(products$CustomerID))

cat("Unique Products:", unique_products, "\n")
cat("Unique Customers:", unique_customers, "\n")

# Extract year, month, and day from Date for analysis
products$Year <- year(products$Date)
products$Month <- month(products$Date)
products$Day <- day(products$Date)

# Plot sales by month
ggplot(products, aes(x = Month)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Sales Distribution by Month", x = "Month", y = "Frequency")

# Plot sales by year
ggplot(products, aes(x = Year)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Sales Distribution by Year", x = "Year", y = "Frequency")

# Top 10 most popular items by quantity
top_items <- products %>%
  group_by(Itemname) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalQuantity)) %>%
  head(10)

# Plot top 10 items by quantity sold
ggplot(top_items, aes(x = reorder(Itemname, TotalQuantity), y = TotalQuantity)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Items", x = "Item Name", y = "Total Quantity Sold")

#------------------------
# Total sales per customer
customer_sales <- products %>%
  group_by(CustomerID) %>%
  summarise(TotalSales = sum(TotalSales)) %>%
  arrange(desc(TotalSales)) %>%
  head(10)

# Plot total sales by customer
ggplot(customer_sales, aes(x = reorder(as.factor(CustomerID), TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Customers by Total Sales", x = "Customer ID", y = "Total Sales")

# Total sales by country
country_sales <- products %>%
  group_by(Country) %>%
  summarise(TotalSales = sum(TotalSales)) %>%
  arrange(desc(TotalSales))

# Plot sales by country
ggplot(country_sales, aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = "Country", y = "Total Sales")

# Check for duplicate transactions
duplicates <- products[duplicated(products), ]
cat("Number of duplicate rows:", nrow(duplicates), "\n")




