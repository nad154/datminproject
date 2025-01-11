orders <- read.csv("order_products__prior.csv", sep =",", fileEncoding = "UTF-8-BOM", na.strings = c(""))
products <- read.csv("products.csv", sep =",", fileEncoding = "UTF-8-BOM", na.strings = c(""))

orders_deteled <- orders[, c("order_id", "product_id")]
orders.merge <- merge(orders_deteled, products, by='product_id')
orders.merge.deleted <- orders.merge[, c("order_id", "product_name")]
transactions <- split(orders.merge.deleted$product_name, orders.merge.deleted$order_id)

install.packages('arules')
library(arules)
transaction_list <- as(transactions, "transactions")

frequent_itemsets <- apriori(transaction_list, 
                             parameter=list(target='frequent itemsets',
                                            support = 0.0003))
inspect(frequent_itemsets)

rules <- ruleInduction(frequent_itemsets, confidence = 0.7)
inspect(rules)
# lift == 1 -> they're independent of each other 
# lift < 1  -> they have positive correlation 
# lift > 1  -> they have negative correlation 

start.time <- Sys.time()
rules_apriori <- apriori(transaction_list, parameter = list(target = 'rules', support = 0.0003, confidence = 0.7))
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
time.taken
inspect(rules_apriori)

fpgrowth_result <- fpgrowth(transaction_list, parameter = list(support = 0.0003))

write(transaction_list, file = 'dataFP.csv', sep = ',')

system('cmd', input = 'fpgrowth')
system('cmd', input = 'fpgrowth -ts -s0003 -k, dataFP.csv fpResult.csv')
system('cmd', input = 'fpgrowth -ts -s0003 -c70 -k, dataFP.csv fpResult.csv')

?fpgrowth

install.packages("fim4r")
library(fim4r)
help(fim4r)
devtools::install_github("rcombinations/fim4r")
setwd("C:/Users/glori/Downloads/fim4r_1.8/fim4r")
install.packages(".", repos = NULL, type = "source")

install.packages("devtools")
install.packages("https://cran.r-project.org/src/contrib/Archive/fim4r/fim4r_1.0.1.tar.gz", repos = NULL, type = "source")

sink("output.txt")
"FP-GROWTH"
start.time <- Sys.time()
test <- fim4r(transaction_list, method = 'fpgrowth', target = 'rules', support = 0.0003, confidence = 0.7)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
c("Time Taken:", time.taken)
inspect(test)

"APRIORI"
start.time <- Sys.time()
apriori <- fim4r(transaction_list, method = 'apriori', target = 'rules', support = 0.0003, confidence = 0.7)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
c("Time Taken:", time.taken)
inspect(apriori)

"ECLAT"
start.time <- Sys.time()
eclat <- fim4r(transaction_list, method = 'eclat', target = 'rules', support = 0.0003, confidence = 0.7)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
c("Time Taken:", time.taken)
inspect(eclat)
sink()
