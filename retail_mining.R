data <- read.csv("retail/assignment_data.csv", sep=";", fileEncoding="UTF-8-BOM")

cleaned_data <- subset(data, data$Quantity >= 0)
cleaned_data <- subset(cleaned_data, cleaned_data$Itemname!= "")
cleaned_data <- subset(cleaned_data, cleaned_data$Itemname!= "?")
cleaned_data <- na.omit(cleaned_data)
cleaned_data <- cleaned_data[!duplicated(cleaned_data)]


filtered_data <- cleaned_data[, c("BillNo", "Itemname")]
filtered_data <- split(filtered_data$Itemname, filtered_data$BillNo)



install.packages('arules')
library(arules)

transaction_list <- as(filtered_data, "transactions")

rules <- apriori(transaction_list, 
                 parameter=list(supp=0.012, 
                                target="rules", 
                                conf=0.8))
inspect(rules)

write(transaction_list, file='dataFP2-non.csv', sep=',')


setwd("C:/Users/Asus/Downloads/fim4r")
install.packages(".", repos = NULL, type = "source")

library(fim4r)

sink("C:/Users/Asus/OneDrive/sem 5/dm-fpgrowth/dataset2output.txt")

start <- Sys.time()
apriori <- fim4r(
  transaction_list,
  method = 'apriori',
  target = "rules",
  support = 0.019,
  confidence = 0.7)
inspect(apriori)
end.time <- Sys.time()
time.taken <- round(end.time - start,4)
time.taken

start <- Sys.time()
fpgrowth1 <- fim4r(
  transaction_list,
  method = 'fpgrowth',
  target = "rules",
  support = 0.019,
  confidence = 0.7)
inspect(fpgrowth1)
end.time <- Sys.time()
time.taken <- round(end.time - start,4)
time.taken

start <- Sys.time()
eclat <- fim4r(
  transaction_list,
  method = 'eclat',
  target = "rules",
  support = 0.019,
  confidence = 0.7)
inspect(eclat)
end.time <- Sys.time()
time.taken <- round(end.time - start,4)
time.taken

sink()


