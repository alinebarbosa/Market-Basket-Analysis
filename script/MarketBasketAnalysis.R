####### Data analytics II - Task 4
####### Market basket analysis - Discover associations between products

#### Install and load packages and set seed ####
pacman::p_load(readr, 
               caret, 
               dplyr,
               ggplot2, 
               DataExplorer, 
               MASS,
               arules,
               arulesViz,
               stringr)
set.seed(123)

#### Upload the transaction data ####
transactions <- read.transactions("ElectronidexTransactions2017.csv", 
                                  format = "basket", 
                                  sep = ",", 
                                  rm.duplicates = T)
View(transactions)

#### Get to know the data ####
# See all the elements
inspect(transactions)
LIST(transactions)
transactions@itemInfo

# Remove empty rows
transactions <- transactions[which(size(transactions) != 0)]

# General information
length(transactions)
size(transactions)
plot(size(transactions))
summary(transactions)
itemFrequencyPlot(transactions, topN = 10)
plot_histogram(size(transactions),
               geom_histogram_args = list(color="black", 
                                          fill="grey"),
               ggtheme = theme_light())

# Sample of the data
transactionsSample <- sample(transactions, 5)
summary(transactionsSample)
image(transactionsSample)

# Filter the data
transactions_10itens <- transactions[which(size(transactions) == 10),]
summary(transactions_10itens)
crosstable_10itens <- crossTable(transactions_10itens)
itemFrequencyPlot(transactions_10itens, topN = 10)

# Transform to a matrix and then to a data frame
transactionsMatrix <- as(transactions, "matrix")
View(transactionsMatrix)
transactionsDF <- as.data.frame(transactionsMatrix)
View(transactionsDF)
str(transactionsDF)

#### Model - General ####
# Apply apriori algorithm
rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, 
                                  conf = 0.55, 
                                  minlen = 2))
inspect(sort(rules, by = "lift"))
summary(rules)
is.redundant(rules)
plot(rules)
plot(rules, method = "graph", control = list(type="items"))

# Filter rules per item
imacRules <- subset(rules, items %in% "iMac")
imacRules

#### Model - By size ####
# Less than 5 itens
trSmallSize <- transactions[which(size(transactions) < 4)]
rulesSmallSize <- apriori(trSmallSize,
                          parameter = list(supp = 0.01,
                                           conf = 0.1,
                                           minlen = 2))
inspect(rulesSmallSize)

# 5 or more itens
trBigSize <- transactions[which(size(transactions) >= 4)]
rulesBigSize <- apriori(trBigSize,
                          parameter = list(supp = 0.01,
                                           conf = 0.6,
                                           minlen = 2))
inspect(rulesBigSize)

#### Upload the data with items information ####
items <- read_csv("ElectronidexItems.csv")
items <- items[order(items$Product),]
items

#### Transaction data with product type ####
# Add levels to the original transaction data
transactions@itemInfo$levels <- items$`Product Type`

# Create a new data with Product Types
unique(transactions@itemInfo$levels)
levels(transactions@itemInfo$levels) <- unique(transactions@itemInfo$levels)
print(levels(transactions@itemInfo$levels))
transactions_ProdTypes <- arules::aggregate(transactions,
                                            transactions@itemInfo$levels)
summary(transactions_ProdTypes)
itemFrequencyPlot(transactions_ProdTypes, topN = 17)

# Remove empty rows
transactions_ProdTypes <- transactions_ProdTypes[which(size(transactions_ProdTypes) != 0)]

#### Model - Product type ####
pt <- unique(transactions@itemInfo$levels)
rulesProdTypes <- apriori(transactions_ProdTypes,
                          parameter = list(supp = 0.01,
                                           conf = 0.3,
                                           minlen = 2),
                          appearance = list(rhs=c("External Hardrives", "Computer Mice",
                                                  "Monitors","Computer Headphones",
                                                  "Active Headphones", "Keyboard", 
                                                  "Smart Home Devices","Computer Cords", 
                                                  "Accessories", "Speakers",
                                                  "Printers", "Printer Ink", 
                                                  "Mouse and Keyboard Combo", 
                                                  "Computer Tablets", "Computer Stands")))
inspect(sort(rulesProdTypes, by = "lift"))
summary(rulesProdTypes)
is.redundant(rulesProdTypes)
plot(rulesProdTypes)
plot(rulesProdTypes, method = "graph", control = list(type="items"))

#### Data frame with all the categories in each transaction ####
# Create a transaction set with all information
items$CategProd <- paste(items$`Product Type`, 
                         items$Product, 
                         sep = "_")
transactions_Cat <- arules::aggregate(transactions,
                                      items$CategProd)

# Transform in matrix and data frame
tr_matrix <- as(transactions_Cat, "matrix")
View(tr_matrix)
tr_df <- as.data.frame(tr_matrix)
View(tr_df)

# Put the column names in the data frame
newData <- data.frame(matrix(nrow=9833,ncol=30))

for (i in 1:nrow(tr_df)) {
  c = 1
  for (j in 1:ncol(tr_df[i,])) {
    if (tr_df[i,j] == "TRUE") {
      newData[i,c] <- names(tr_df)[j]
      c = c+1
    }
  }
}

View(newData)

# Remove the product name to stay only the product type
for (i in 1:nrow(newData)) {
  for (j in 1:ncol(newData[i,])) {
    if (is.na(newData[i,j]) == "FALSE") {
      newData[i,j] <- as.character(sub("\\_.*", "", newData[i,j]))
    }
  }
}







# ------------------------- Trying things---------------------------------

#1 - contar frequencia de cada product type
#2 - criar condições
# >10 - business            rowSums(is.na(newData)) < 20
# <10 , >3 - business       rowSums(is.na(newData)) >= 20 &
# <10 , <3 - person         rowSums(is.na(newData)) >= 20 &
#3 - aplicar condições
#4 - dividir data set
#5 - aplicar algoritmo

length(is.na(newData[1,]) == "FALSE")

rowSums(newData['Desktop', 4])

unique(transactions@itemInfo$levels)[1]

for (i in 1:17) {
  print(unique(transactions@itemInfo$levels)[i])
}


getCount <- function(data,keyword) {
  wcount <- str_count(data, keyword)
  return(wcount)
}

getCount(newData[4,], "Desktop")







index <- c()

for (c in 1:17) {
  ct = 0
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.na(newData[i,j]) == "FALSE") {
        if (newData[i,j] == unique(transactions@itemInfo$levels)[c]) {
#          print(newData[i,j])
          ct = ct + 1
          print(ct)
            print(newData[i,j])
          print(i)
          print(j)
#          if (ct > 3) {
#            index <- rbind(index, i)
#            print(index)
#          }
        }
      }
    }
  }
}
