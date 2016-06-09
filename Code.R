# Load the libraries
install.packages('arules')
library(arules)
install.packages('arulesViz')
library(arulesViz)
install.packages('datasets')
library(datasets)

setwd("C:/Users/MasterYoda/Desktop/R")

set.seed(1000)

Groceries <- read.transactions("C:/Users/MasterYoda/Desktop/R/groceries.csv",sep = ",")
summary(Groceries)

itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

summary(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

subset.matrix <- is.subset(rules, rules)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])

plot(rules,method="graph",interactive=TRUE,shading=NA)
