
install.packages("arules")
library("arules")

library(Matrix)

#install.packages( arules , scatterplot3d, vcd, seriation, 
#                 igraph,"grid","cluster","TSP","gclus", "colorspace")

install.packages("arulesViz")

data(package = .packages(all.available = TRUE))


myDataSet <- read.table(file.choose(""), header = FALSE, sep = ",")

myrules = apriori(titanic.raw)

inspect(myrules)


filterRHS <- apriori(titanic.raw, parameter = list(supp=0.005, conf=0.8), appearance = 
                       list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"))

inspect(filterRHS)

sortedRules <- sort(myrules, by="confidence")

inspect(sortedRules)

redundantRules <- inspect(sortedRules[is.redundant(sortedRules)])

nonRedundantRules <- inspect(sortedRules[!is.redundant(sortedRules)])

removeRedundantRules <- is.redundant(sortedRules, measure = "confidence")

removeRedundantRules

inspect(removeRedundantRules)

library(arulesViz)

plot(nonRedundantRules)

plot(nonRedundantRules,method="graph")

plot(nonRedundantRules, method="paracoord", control=list(reorder=TRUE))
