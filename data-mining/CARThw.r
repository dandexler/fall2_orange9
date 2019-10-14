library(rpart)
library(rpart.plot)
library(lattice)
library(RColorBrewer)
library(rattle)
setwd("~/Fall/Module 2/Data Mining/HW2")
load("bankData.RData")

# Split data
set.seed(1)
perm = sample(1:41188)
bank_random = bank[perm,]
train = bank_random[1:floor(0.75*41188),]
test = bank_random[(floor(0.75*41188)+1):41188,]

# Create tree to model next product purchased
tree = rpart(next.product ~ . - next.product, data=train, 
             method='class', parms = list(split='entropy'),
             control = rpart.control(cp =0))

# Plot the original tree
.pardefault = par()
par(mai=c(.2,.2,.2,.2))
plot(tree2, uniform=T)
text(tree2)

# Get variable importance for each variable in the tree
tree$variable.importance

# Create barplot to visualize variable importance
barchart(tree$variable.importance[order(tree$variable.importance)],
         xlab = 'Importance', horiz=T, xlim=c(0,8000),ylab='Variable',
         main = 'Variable Importance',cex.names=0.8, las=2, col = 'orange')

# Calculate misclassification rate on training data set
tscores = predict(tree,type='class')
cat('Training Misclassification Rate:', sum(tscores!=train$next.product)/nrow(train))

# Calculate misclassification rate on validation data set
scores = predict(tree, test, type='class')
cat('Validation Misclassification Rate:',sum(scores!=test$next.product)/nrow(test))

# Print and plot complexity points for the tree
printcp(tree)
plotcp(tree)

# Tree 1 has too much information - simplify tree using age/balance/job
# Build Tree 2 based on variable importance from original tree

tree2 = rpart(next.product ~ age + balance + housing, 
              data=train, method='class', parms = list(split='entropy'))


# Pretty tree using prp() from rpart.plot package
prp(tree2, type =0, extra=8, leaf.round=1, border.col=1, box.col=brewer.pal(10,"Set3")[tree$frame$yval], )

# Calculate misclassification rate on training data set
tscores = predict(tree2,type='class')
cat('Training Misclassification Rate:', sum(tscores!=train$next.product)/nrow(train))

# Calculate misclassification rate on validation data set
scores = predict(tree2, test, type='class')
cat('Validation Misclassification Rate:',sum(scores!=test$next.product)/nrow(test))

# Why wasn't CD included?
table(bank$next.product)
520/41188
