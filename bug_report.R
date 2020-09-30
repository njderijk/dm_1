conf_matrix <- table(true = true_values$true, pred = y)
print(conf_matrix)

# The accuracy is equal to the diagonal
# The recall is the sensitivity = TPR
# The precision is the PPV

## CONFUSION MATRIX
#0 = Negative
#1 = Postive

TN <- conf_matrix[1,1] 
TP <- conf_matrix[2,2] 
FN <- conf_matrix[2,1] 
FP <- conf_matrix[1,2]

tibble(TPR = TP / (TP+FN), #sensitivity 
       TNR = TN / (TN+FP), #specificity 
       FPR = FP / (FP+TN),
       FNR = FN / (FN+TP),
       PPV = TP / (TP + FP), # Precision 
       NPV = TN / (TN + FN),
       Acc = (TP + TN) / sum(conf_matrix))


# Part 2: Data Analysis
# read in the training data (eclipse bug 2.0)
raw_training_data <- read.csv('./bug_data/eclipse-metrics-packages-2.0.csv', header = T, sep = ";")
training_data_x <- raw_training_data[,c(3, 5:44)]
training_data_y <- raw_training_data[4]
training_data_y <- ifelse(training_data_y$post >= 1, 1, 0)

# read in the testing data (eclipse bug 3.0)
raw_testing_data <- read.csv('./bug_data/eclipse-metrics-packages-3.0.csv', header = T, sep = ";")
testing_data_x <- raw_testing_data[,c(3, 5:44)]
testing_data_y <- raw_testing_data[4]
testing_data_y <- ifelse(testing_data_y$post >= 1, 1, 0)

# 1) train a single classification tree (nmin = 15, minleaf = 5, nfeat = 41)
#       and compute the accuracy, precision and recall on the test set
# grow a tree on the training data
tr <- tree_grow(training_data_x, training_data_y, 15, 5, 41)
# predict classes for testing data using tree grown on training data
y <- tree_pred(testing_data_x, tr)

# confusion matrix:
conf_matrix <- table(true = testing_data_y, pred = y)
TP <- conf_matrix[1,1]
FP <- conf_matrix[1,2]
FN <- conf_matrix[2,1]
TN <- conf_matrix[2,2]

# accuracy, precision and recall:
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

# 2) use bagging with nmin = 15, minleaf = 5, nfeat = 41, m = 100
#       and compute the accuracy, precision and recall on the test set
# grow a list of trees on the training data
trees <- tree_grow_b(training_data_x, training_data_y, 15, 5, 41, 100)
