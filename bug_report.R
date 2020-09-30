## Part 1: Programming 
library(data.tree)

tree_grow <- function(x, y, nmin, minleaf, nfeat) {
        # get the total numbers of good and bad cases
        nr_good <- sum(y == 0)
        nr_bad <- sum(y == 1)
        
        # the root node of the prediction tree
        root_node <- Node$new("root_node", nr_good = nr_good, nr_bad = nr_bad)
        
        # grow the tree using a recursive function
        tree_grow_recurs(root_node, x, y, nmin, minleaf, nfeat)
        
        return(root_node)
}

tree_grow_recurs <- function(node, cases, classes, nmin, minleaf, nfeat) {
        # check nmin constraint
        if (nrow(cases) < nmin) {
                return()
        }
        
        # don't split pure nodes (nodes where all cases have the same class)
        if (node$nr_good == 0 | node$nr_bad == 0) {
                return()
        }
        
        # pick nfeat random features
        random_features <- sample(1:ncol(cases), nfeat, replace=FALSE)
        
        # add classes column to cases
        cases$class <- classes
        
        # keep track of best found split
        best_split_quality <- -1
        best_split_feature <- -1
        best_split_value <- -1
        
        # loop over all features
        for (feature in random_features) {
                # find best split for this feature
                unique_values <- sort(unique(cases[,feature]))
                
                # check if split is possible (can't split on only one unique value)
                if (length(unique_values) >= 2) {
                        # calculate splits for consecutive values
                        for (i in 1:(length(unique_values) - 1)) {
                                # calculate the average value
                                val1 <- unique_values[i]
                                val2 <- unique_values[i + 1]
                                avg <- (val1 + val2) / 2
                                
                                # split cases on this average value
                                left_cases <- cases[cases[feature] < avg,]
                                right_cases <- cases[cases[feature] > avg,]
                                
                                # check minleaf constraint
                                if (nrow(left_cases) >= minleaf & nrow(right_cases) >= minleaf) {
                                        # calculate quality of this split
                                        total_bad <- sum(cases[ncol(cases)] == 0)
                                        total_good <- sum(cases[ncol(cases)] == 1)
                                        
                                        left_bad <- sum(left_cases[ncol(left_cases)] == 0)
                                        left_good <- sum(left_cases[ncol(left_cases)] == 1)
                                        
                                        right_bad <- sum(right_cases[ncol(right_cases)] == 0)
                                        right_good <- sum(right_cases[ncol(right_cases)] == 1)
                                        
                                        # calculate the quality of this split using the gini-index
                                        total_i <- (total_good / nrow(cases)) * (total_bad / nrow(cases))
                                        left_i <- (nrow(left_cases) / nrow(cases)) * (left_good / nrow(left_cases)) * (left_bad / nrow(left_cases))
                                        right_i <- (nrow(right_cases) / nrow(cases)) * (right_good / nrow(right_cases)) * (right_bad / nrow(right_cases))
                                        
                                        quality <- total_i - left_i - right_i
                                        
                                        # check if this split is the best found so far
                                        if (quality > best_split_quality) {
                                                best_split_quality <- quality
                                                best_split_feature <- feature
                                                best_split_value <- avg
                                        }
                                }
                        }
                }
        }
        
        # check if a split has been found which meets all the constraints
        if (!(best_split_quality == -1) & !(best_split_feature == -1) & !(best_split_value == -1)) {
                # get the names for the child nodes
                feature_name <- colnames(cases)[best_split_feature]
                left_name <- paste(feature_name, " < ", best_split_value)
                right_name <- paste(feature_name, " > ", best_split_value)
                
                # split cases on split values
                left_cases <- cases[cases[best_split_feature] < best_split_value,]
                right_cases <- cases[cases[best_split_feature] > best_split_value,]
                
                left_bad <- sum(left_cases[ncol(left_cases)] == 0)
                left_good <- sum(left_cases[ncol(left_cases)] == 1)
                
                right_bad <- sum(right_cases[ncol(right_cases)] == 0)
                right_good <- sum(right_cases[ncol(right_cases)] == 1)
                
                node$feature <- best_split_feature
                node$value <- best_split_value
                
                left_node <- node$AddChild(left_name, nr_good = left_good, nr_bad = left_bad)
                right_node <- node$AddChild(right_name, nr_good = right_good, nr_bad = right_bad)
                
                tree_grow_recurs(left_node, left_cases[-ncol(left_cases)], left_cases[ncol(left_cases)], nmin, minleaf, nfeat)
                tree_grow_recurs(right_node, right_cases[-ncol(right_cases)], right_cases[ncol(right_cases)] ,nmin, minleaf, nfeat)
        }
        
        return()
}

tree_pred <- function(x, tr) {
        # vector for all the predicted class labels
        y <- vector(mode="integer", length=nrow(x))
        
        # loop over each row of x
        for (i in 1:nrow(x)) {
                current_node <- tr
                
                # traverse down the tree until a leaf is reached
                repeat {
                        # check if current node is a leaf
                        if (current_node$isLeaf) {
                                # classify sample as majority class in this leaf
                                if (current_node$nr_good > current_node$nr_bad) {
                                        y[i] <- 1
                                }
                                else {
                                        y[i] <- 0
                                }
                                break
                        }
                        
                        # check whether to go to the left or right child node
                        split_feature <- current_node$feature
                        split_value <- current_node$value
                        
                        if (x[i, split_feature] < split_value) {
                                current_node <- Climb(current_node, position = 1)
                        }
                        else {
                                current_node <- Climb(current_node, position = 2)
                        }
                }
        }
        
        return(y)
}

# tree grow function with bagging
tree_grow_b <- function(x, y, nmin, minleaf, nfeat, m) {
        # combine features with classes
        cases <- x
        cases$class <- y
        
        # create a list for the trees
        tree_list <- list()
        
        # create m trees, each grown on a bootstrap sample of x
        for (i in 1:m) {
                # take a random sample
                sample_cases <- cases[sample(nrow(cases), (nrow(cases)), replace = TRUE), ]
                
                attributes <- cases[-ncol(cases)]
                classes <- cases[ncol(cases)]
                
                # grow a tree for this sample
                tree <- tree_grow(attributes, classes, nmin, minleaf, nfeat)
                
                tree_list[[i]] <- tree
        }
        
        return (tree_list)
}

tree_pred_b <- function(trees, x) {
        # create a dataframe for all the predictions
        predictions_matrix <- data.frame(matrix(NA, nrow = nrow(x), ncol = length(trees)))
        
        # loop over all the trees and add predictions vector as column to the matrix
        for (i in 1:length(trees)) {
                tree <- trees[[i]]
                prediction <- tree_pred(x, tree)
                predictions_matrix[i] <- prediction
        }
        
        # create a predictions vector by taking the majority votes for each row
        y <- ifelse(rowSums(predictions_matrix==1) < rowSums(predictions_matrix==0), 0, 1)
        
        return(y)
}


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
tr_single <- tree_grow(training_data_x, training_data_y, 15, 5, 41)
# predict classes for testing data using tree grown on training data
y_single <- tree_pred(testing_data_x, tr_single)

# confusion matrix:
conf_matrix_single <- table(true = testing_data_y, pred = y_single)
TP_single <- conf_matrix_single[1,1]
FP_single <- conf_matrix_single[1,2]
FN_single <- conf_matrix_single[2,1]
TN_single <- conf_matrix_single[2,2]

# accuracy, precision and recall:
accuracy <- (TP_single + TN_single) / (TP_single + TN_single + FP_single + FN_single)
precision <- TP_single / (TP_single + FP_single)
recall <- TP_single / (TP_single + FN)

# 2) use bagging with nmin = 15, minleaf = 5, nfeat = 41, m = 100
#       and compute the accuracy, precision and recall on the test set
# grow a list of trees on the training data
trees <- tree_grow_b(training_data_x, training_data_y, 15, 5, 41, 100)

# predict 
y_predict_trees <- tree_pred_b(testing_data_x, trees)


#### 
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

