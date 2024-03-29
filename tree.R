# Noud Jan de Rijk - 5670721
# Rose Mary Hulscher - 4272978
# Yasmin van Dijk - 6014542

## Part 1: Programming 
library(data.tree)
library(tibble)

# tree_grow
#
# input arguments names and types:
# x: data.frame
# y: vector
# nmin: integer
# minleaf: integer
# nfeat: integer
#
# returned result:
# the complete classification tree object (data.tree object)
# 
# cases x and classes y are used to grow a classification tree,
# nmin and minleaf are used to stop growing the tree early
# nfeat is the number of features that are used
# tree_grow_recurs function is used to grow the tree recursively
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

# tree_pred:
# 
# input arguments names and types:
# x: data.frame
# tr: data.tree object
#
# returned result:
# a vector with the classes predicted for x
#
# predicts the classes for x using classification tree tr
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

# tree_grow_b:
#
# input arguments names and types:
# x: data.frame
# y: vector
# nmin: integer
# minleaf: integer
# nfeat: integer
# m: integer
#
# returned result:
# a list of m classification trees
#
# m trees are grown for cases x and classes y and stored in a list
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
                
                attributes <- sample_cases[-ncol(sample_cases)]
                classes <- sample_cases[ncol(cases)]
                
                # grow a tree for this sample
                tree <- tree_grow(attributes, classes, nmin, minleaf, nfeat)
                
                tree_list[[i]] <- tree
        }
        
        return (tree_list)
}

# tree_pred_b:
#
# input argument names and types:
# trees: list of data.tree objects
# x: data.frame
#
# returned result:
# vector with classes predicted for x
#
# the classes for x are predicted using each classification tree,
# the majority of each of these predictions is then used as the predicted class for x
tree_pred_b <- function(x, trees) {
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

# tree_grow_recurs
#
# input arguments names and types:
# node: data.tree object
# cases: data.frame
# classes: vector
# nmin: integer
# minleaf: integer
# nfeat: integer
#
# returned result:
# complete classification subtree with the given node as a root (data.tree object)
#
# this function is used to grow a classification tree recursively,
# given the nmin and minleaf constraint, the best split for the given node is determined,
# if a split is possible, possible splits for the child nodes are determined recursively
tree_grow_recurs <- function(node, cases, classes, nmin, minleaf, nfeat) {
        # total number of cases
        total_cases <- nrow(cases)
        
        # check nmin constraint
        if (total_cases < nmin) {
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
        
        total_columns <- ncol(cases)
        
        # quality of the parent node
        total_bad <- sum(cases[total_columns] == 0)
        total_good <- sum(cases[total_columns] == 1)
        total_i <- (total_good / total_cases) * (total_bad / total_cases)
        
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
                                
                                # total number of cases both splits
                                total_left <- nrow(left_cases)
                                total_right <- nrow(right_cases)
                                
                                # check minleaf constraint
                                if (total_left >= minleaf & total_right >= minleaf) {
                                        # calculate quality of this split
                                        left_good <- sum(left_cases$class)
                                        left_bad <- total_left - left_good
                                        
                                        right_good <- sum(right_cases$class)
                                        right_bad <- total_right - right_good
                                        
                                        # calculate the quality of this split using the gini-index
                                        left_i <- (total_left / total_cases) * (left_good / total_left) * (left_bad / total_left)
                                        right_i <- (total_right / total_cases) * (right_good / total_right) * (right_bad / total_right)
                                        
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
                
                # nr of good and bad cases that will end up in the right child node
                left_total <- nrow(left_cases)
                left_good <- sum(left_cases$class)
                left_bad <- left_total - left_good
                
                # nr of good and bad cases that will end up in the left child node
                right_total <- nrow(right_cases)
                right_good <- sum(right_cases$class)
                right_bad <- right_total - right_good
                
                # add the feature and the value this node is split on to the node
                node$feature <- best_split_feature
                node$value <- best_split_value
                
                # create and add both child nodes
                left_node <- node$AddChild(left_name, nr_good = left_good, nr_bad = left_bad)
                right_node <- node$AddChild(right_name, nr_good = right_good, nr_bad = right_bad)
                
                # grow subtrees for both child nodes recursively
                tree_grow_recurs(left_node, left_cases[-total_columns], left_cases[total_columns], nmin, minleaf, nfeat)
                tree_grow_recurs(right_node, right_cases[-total_columns], right_cases[total_columns] ,nmin, minleaf, nfeat)
        }
        
        return()
}

# ## Part 2: Data Analysis
# 
# # 1. Short description of the data
# 
# # The data set lists the number of pre- and post-release defects for every package and file in 
# # Eclipse releases in versions 2.0, 2.1, and 3.0. Historically, bug databases do not have information
# # regarding how, where, and by whom bugs were solved. However, this data set features metrics for the mapping 
# # of packages and classes to the number of defects that were reported in the first six 
# # months before and after release.
# 
# # read in the training data (eclipse bug 2.0)
# raw_training_data <- read.csv('./bug_data/eclipse-metrics-packages-2.0.csv', header = T, sep = ";")
# training_data_x <- raw_training_data[,c(3, 5:44)]
# training_data_y <- raw_training_data[4]
# training_data_y <- ifelse(training_data_y$post >= 1, 1, 0)
# 
# # read in the testing data (eclipse bug 3.0)
# raw_testing_data <- read.csv('./bug_data/eclipse-metrics-packages-3.0.csv', header = T, sep = ";")
# testing_data_x <- raw_testing_data[,c(3, 5:44)]
# testing_data_y <- raw_testing_data[4]
# testing_data_y <- ifelse(testing_data_y$post >= 1, 1, 0)
# 
# # 1) train a single classification tree (nmin = 15, minleaf = 5, nfeat = 41)
# #       and compute the accuracy, precision and recall on the test set
# 
# # grow a single tree on the training data
# tr_single <- tree_grow(training_data_x, training_data_y, 15, 5, 41)
# 
# 
# # predict classes for testing data using single tree grown on training data
# y_single <- tree_pred(testing_data_x, tr_single)
# 
# # confusion matrix (single tree):
# conf_matrix_single <- table(true = testing_data_y, pred = y_single)
# TP_single <- conf_matrix_single[1,1]
# FP_single <- conf_matrix_single[1,2]
# FN_single <- conf_matrix_single[2,1]
# TN_single <- conf_matrix_single[2,2]
# 
# print(conf_matrix_single)
# 
# # accuracy, precision and recall for bagging:
# accuracy_single <- (TP_single + TN_single) / (TP_single + TN_single + FP_single + FN_single)
# precision_single <- TP_single / (TP_single + FP_single)
# recall_single <- TP_single / (TP_single + FN_single)
# print(paste("Accuracy single tree:", accuracy_single, "Precision single tree:", precision_single, "Recall single tree:", recall_single))
# 
# tibble(Sensitivity = TP_single / (TP_single+FN_single), #sensitivity
#        Specificity = TN_single / (TN_single+FP_single), #specificity
#        #FPR = FP_single / (FP_single+TN_single),
#        #FNR = FN_single / (FN_single+TP_single),
#        Precision = TP_single / (TP_single + FP_single), # Precision
#        #NPV = TN_single / (TN_single + FN_single),
#        Accuracy = (TP_single + TN_single) / sum(conf_matrix_single))
# 
# # 2) use bagging with nmin = 15, minleaf = 5, nfeat = 41, m = 100
# #       and compute the accuracy, precision and recall on the test set
# # grow a tree with bagging on the training data
# tr_bagging <- tree_grow_b(training_data_x, training_data_y, 15, 5, 41, 100)
# 
# # predict classes for testing data using tree grown on training data with bagging
# y_bagging <- tree_pred_b(tr_bagging, testing_data_x)
# 
# 
# # confusion matrix (bagging):
# conf_matrix_bagging <- table(true = testing_data_y, pred = y_bagging)
# TP_bagging <- conf_matrix_bagging[1,1]
# FP_bagging <- conf_matrix_bagging[1,2]
# FN_bagging <- conf_matrix_bagging[2,1]
# TN_bagging <- conf_matrix_bagging[2,2]
# 
# print(conf_matrix_bagging)
# 
# 
# # accuracy, precision and recall for bagging:
# accuracy_bagging <- (TP_bagging + TN_bagging) / (TP_bagging + TN_bagging + FP_bagging + FN_bagging)
# precision_bagging <- TP_bagging / (TP_bagging + FP_bagging)
# recall_bagging <- TP_bagging / (TP_bagging + FN_bagging)
# print(paste("Accuracy bagging tree:", accuracy_bagging, "Precision bagging tree:", precision_bagging, "Recall bagging tree:", recall_bagging))
# 
# tibble(Sensitivity = TP_bagging / (TP_bagging+FN_bagging), #sensitivity
#        Specificity = TN_bagging / (TN_bagging+FP_bagging), #specificity
#        # FPR = FP_bagging / (FP_bagging+TN_bagging),
#        # FNR = FN_bagging / (FN_bagging+TP_bagging),
#        Precision = TP_bagging / (TP_bagging + FP_bagging), # Precision
#        # NPV = TN_bagging / (TN_bagging + FN_bagging),
#        Accuracy = (TP_bagging + TN_bagging) / sum(conf_matrix_bagging))
# 
# # 3) Use Use random forests with the same parameter settings as under (2), except that nfeat = 6 
# # Compute the accuracy, precision and recall of the random forest on the test set.
# # grow a random forest on the training data
# tr_random_forest <- tree_grow_b(training_data_x, training_data_y, 15, 5, 6, 100)
# 
# # predict classes for testing data using tree grown on training data with bagging
# y_random_forest <- tree_pred_b(tr_random_forest, testing_data_x)
# 
# # confusion matrix (random forest):
# conf_matrix_random_forest <- table(true = testing_data_y, pred = y_random_forest)
# TP_random_forest <- conf_matrix_random_forest[1,1]
# FP_random_forest <- conf_matrix_random_forest[1,2]
# FN_random_forest <- conf_matrix_random_forest[2,1]
# TN_random_forest <- conf_matrix_random_forest[2,2]
# 
# print(conf_matrix_random_forest)
# 
# 
# # accuracy, precision and recall for random forest:
# accuracy_random_forest <- (TP_random_forest + TN_random_forest) / (TP_random_forest + TN_random_forest + FP_random_forest + FN_random_forest)
# precision_random_forest <- TP_random_forest / (TP_random_forest + FP_random_forest)
# recall_random_forest <- TP_random_forest / (TP_random_forest + FN_random_forest)
# print(paste("Accuracy random_forest:", accuracy_random_forest, "Precision random forest:", precision_random_forest, "Recall random forest:", recall_random_forest))
# 
# tibble(Sensitivity = TP_random_forest / (TP_random_forest+FN_random_forest), #sensitivity
#        Specificity = TN_random_forest / (TN_random_forest+FP_random_forest), #specificity
#        # FPR = FP_random_forest / (FP_random_forest+TN_random_forest),
#        #FNR = FN_random_forest / (FN_random_forest+TP_random_forest),
#        Precision = TP_random_forest / (TP_random_forest + FP_random_forest), # Precision
#        #NPV = TN_random_forest / (TN_random_forest + FN_random_forest),
#        Accuracy = (TP_random_forest + TN_random_forest) / sum(conf_matrix_random_forest))
# 
