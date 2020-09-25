library(data.tree)

creditdata <- read.csv('creditdata')
attributes <- creditdata[-ncol(creditdata)]
classes <- creditdata[ncol(creditdata)]

tree_grow <- function(x, y, nmin, minleaf, nfeat) {
  
  nr_good <- sum(classes == 0)
  nr_bad <- sum(classes == 1)
  
  root_node <- Node$new("root_node", nr_good = nr_good, nr_bad = nr_bad)
  
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
    
    # check if split is possible
    if (length(unique_values) >= 2) {
      for (i in 1:(length(unique_values) - 1)) {
        val1 <- unique_values[i]
        val2 <- unique_values[i + 1]
        avg <- (val1 + val2) / 2
        
        left_cases <- cases[cases[feature] < avg,]
        right_cases <- cases[cases[feature] > avg,]
      
        # check minleaf constraint
        if (nrow(left_cases) >= minleaf & nrow(right_cases) >= minleaf) {
          # calculate quality of this split
          total_bad <- sum(cases[ncol(cases)]==0)
          total_good <- sum(cases[ncol(cases)]==1)
          
          left_bad <- sum(left_cases[ncol(left_cases)]==0)
          left_good <- sum(left_cases[ncol(left_cases)]==1)
          
          right_bad <- sum(right_cases[ncol(right_cases)]==0)
          right_good <- sum(right_cases[ncol(right_cases)]==1)
          
          # calculate the quality of this split using the gini-index
          total_i <- (total_good / nrow(cases)) * (total_bad / nrow(cases))
          left_i <- (nrow(left_cases) / nrow(cases)) * (left_good / nrow(left_cases)) * (left_bad / nrow(left_cases))
          right_i <- (nrow(right_cases) / nrow(cases)) * (right_good / nrow(right_cases)) * (right_bad / nrow(right_cases))
          
          quality <- total_i - left_i - right_i
          
          if (quality > best_split_quality) {
            best_split_quality <- quality
            best_split_feature <- feature
            best_split_value <- avg
          }
        }
      }
    }
  }
  
  if (!(best_split_quality == -1) & !(best_split_feature == -1) & !(best_split_value == -1)) {
    # a best split which meets the minleaf constraint is found
    feature_name <- colnames(cases)[best_split_feature]
    left_name <- paste(feature_name, " < ", best_split_value)
    right_name <- paste(feature_name, " > ", best_split_value)
    
    left_cases <- cases[cases[best_split_feature] < best_split_value,]
    right_cases <- cases[cases[best_split_feature] > best_split_value,]
    
    left_bad <- sum(left_cases[ncol(left_cases)]==0)
    left_good <- sum(left_cases[ncol(left_cases)]==1)
    
    right_bad <- sum(right_cases[ncol(right_cases)]==0)
    right_good <- sum(right_cases[ncol(right_cases)]==1)
    
    node$feature <- best_split_feature
    node$value <- best_split_value
    
    left_node <- node$AddChild(left_name, nr_good = left_good, nr_bad = left_bad)
    right_node <- node$AddChild(right_name, nr_good = right_good, nr_bad = right_bad)
    
    tree_grow_recurs(left_node, left_cases[-ncol(left_cases)], left_cases[ncol(left_cases)], nmin, minleaf, nfeat)
    tree_grow_recurs(right_node, right_cases[-ncol(right_cases)], right_cases[ncol(right_cases)] ,nmin, minleaf, nfeat)
  }
  
  return()
}

tr <- tree_grow(attributes, classes, 2, 1, ncol(attributes))

print(tr, "nr_good", "nr_bad", "feature", "value")
