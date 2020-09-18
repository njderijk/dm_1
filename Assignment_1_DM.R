# R-script 

### ASSUMPTIONS
# No missing values
# x-values are all numeric
# Brute-force approach (try all)
# To test your algorithm, first apply it to the credit scoring data set used 
# in the lectures. With nmin = 2 and minleaf = 1 you should get the same tree as presented in the 
# lecture slides.

# Tree grow
# x = data matrix 2-dim array: each row, one training sample
# y = vector with class labels (binary --> 0-1)
# nmin =  the number of observations that a node must contain at least, 
# for it to be allowed to be split, otherwise it becomes a leaf 
# minleaf = the minimum number of observations required for a leaf node;
# hence a split that creates a node with fewer than minleaf observations is not acceptable
# 
# 
# 
# tree_grow ( x, y, nmin, minleaf, nfeat) 
# -> return tree object (tr)
# 
# x = datamatrix, numeric, 2-dimensional
# y = vector (1-0), 1-dimensional
# nmin = min. observations for split
# 
# t(observ) < nmin: leafnode -> but also t(observ) > minleaf
# t(observ) >= nmin: split
# * Draw random feature from columns for split
# * If (>= minleaf) -> Gini-index determining quality
# * If (< minleaf) -> node = leafnode
# 
# nleaf: number of features from x
# 
# 
# tree_pred(x, tr) -> return S
# x = data matrix
# tr = tree object
# return S = prediction of Class (0-1)
# 
# 1) Select feature sample
# 2) If node(observ) >= nmin && Gini > 0
# No -> leaf
# Yes -> Split options if
# -> Observ a) >= minleaf -> Check purity reduction -> pick best
# -> Observ b) 
# 
# 3) Pick best split -> Ordered list
# Split value: (x * (highest Gini value) + (x + 1)) / 2
# 
# 4) child nodes <- apply(s*, current node)
# * loop through the tree whenever a split can be applied

library(reldist)
library(glue)
creditdata <- read.csv("~/Documents/DataMining/dm_1/creditdata")
x <- creditdata[,-6]
nmin <- 2
minleaf <- 1
y <- creditdata[,6]

tree_grow <- function(x, y, nmin, minleaf, nfeat){
  # For the first iteration.. Start building the tree
  #1 pick a random feature
  feature <- sample(1:length(x), 1, replace = TRUE)
  print(feature)
  # select node with feature
  current_node_full <- x
  current_node <- x[,feature]
  print(current_node)
  Root_Node <- Node$new("Root_Node")
  if (nrow(current_node_full) >= nmin) {
    # check if the minimum observations are larger or equal to nmin
    # If Gini-Index > 0?
    # TODO: Write Gini-function (mogen we library gebruiken?)
    print(gini(current_node))
    if (gini(current_node) > 0) {
      print("Gini greater than 0, proceeding..")
      gini_df <- calculate_gini(current_node, y)
      optimal_split_value <- calculate_optimal_split_value(gini_df)
      # split on that value 
      left_split <- current_node_full[current_node_full[, feature] <= split_value,]
      right_split <- current_node_full[current_node_full[, feature] > split_value,]
        if ((nrow(left_split) > minleaf) & nrow(right_split > minleaf)){
          # allowed to split
          print("allowed to split based on minleaf constraint")
          # left split 
          # left_split_name <- "{colnames(current_node_full[feature])} <= {split_value}"
          # left_split_name <- glue(left_split_name)
          # TODO: Save the splits in the tree object. 
          # Iterate: create a new "current node on which the values "y" are also adapted"
          current_node <- left_split
          return (new_node, y, ...)
        }
        else {
          print("split not allowed, try next max value.. or make leaf of current node???")
          # TODO: make leaf of current node 
        }
      # for the amount of splits possible.. check if you can split it.
      }
  }
  # return tree
}

calculate_gini <- function(current_node, y) {
  # Calculate all possible splits 
  splits_n <- sort(unique(current_node))
  print(splits_n)
  # for each element in the splits, calculate the gini
  root_amt_bad <- sum(y==0)
  root_amt_good <- sum(y==1)
  root_impurity <- (root_amt_bad / length(y)) * (root_amt_good / length(y))
  print(root_impurity)
  print(root_amt_bad)
  print(root_amt_good)
  
  S <- current_node
  sorted_S <- sort(S)
  length_S <- length(S)
  unique_sorted <- sort(unique(S))
  print(unique_sorted)
  var_length <- length(unique_sorted)
  var_length_2 <- var_length - 1
  
  gini_df <- data.frame(NumAttr=integer(),
                        GiniValue=integer(),
                        stringsAsFactors=FALSE)
  
  print(gini_df)
  
  # Do not split on the last value of unique sorted
  for (i in splits_n[-length(splits_n)]) {
    #print("leeftijd:", i)
    split_left_greater <- y[S > i]
    #print("larger than number:", split_left_greater)
    split_left_length <- length(split_left_greater)
    print(split_left_length)
    split_left_bad <- sum(split_left_greater==0)
    split_left_good <- sum(split_left_greater==1)
    print(split_left_bad)
    print(split_left_good)
    
    split_right_smaller_equalto <- y[S <= i]
    print(split_right_smaller_equalto)
    split_right_length <- length(split_right_smaller_equalto)
    print(split_right_length)
    split_right_bad <- sum(split_right_smaller_equalto==0)
    split_right_good <- sum(split_right_smaller_equalto==1)
    print(split_right_bad)
    print(split_right_good)
    
    gini_numeric_split <- root_impurity - (((split_left_length / length_S) * (split_left_bad / split_left_length) * (split_left_good / split_left_length)) + ((split_right_length / length_S) * (split_right_bad / split_right_length) * (split_right_good / split_right_length)))
    gini_numeric_split <- format(round(gini_numeric_split, 2), nsmall = 2)	
    print(gini_numeric_split) 
    
    gini_df[nrow(gini_df) + 1,] = c(i,gini_numeric_split)
  }
  print(gini_df)
  return(gini_df)
}

calculate_optimal_split_value <- function(gini_df){
  colMax <- max(gini_df$GiniValue)
  # What if there are more than 1 maximum values?
  # print(colMax)
  # Select the index of that value
  indexMax <- which(gini_df$GiniValue == colMax)
  assocMax <- gini_df[grep(colMax,gini_df$GiniValue),]
  # print(assocMax)
  # Select the next value and calculate split value
  value_1 <- as.integer(gini_df[grep(colMax,gini_df$GiniValue),1])
  value_2 <- as.integer(gini_df[indexMax+1,1])
  split_value <- (value_1 + value_2)/2
  # TO DO: is the split possible (minleaf), if not find the next optimal value and split 
  return(split_value)
}






Gini_function <- function() {
  
}

tree_pred <- function(tr, x) {
  
  #return y
}

# Bagging 
tree_grow_b <- function(){
  
}

tree_pred_b <- function() {
} 
