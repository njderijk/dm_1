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

tree_grow <- function(x, y, nmin, minleaf, nfeat){
}

tree_pred <- function() {
}

tree_grow_b <- function(){
  
}

tree_pred_b <- function() {
} 
