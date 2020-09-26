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

y <- tree_pred(attributes, tr)

print(y)