tree_pred_b <- function(x, trees) {
  # vector for all the predicted class labels
  # y <- vector(mode="integer", length=nrow(x))
  # 
  # print(y)
  
  predictions_matrix <- data.frame(matrix(NA, nrow(attributes), ncol=10))
    
  count <- 0
  
  for (tree in trees){
    count <- count + 1
    prediction <- tree_pred(x, tree)
    predictions_matrix[count] <- prediction
  }
  
  print(predictions_matrix)
  
  y <- ifelse(rowSums(predictions_matrix==1) < rowSums(predictions_matrix==0),0,1)
  
  print(y)
  
  return(y)
}

y <- tree_pred_b(attributes, tr)

print(y)

tree_pred <- function(x, tr) {
  # vector for all the predicted class labels
  y <- vector(mode="integer", length=nrow(x))
  
  # loop over each row
  for (i in 1:nrow(x)) {
    current_node <- tr
    
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

