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

y <- tree_pred_b(trees, attributes)

print(y)