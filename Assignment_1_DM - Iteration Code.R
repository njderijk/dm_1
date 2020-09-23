library(reldist)
library(glue)

dataset <- read.csv("~/GitHub/dm_1/creditdata")
dataset <- dataset[,-length(dataset)]
testdata <- creditdata[,3]
minimum_observations <- 2
minimum_leafs <- 1
minimum_features <- 2

tree_grow(dataset, testdata, minimum_observations, minimum_leaf, minimum_features)


tree_grow <- function(x, y, nmin, minleaf, nfeat){
  # repeat {
    feature <- sample(1:length(x), 1, replace = TRUE)
    print(feature)
    current_node <- x[feature]
    print(current_node)
    
    if (nrow(current_node) >= nmin) {
      # Depth traverse the left split until the end
      
      print("nrow current node > nmin")
      if (gini_function(current_node, y) > 0) {
        gini_df <- calculate_gini(current_node, y)
        optimal_split_value <- calculate_optimal_split_value(gini_df)
        
        left_split <- current_node[current_node <= split_value,]
        right_split <- current_node_full[current_node > split_value,]
        
        if (nrow(left_split) > minleaf) {
          # wWrk een split uit en geef left_split terug om er weer doorheen te kunnen gaan totdat stopping rule inkickt (minleaf)
          
          
          # current_node <- left split # Eerst de hele linkerkant uitwerken
          
        }
        else {
          
        }
        
        if (nrow(left_split) > minleaf) {
          # Werk een split uit en geef left_split terug om er weer doorheen te kunnen gaan totdat stopping rule inkickt (minleaf)
          
          
          # Get Tree; Add ChildNodes to appropriate Level
          # current_node <- left_split
          
        }
        else {
          # left_split is opgedroogd en wordt een leaf node.
          # werk de splits uit en geef right_split terug als current node
          
          
          # current_node <- right_split
          
        }
        
      }
      
    }
    else {
      
    }
    
    
    
  # }
  
  
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






gini_function <- function(x, y) {
  return(0.1)
  
}

tree_pred <- function(tr, x) {
  
  #return y
}

# Bagging 
tree_grow_b <- function(){
  
}

tree_pred_b <- function() {
} 