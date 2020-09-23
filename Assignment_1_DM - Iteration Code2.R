library(dplyr)
dataset <- read.csv("~/GitHub/dm_1/creditdata")
dataset <- dataset[,-length(dataset)]
testdata <- creditdata[,3]
minimum_observations <- 2
minimum_leafs <- 1
minimum_features <- 2
# 
# pred_tree <- Node$new("Root_Node", observations=20, bad=5, good=5)
pred_tree$AddChild("income <= 36", observations=5, bad=5, good=2, operator="<=", split_on=36)
# pred_tree$'income <= 36'$AddChild("age <= 32.5", observations=5, bad=4, good=0, operator="<=", split_on=32.5)
# pred_tree$'income <= 36'$AddChild("age > 32.5", observations=5, bad=1, good=2, operator=">", split_on=32.5)
pred_tree$AddChild("income > 36", observations=5, bad=0, good= 3, operator=">", split_on=36)
# print(pred_tree, "observations", "bad", "good", "operator", "split_on", "level")

tree_grow(dataset, testdata, minimum_observations, minimum_leaf, minimum_features)

x <- creditdata

tree_grow <- function(x, y, nmin, minleaf, nfeat){
  # Start beginning of the tree, build the root
  pred_tree <- Node$new("Root_Node", observations=nrow(x), bad=(sum(y==0)), good=sum(y==1))
  # Calculate the children 
  # repeat {
    feature <- sample(1:length(x), 1, replace = TRUE)
    print(feature)
    # current_node <- x[feature]
    # There is an error here, the current node should not be a subset of x, 
    # otherwise the result will be the same constantly. 
    # print(current_node)
    
    # How large is the tree? 
    depth_tree=pred_tree$height
    # SELECTION OF CURRENT NODE HERE; perhaps writing a function..?
    # Combining the heigth of the tree.. 
    # here, select one-node for simplicity 
    current_node <- pred_tree$Climb(position=1)
    
    # If the current_node is larger than the nmin
    if (current_node$observations >= nmin) {
      # Depth traverse the left split until the end.. 
      # Rose: But you have to select new feature constantly.. 
      print("nrow current node > nmin")
      if (gini_impurity(current_node) > 0){ 
        # calculate the opportunities for the split,
        # So not only the tree with "bad/good" values have to be presented,
        # also the dataframe with the values itself to split on. 
        # Thus: give the tree + the original dataframe
        # Propagate the tree, to select the original "values" to split on. 
        # It would be best to actualy depth traverse the left split until the end
        gini_df <- calculate_split_options(current_node, x, feature)} 
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


calculate_split_options <- function(current_node, x, feature) {
  # Calculate all possible splits 
  # propagate through the tree.
  # this is not the best solution, but we have to know the feature values we split on.
  current_node
  
  # Split the original dataframe, on the "operator" and "split on" the left side (depending on 
  # the depth of the tree.. ) of the dataframe 
  current_node_operator <- current_node$operator
  current_node_split_value <- current_node$split_on
  
  # TODO: Actually select the relevant data 
  # current_node_selection <- filter(current_node_selection, feature + current_node_operator + current_node_split_value)
  # For now, select all the values
  # Add the "bad" and "good" values tot that selection, so we can actually
  # use this df to split
  # TODO: actually select the relevant Y values
  current_node_tibble <- tibble(values = x[,feature], lables = y)

  # Now we've selected the node, and the data to actually calculate the split
  # How many splits are possible?
  splits_n <- sort(unique(current_node_tibble$values))
  print(splits_n)
  
  # Calculate the gini for the current_node
  gini_current_node = gini_impurity(current_node)
  
  # for each element in the splits, calculate the gini
  S <- current_node_tibble$values
  sorted_S <- sort(S)
  length_S <- length(S)
  unique_sorted <- sort(unique(S))
  print(unique_sorted)
  
  gini_df <- data.frame(NumAttr=integer(),
                        GiniValue=integer(),
                        stringsAsFactors=FALSE)
  print(gini_df)
  
  # Do not split on the last value of unique sorted
  for (i in splits_n[-length(splits_n)]) {
    #print("leeftijd:", i)
    split_value = i 
    
    ### Calculate for the left side: 
    split_left_greater <- current_node_tibble %>%
      filter(values > i)
    # print("larger than number", split_value)
    split_left_length <- nrow(split_left_greater)
    print(split_left_length)
    
    # calculate the good/bad splits
    # I get an error.. not sure why, so I added the tibble before it. 
    # Superfluous.. 
    split_left_bad <- split_left_greater %>%
      filter(split_left_greater$labels == 0)
    split_left_good <- split_left_greater %>%
      filter(split_left_greater$labels == 1)
    print(split_left_bad)
    print(split_left_good)
    
    ### Calculate for the right side 
    split_right_smaller_equalto <- current_node_tibble %>%
      filter(values <= i)
    print(split_right_smaller_equalto)
    split_right_length <- nrow(split_right_smaller_equalto)
    print(split_right_length)
    split_right_bad <- split_right_smaller_equalto %>%
      filter(split_right_smaller_equalto$lables == 0)
    split_right_good <- split_right_smaller_equalto %>%
      filter(split_right_smaller_equalto$lables == 1)
    print(split_right_bad)
    print(split_right_good)
    
    ## Gini impurity
    gini_numeric_split <- gini_current_node - (((split_left_length / length_S) * (nrow(split_left_bad) / split_left_length) * (nrow(split_left_good) / split_left_length)) + ((nrow(split_right_length) / length_S) * (nrow(split_right_bad) / split_right_length) * (nrow(split_right_good) / split_right_length)))
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

gini_impurity <- function(node){
  (node$bad /node$observations) * (node$good/node$observations)
}

tree_pred <- function(tr, x) {
  
  #return y
}

# Bagging 
tree_grow_b <- function(){
  
}

tree_pred_b <- function() {
} 