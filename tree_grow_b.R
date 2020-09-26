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

trees <- tree_grow_b(attributes, classes, 2, 1, ncol(attributes), 15)

print(trees)