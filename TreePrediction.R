# for every node that's not a leaf, we need to know:
# - on what feature it is split
# - on what value it is split
# - (do smaller values go left, and bigger right? then traverse either left or right)
# for every node that is a leaf, we need to know:
# - what is the majority class; nr good & nr bad
# - (class is predicted by assigning a test case to the majority class of the leaf node it ends up in)

# (what data do we store in the nodes?)

# when growing the tree:
# create nodes and give them names (feature + split value)
node$AddChild("age <= 27")

# adding extra attributes to nodes:
# column index of feature that's split on:
node$feature <- 3

# value that's split on:
node$splitValue <- 35

# observations to the left / right?:
node$left <- ?
node$right <- ?
  
# keep track of nr of good and nr of bad observations in the node:
# (is waarschijnlijk alleen nodig in de leafs, voor het predicten op basis van majority)
node$good <- 40
node$bad <- 60

# predicting:
# check if node is leaf
if (node$isLeaf) { }

# if leaf:
# check if majority is good or bad; assign majority class
if (node$nrBad > node$nrGood) { 0 }
else { 1 }

# if not leaf:
feature <- node$feature
splitvalue <- node$splitValue
# check whether to traverse the tree further on the left or on the right side;
# repeat untill you end up in a leaf node





###

pred_tree <- Node$new("Root_Node", observations=10, bad=5, good=5)

pred_tree$AddChild("income <= 36", observations=5, bad=5, good=2)

pred_tree$'income <= 36'$AddChild("age <= 32.5", observations=5, bad=4, good=0)

pred_tree$'income <= 36'$AddChild("age > 32.5", observations=5, bad=1, good=2)

pred_tree$AddChild("income > 36", observations=5, bad=0, good= 3)

#

print(pred_tree, "observations", "bad", "good")

###

# get node name
pred_tree$name

# get 'left' child
left <- pred_tree$children[1]

pred_tree$Climb(position = 1, position = 1)$children
?Climb

# get 'right' child
right <- pred_tree$children[2]

# get names of child nodes
leftname <- left$`income <= 36`$name
rightname <- right$`income > 36`$name

# split name into column name; operator; and value
nameparts <- unlist(strsplit(leftname, " "))
nameparts[1]
nameparts[2]
nameparts[3]

# determine if you should go on to the left or right child node


# get left child nodes children:
left$`income <= 36`$children[1]
left$`income <= 36`$children[2]$`age > 32.5`$isLeaf

left$`income <= 36`$good
left$`income <= 36`$bad
right$`income > 36`$good
right$`income > 36`$bad

#######


pred_tree <- Node$new("Root_Node", observations=10, bad=5, good=5)

pred_tree$AddChild("income <= 36", observations=5, bad=5, good=2, operator="<=", split_on=36)

pred_tree$'income <= 36'$AddChild("age <= 32.5", observations=5, bad=4, good=0, operator="<=", split_on=32.5)

pred_tree$'income <= 36'$AddChild("age > 32.5", observations=5, bad=1, good=2, operator=">", split_on=32.5)

pred_tree$AddChild("income > 36", observations=5, bad=0, good= 3, operator=">", split_on=36)

#

print(pred_tree, "observations", "bad", "good", "operator", "split_on", "level")

# recurs test
rootnode <- Node$new("root_node")

recurs <- function(node, nrlevels) {
  if (nrlevels > 3) {
    return(node)
  }
  
  nrlevels <- nrlevels + 1
  
  left <- node$AddChild(paste("left", nrlevels))
  right <- node$AddChild(paste("right", nrlevels))
  
  recurs(left, nrlevels)
  recurs(right, nrlevels)
  
  return(node)
}

recurs(rootnode, 0)
print(rootnode)

