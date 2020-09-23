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