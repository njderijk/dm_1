# R-script for assigning the best possible split (without minleaf, nmin)



function (x, y, nmin, minleaf, nfeat) 
{
library(reldist)

for (i in nfeat) {
  print(i)
  current_node <- x[,c(i)]
  print(current_node)
  x <- x[,c(-i)]
  print(x)
  print(gini(current_node))
  if (gini(current_node) > 0) {
	print("Gini greater than 0, proceeding.")

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

	for (i in unique_sorted) {
		print(i)

		split_left_greater <- y[S > i]
		print(split_left_greater)
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

		if (gini_numeric_split != "NaN") {
			gini_df[nrow(gini_df) + 1,] = c(i,gini_numeric_split)
		}

 	}

	print(gini_df)

	colMax <- max(gini_df$GiniValue)
  	print(colMax)
  	assocMax <- gini_df[grep(colMax,gini_df$GiniValue),]
  	print(assocMax)

  	new_gini_df <- subset(gini_df, GiniValue < colMax)
 	 print(new_gini_df)
  	secondColMax <- max(new_gini_df$GiniValue)
  	print(secondColMax)
  	secondAssocMax <- gini_df[grep(secondColMax,gini_df$GiniValue),]
  	print(secondAssocMax)

  	print("Best split(s)")
  	print(assocMax$NumAttr)
  	print(secondAssocMax$NumAttr)

	
  }
 
}



}