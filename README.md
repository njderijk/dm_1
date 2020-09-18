# dm_1

tree_grow ( x, y, nmin, minleaf, nfeat) 
	-> return tree object (tr)

x = datamatrix, numeric, 2-dimensional
y = vector (1-0), 1-dimensional
nmin = min. observations for split

t(observ) < nmin: leafnode -> but also t(observ) > minleaf
t(observ) >= nmin: split
	* Draw random feature from columns for split
	* If (>= minleaf) -> Gini-index determining quality
	* If (< minleaf) -> node = leafnode

nleaf: number of features from x


tree_pred(x, tr) -> return S
x = data matrix
tr = tree object
return S = prediction of Class (0-1)

1) Select feature sample
2) If node(observ) >= nmin && Gini > 0
	No -> leaf
	Yes -> Split options if
		-> Observ a) >= minleaf -> Check purity reduction -> pick best
		-> Observ b) 

3) Pick best split -> Ordered list
	Split value: (x * (highest Gini value) + (x + 1)) / 2

4) child nodes <- apply(s*, current node)
	* loop through the tree whenever a split can be applied
