running
run
	self initializeRootNodes.
	self computeLeafNodes.
	previousRanks addAll: self rootNodes.
	maxIterations := self nodes size + 20.
	self computeStep