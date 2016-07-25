running
run
	self initializeRootNodes.
	self computeLeafNodes.
	previousRanks addAll: self rootNodes.
	self computeStep