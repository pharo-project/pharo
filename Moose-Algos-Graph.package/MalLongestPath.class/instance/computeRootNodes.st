computing
computeRootNodes
	self rootNodes: (self nodes select: [ :node | node previousEdges isEmpty ])