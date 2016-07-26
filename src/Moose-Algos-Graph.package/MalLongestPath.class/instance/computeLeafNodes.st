computing
computeLeafNodes
	self leafNodes: (self nodes select: [ :node | node nextEdges isEmpty ])