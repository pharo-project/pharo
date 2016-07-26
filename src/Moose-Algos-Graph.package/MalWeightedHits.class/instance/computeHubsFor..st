running
computeHubsFor: aNode

	aNode hub: (aNode nextEdges inject: 0 into: [:s :e| s + (e weight * e to auth)])