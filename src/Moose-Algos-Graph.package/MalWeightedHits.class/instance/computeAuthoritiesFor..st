running
computeAuthoritiesFor: aNode

	aNode auth: (aNode previousEdges inject: 0 into: [:s :e| s + (e weight * e from hub)])