merging
mergeNodesInCircuits
	circuits do: [ :each | self mergeNodesInCircuit: each ].
	self removeReferencesToMergedNodes.
	self removeMergedNodes.
	self addCircuitNodes