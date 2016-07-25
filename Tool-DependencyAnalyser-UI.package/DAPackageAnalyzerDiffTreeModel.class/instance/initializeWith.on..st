initialization
initializeWith: oldRelationGraph on: newRelationGraph
	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph.
	packageRelationGraphDiff make.
	
	self buildRoots
	
	