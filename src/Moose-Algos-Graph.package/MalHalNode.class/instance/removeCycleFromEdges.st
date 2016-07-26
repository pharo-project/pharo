hal-algorithm
removeCycleFromEdges

	self nextNodes removeAllFoundIn: self cycleNodes.
	self previousNodes removeAllFoundIn: self cycleNodes 