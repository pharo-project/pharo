merging
mergeNodesInCircuit: aNodeList
	circuitNodes add: (self nodeClass new
		mergedNodes: aNodeList;
		model: (aNodeList collect: #model);
		nextNodes: (self nextNodesFor: aNodeList);
		previousNodes: (self previousNodesFor: aNodeList);
		yourself)