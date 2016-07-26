shared pools
addSharedPool: aRGPoolVariableDefinition
	self 
		addVariable: (aRGPoolVariableDefinition parent: self) 
		in: sharedPools