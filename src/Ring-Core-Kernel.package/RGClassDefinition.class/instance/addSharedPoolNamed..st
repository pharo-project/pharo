shared pools
addSharedPoolNamed: poolName
	| pool |
	pool:= (RGPoolVariableDefinition named:  poolName) parent: self.
	self addVariable: pool in: sharedPools.
	^pool