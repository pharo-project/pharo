shared pools
removeSharedPoolNamed: poolName

	self removeVariable: (self sharedPoolNamed: poolName) from: sharedPools