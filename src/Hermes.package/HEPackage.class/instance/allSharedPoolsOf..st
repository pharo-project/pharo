testing
allSharedPoolsOf: aClass
	^ aClass withAllSuperclasses flatCollect: #sharedPools.