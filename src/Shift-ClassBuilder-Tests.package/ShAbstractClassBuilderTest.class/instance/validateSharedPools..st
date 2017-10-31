validation
validateSharedPools: pools
	self assertCollection: result sharedPools equals: pools asOrderedCollection.