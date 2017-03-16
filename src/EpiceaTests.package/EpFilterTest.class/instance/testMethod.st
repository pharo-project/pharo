tests
testMethod	
	self 
		assert: EpPluggableFilter methodFilter
		accepts: self methodAddition
		rejects: self classAddition