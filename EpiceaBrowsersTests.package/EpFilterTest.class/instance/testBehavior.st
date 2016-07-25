tests
testBehavior
	self 
		assert: EpPluggableFilter behaviorFilter
		accepts: self classAddition
		rejects: self methodAddition