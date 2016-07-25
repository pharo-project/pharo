initialization
initializeWithPackageName: aCollection
	
	self analysis:  (DAPackageCycleDetector onPackagesNamed: aCollection) runAlgorithm.
	
	self cycles: self analysis cycles.