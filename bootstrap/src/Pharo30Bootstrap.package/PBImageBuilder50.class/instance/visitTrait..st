visiting
visitTrait: aRFiTrait 
	^ EPRemoteTrait 
		on: (classLoader classNamed: aRFiTrait name)
		environment: self