visiting
visitClass: aRFiClass
	^ EPRemoteClass 
		on: (classLoader classNamed: aRFiClass name)
		environment: self