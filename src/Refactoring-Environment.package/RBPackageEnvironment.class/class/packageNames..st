instance creation
packageNames: aCollection
	^ self 
		onEnvironment: self default
		packageNames: aCollection