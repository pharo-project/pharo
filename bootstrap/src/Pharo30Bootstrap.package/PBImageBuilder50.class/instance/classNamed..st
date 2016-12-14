class-loader
classNamed: aString 
	
	^ (classLoader classNamed: aString)
		asRemoteClassOrTraitFromEnvironment: self