class-loader
classNamed: aString 
	
	^ EPRemoteClass on: (classLoader classNamed: aString) environment: self