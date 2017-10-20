instance creation
readOnlyFileNamed: fileName 
	^ self concreteStream readOnlyFileNamed: (self fullName: fileName)