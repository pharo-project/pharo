instance creation
oldFileNamed: fileName 
	^ self concreteStream oldFileNamed: (self fullName: fileName)