instance creation
named: aName packageName: aPackageName

	^ self basicNew 
		initializeNamed: aName packageName: aPackageName;
		yourself