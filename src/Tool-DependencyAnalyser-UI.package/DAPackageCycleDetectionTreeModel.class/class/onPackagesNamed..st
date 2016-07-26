instance-creation
onPackagesNamed: aCollection
	^ self new 
		initializeWithPackageName: aCollection;
		yourself