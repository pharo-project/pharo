instance-creation
onPackagesNamed: aCollection
	^ self new 
		initializeWithPackageNames: aCollection;
		yourself