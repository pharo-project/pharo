instance creation
onPackages: aCollection
	^ self basicNew
		initializeWithPackages: aCollection;
		yourself