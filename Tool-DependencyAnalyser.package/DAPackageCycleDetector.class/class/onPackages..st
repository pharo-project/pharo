instance-creation
onPackages: aCollection
	^ self basicNew 
		initializeWithRPackageSet: aCollection;
		yourself