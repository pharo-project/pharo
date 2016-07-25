instance-creation
onPackages: aCollection
	^ self new
		initializeWithRPackageSet: aCollection;
		yourself