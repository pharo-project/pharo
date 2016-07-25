accessing
initializeWithRPackageSet: aCollection
	self relationGraph: (DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: each ]))