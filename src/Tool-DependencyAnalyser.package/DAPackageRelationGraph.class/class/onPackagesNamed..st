instance creation
onPackagesNamed: packageNames
	^ self basicNew
		initializeWithPackages: (packageNames collect: [ :each | DAPackage onPackageNamed: each ]);
		yourself