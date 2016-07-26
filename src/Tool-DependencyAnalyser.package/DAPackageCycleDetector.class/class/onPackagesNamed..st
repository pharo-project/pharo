instance-creation
onPackagesNamed: aCollection
	^ self onPackages: (aCollection collect: [ :each | RPackageSet named: each ])