instance-creation
system
	^ (self onPackagesNamed: 
		(RPackageOrganizer default packages collect: [ :package | package packageName asString ])) 			runAlgorithm.