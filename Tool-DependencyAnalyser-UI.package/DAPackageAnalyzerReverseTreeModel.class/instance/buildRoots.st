private
buildRoots
	| collectionOfWrapper |
	collectionOfWrapper := relationGraph allDependentPackagesFromSeenPackages
		collect: [ :package | package asDependentPackageWrapperOn: relationGraph ].
	
	self tree roots: (self nodesFor: collectionOfWrapper asSortedCollection).
	self updatePackageLabel.