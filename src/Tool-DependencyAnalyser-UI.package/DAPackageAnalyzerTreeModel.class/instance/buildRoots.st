actions
buildRoots
	| matching roots |
	self filter
		ifNotNil: [ 
			matching := '*' , self filter , '*'.
			roots := (self nodesFor: (relationGraph seenPackagesWithFilter: matching)) ]
		ifNil: [ roots := (self nodesFor: relationGraph seenPackages) ].
	self tree roots: roots.
	self updatePackageLabel.