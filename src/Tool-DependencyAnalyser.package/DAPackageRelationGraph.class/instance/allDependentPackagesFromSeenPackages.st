accessing
allDependentPackagesFromSeenPackages
	| aSet |
	aSet := Set new.
	self seenPackages do: [ :seenPackage |
		aSet addAll: (seenPackage dependentPackages) ].
	
	^ aSet	
