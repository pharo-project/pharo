dependencies
wrappedPackageDependencies
	^ (self packageDependencies
		collect: #asPackageDependencyWrapper 
		as: OrderedCollection) sorted