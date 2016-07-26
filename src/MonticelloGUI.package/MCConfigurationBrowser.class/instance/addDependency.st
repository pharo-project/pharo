dependencies
addDependency
	(self pickWorkingCopiesSatisfying: [:each | (self includesPackage: each package) not])
		do: [:wc |
			wc ancestors isEmpty
				ifTrue: [self inform: 'You must save ', wc packageName, ' first!
Skipping this package']
				ifFalse: [
					self dependencies add: (MCVersionDependency
						package: wc package
						info: wc ancestors first)]].
	self changed: #dependencyList; changed: #description