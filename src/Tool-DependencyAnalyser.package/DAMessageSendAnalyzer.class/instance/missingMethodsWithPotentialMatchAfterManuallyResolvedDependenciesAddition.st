computing
missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition
	| dictionary |
	
	dictionary := Dictionary new.
	self differenceWithManuallyResolvedDependencies do: [ :selector |
		dictionary 
			at: selector 
			put: (self packagesProvidingSelector: selector) ].
	^ dictionary