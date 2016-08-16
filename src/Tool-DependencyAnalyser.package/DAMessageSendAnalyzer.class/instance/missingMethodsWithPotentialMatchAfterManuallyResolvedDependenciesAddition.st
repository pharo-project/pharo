computing
missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition
	| dictionary |
	
	dictionary := Dictionary new.
	self unimplementedCallsInAllDependencies do: [ :selector |
		dictionary 
			at: selector 
			put: (self packagesProvidingSelector: selector) ].
	^ dictionary