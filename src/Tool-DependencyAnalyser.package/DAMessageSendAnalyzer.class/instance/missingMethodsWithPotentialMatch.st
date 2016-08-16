computing
missingMethodsWithPotentialMatch
	^ missingMethods ifNil: 
		[ missingMethods := Dictionary new.
		self unimplementedCalls do: [ :selector |
		missingMethods
			at: selector 
			put: (self packagesProvidingSelector: selector) ].
		missingMethods ]