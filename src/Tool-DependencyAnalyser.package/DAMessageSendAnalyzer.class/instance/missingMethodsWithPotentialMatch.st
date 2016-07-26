computing
missingMethodsWithPotentialMatch
	^ missingMethods ifNil: 
		[ missingMethods := Dictionary new.
		self difference do: [ :selector |
		missingMethods
			at: selector 
			put: (self packagesProvidingSelector: selector) ].
		missingMethods ]