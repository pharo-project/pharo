private
newExampleFromPragma: aPragma
	^ self newExample
		initializeFromFinder: self withPragma: aPragma;
		yourself 