private-source
privateCreateCascadeBetween: aSelection from: aMethod
	^ RBCreateCascadeRefactoring 
		model: environment
		combine: aSelection
		from: aMethod selector
		in: aMethod methodClass