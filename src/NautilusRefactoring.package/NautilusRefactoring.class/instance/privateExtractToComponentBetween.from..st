private-source
privateExtractToComponentBetween: anInterval from: aMethod
	^ RBExtractMethodToComponentRefactoring
		model: environment
		extract: anInterval
		from: aMethod selector
		in: aMethod methodClass