private-source
privateExtractBetween: anInterval from: aMethod
	^ RBExtractMethodRefactoring
		model: environment
		extract: anInterval
		from: aMethod selector
		in: aMethod methodClass