private-source
privateExtractToTemporaryBetween: anInterval from: aMethod
	^ RBExtractToTemporaryRefactoring
		model: environment
		extract: anInterval
		to: (self request: 'Enter the new variable name:')
		from: aMethod selector
		in: aMethod methodClass