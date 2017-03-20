private-source
privateExtractToTemporaryBetween: anInterval from: aMethod

	^ RBExtractToTemporaryRefactoring
		model: environment
		extract: anInterval
		to: (self request: self newVariableRequestText)
		from: aMethod selector
		in: aMethod methodClass