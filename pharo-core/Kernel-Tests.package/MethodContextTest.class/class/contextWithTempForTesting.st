closures for testing
contextWithTempForTesting 
	| string |
	string := 'test'.
	^ [self class. string asUppercase] asContext.
