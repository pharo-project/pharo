accessing
guessTypesFor: anInstVarName 
	^bestGuesses at: anInstVarName ifAbsent: [self typesFor: anInstVarName]