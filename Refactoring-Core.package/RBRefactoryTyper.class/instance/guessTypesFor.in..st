accessing
guessTypesFor: anInstVarName in: aClass 
	class = aClass ifFalse: [self runOn: aClass].
	^bestGuesses at: anInstVarName
		ifAbsent: [self typesFor: anInstVarName in: aClass]