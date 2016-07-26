compiling
possibleVariablesFor: misspelled continuedFrom: oldResults

	| results |
	results := misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
	self sharedPools do: [:pool | 
		results := misspelled correctAgainstDictionary: pool classPool continuedFrom: results ].
	self superclass == nil
		ifTrue: 
			[ ^ misspelled correctAgainstDictionary: self environment continuedFrom: results ]
		ifFalse:
			[ ^ self superclass possibleVariablesFor: misspelled continuedFrom: results ]