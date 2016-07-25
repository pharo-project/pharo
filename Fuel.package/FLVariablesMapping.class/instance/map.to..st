accessing
map: sourceName to: destinationName

	| indexOfVariableToMap |
	indexOfVariableToMap := notIgnoredVariables indexOf: sourceName.
	indexOfVariableToMap > 0
		ifTrue: [ mapping 
			at: indexOfVariableToMap 
			put: (theClass
				instVarIndexFor: destinationName
				ifAbsent: [self error: 'Bad variable destination.'])].
