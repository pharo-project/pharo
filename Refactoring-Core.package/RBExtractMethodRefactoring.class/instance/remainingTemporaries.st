transforming
remainingTemporaries
	| temps |
	temps := modifiedParseTree allDefinedVariables asSet.
	extractedParseTree allDefinedVariables 
		do: [:each | temps remove: each ifAbsent: []].
	^temps