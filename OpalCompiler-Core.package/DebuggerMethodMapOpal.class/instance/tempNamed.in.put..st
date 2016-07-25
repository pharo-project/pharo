public
tempNamed: name in: aContext put: aValue
	"Assign the value of the temp with name in aContext
	 If the value is a copied value we also need to set it along the lexical chain."

	| scope var |
	scope := aContext sourceNodeExecuted scope.
	var := scope lookupVar: name.
	^var writeFromContext: aContext scope: scope value: aValue.
	
	
	