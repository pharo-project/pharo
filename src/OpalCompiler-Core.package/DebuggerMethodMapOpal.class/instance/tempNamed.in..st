public
tempNamed: name in: aContext
	"Answer the value of the temp with name in aContext"
	
	| scope var |
	scope := aContext sourceNodeExecuted scope.
	var := scope lookupVar: name.
	^var searchFromContext: aContext scope: scope.
		