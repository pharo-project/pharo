context
testContextPairs
	| pairs x tempVariables indexVariables instVariables |
	
	x:=42.
	pairs := [:y| y+1+x ] asContext gtInspectorVariableValuePairs withIndexCollect: [ :e :i | i -> e].	
	
	tempVariables := pairs select: [ :e | e value key asString beginsWith: '[' ].
	indexVariables := pairs select: [ :e | e value key isInteger ].
	instVariables := pairs select: [ :e | e value key isSymbol ].
	
	self assert: (tempVariables sort last key) < (indexVariables sort first key).
	self assert: (indexVariables sort last key) < (instVariables sort first key).