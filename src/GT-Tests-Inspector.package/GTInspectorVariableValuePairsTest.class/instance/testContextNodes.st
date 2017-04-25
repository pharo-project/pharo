context
testContextNodes
	| pairs x tempVariables indexVariables instVariables |
	
	x:=42.
	pairs := OrderedCollection new.
	[:y| y+1+x ] asContext gtInspectorVariableNodesIn: pairs.	
	pairs := pairs withIndexCollect: [ :e :i | i -> e].
	
	tempVariables := pairs select: [ :e | e value label beginsWith: '[' ].
	indexVariables := pairs select: [ :e | e value key isInteger ].
	instVariables := pairs select: [ :e | e value isKindOf: GTInspectorSlotNode ].
	
	self assert: (tempVariables sort last key) < (indexVariables first key).
	self assert: (indexVariables sort last key) < (instVariables first key).