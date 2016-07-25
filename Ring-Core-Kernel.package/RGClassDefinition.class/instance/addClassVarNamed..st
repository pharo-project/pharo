class variables
addClassVarNamed: aString
	| var |
	var := (RGClassVariableDefinition named: aString) parent: self.
	self addVariable: var in: classVariables.
	^var