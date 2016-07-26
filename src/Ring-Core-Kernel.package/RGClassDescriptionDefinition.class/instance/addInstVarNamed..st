instance variables
addInstVarNamed: aString
	| var |
	var := ((self isMeta 
				ifTrue: [ RGClassInstanceVariableDefinition ]
				ifFalse:[  RGInstanceVariableDefinition ])
		named: aString) parent: self.
	self addVariable: var in: instanceVariables.
	^var