instance creation
identifierNamed: anIdentifierName at: aPosition
	^anIdentifierName isPatternVariable 
		ifTrue: [super identifierNamed: anIdentifierName at: aPosition]
		ifFalse: [RBVariableNode identifierNamed: anIdentifierName at: aPosition]