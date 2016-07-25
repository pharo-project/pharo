private-parsing
parsePrimitiveObject
	currentToken isPatternBlock 
		ifTrue: [^self parsePatternBlock: RBPatternBlockNode].
	^super parsePrimitiveObject