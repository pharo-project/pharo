private-parsing
parseUnaryMessage
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.
	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isIdentifier] 
			whileTrue: [node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node