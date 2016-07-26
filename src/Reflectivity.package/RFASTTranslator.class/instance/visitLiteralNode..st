visitor-double dispatching
visitLiteralNode: aLiteralNode
	self emitMetaLinkBefore: aLiteralNode.
	aLiteralNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aLiteralNode ]
		ifFalse: [ methodBuilder pushLiteral: aLiteralNode value ].
	self emitMetaLinkAfterNoEnsure: aLiteralNode.

	
