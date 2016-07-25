visiting
visitBlockNode: block
	super visitBlockNode: block.
	block body statements ifEmpty: [  ^ self ].
	block body statements last isLiteralNode ifFalse: [ ^self ].
	block body statements last value ifNotNil: [ ^self ].
	block body statements removeLast