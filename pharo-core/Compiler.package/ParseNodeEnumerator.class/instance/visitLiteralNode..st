visiting
visitLiteralNode: aLiteralNode
	(theSelectBlock isNil or: [theSelectBlock value: aLiteralNode]) ifFalse:
		[^nil].
	theBlock value: aLiteralNode.
	^super visitLiteralNode: aLiteralNode