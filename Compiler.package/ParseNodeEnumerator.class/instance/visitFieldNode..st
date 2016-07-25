visiting
visitFieldNode: aFieldNode
	(theSelectBlock isNil or: [theSelectBlock value: aFieldNode]) ifFalse:
		[^nil].
	theBlock value: aFieldNode.
	^super visitFieldNode: aFieldNode