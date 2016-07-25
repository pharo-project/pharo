visiting
visitReturnNode: aReturnNode
	(theSelectBlock isNil or: [theSelectBlock value: aReturnNode]) ifFalse:
		[^nil].
	theBlock value: aReturnNode.
	^super visitReturnNode: aReturnNode