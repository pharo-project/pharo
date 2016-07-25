visiting
visitFutureNode: aFutureNode
	(theSelectBlock isNil or: [theSelectBlock value: aFutureNode]) ifFalse:
		[^nil].
	theBlock value: aFutureNode.
	^super visitFutureNode: aFutureNode