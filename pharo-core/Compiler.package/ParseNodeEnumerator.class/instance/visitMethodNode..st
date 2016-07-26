visiting
visitMethodNode: aMethodNode
	(theSelectBlock isNil or: [theSelectBlock value: aMethodNode]) ifFalse:
		[^nil].
	theBlock value: aMethodNode.
	^super visitMethodNode: aMethodNode