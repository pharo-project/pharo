visiting
visitCommentNode: aCommentNode
	(theSelectBlock isNil or: [theSelectBlock value: aCommentNode]) ifFalse:
		[^nil].
	theBlock value: aCommentNode.
	^super visitCommentNode: aCommentNode