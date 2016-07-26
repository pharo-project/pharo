visiting
visitInstanceVariableNode: anInstanceVariableNode
	
	(self shouldVisit: anInstanceVariableNode) ifFalse:
		[^nil].
	theBlock value: anInstanceVariableNode.
	^super visitInstanceVariableNode: anInstanceVariableNode