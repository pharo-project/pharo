instance creation
receiver: aValueNode selector: aSelector keywordsPositions: positionList arguments: valueNodes

	^aSelector isPatternVariable
		ifTrue: 
			[super
				receiver: aValueNode
				selector: aSelector 
				keywordsPositions: positionList asIntegerArray
				arguments: valueNodes]
		ifFalse: 
			[RBMessageNode 
				receiver: aValueNode
				selector: aSelector 
				keywordsPositions: positionList asIntegerArray
				arguments: valueNodes]