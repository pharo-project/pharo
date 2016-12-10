defaults
iconBlock: aNode
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self notifySourceError: aNode ]
		ifFalse: [ self runExampleBlock: aNode ]