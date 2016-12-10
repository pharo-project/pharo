defaults
iconFor: aNode
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self iconNamed: #smallWarning ]
		ifFalse: [ self iconNamed: #smallInfo ]