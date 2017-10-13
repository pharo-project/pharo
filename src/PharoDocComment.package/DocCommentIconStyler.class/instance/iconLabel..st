testing
iconLabel: aNode
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ 'Faulty Example' ]
		ifFalse: [ 'Runnable Example' ]