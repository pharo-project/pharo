replacing
replaceNode: oldNode  withNode: newNode
	self statements: (statements collect: [ :statement |
		statement == oldNode 
			ifTrue: [ newNode ]
			ifFalse: [ statement ] ])