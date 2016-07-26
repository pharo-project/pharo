matching
createMatchingBlock
	| newBlock |
	self arguments size > 2 
		ifTrue: 
			[self 
				error: 'Search blocks can only contain arguments for the node and matching dictionary'].
	newBlock := RBBlockNode arguments: arguments body: body.
	newBlock arguments isEmpty 
		ifTrue: [self addArgumentWithNameBasedOn: 'aNode' to: newBlock].
	newBlock arguments size = 1 
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	^self createBlockFor: newBlock