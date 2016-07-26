matching
createReplacingBlock
	| newBlock |
	self arguments size > 1 
		ifTrue: 
			[self 
				error: 'Replace blocks can only contain an argument for the matching dictionary'].
	newBlock := RBBlockNode arguments: arguments body: body.
	self arguments isEmpty 
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	^self createBlockFor: newBlock