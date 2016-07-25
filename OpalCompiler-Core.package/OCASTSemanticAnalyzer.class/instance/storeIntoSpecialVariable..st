error handling
storeIntoSpecialVariable: variableNode
	compilationContext forSyntaxHighlighting
		ifTrue: [ ^ self ].
	^ OCStoreIntoSpecialVariableError new
		node: variableNode;
		compilationContext: compilationContext;
		messageText: 'Cannot store into';
		signal