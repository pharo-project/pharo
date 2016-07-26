error handling
storeIntoReadOnlyVariable: variableNode
	compilationContext forSyntaxHighlighting
		ifTrue: [ ^ self ].
		
	^ OCStoreIntoReadOnlyVariableError new
		node: variableNode;
		compilationContext: compilationContext;
		messageText: 'Cannot store into';
		signal