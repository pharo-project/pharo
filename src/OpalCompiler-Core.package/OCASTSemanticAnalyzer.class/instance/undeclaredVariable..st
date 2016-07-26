error handling
undeclaredVariable: variableNode
	compilationContext forSyntaxHighlighting 
		ifTrue: [ ^OCUndeclaredVariable new name: variableNode name ].
	^ OCUndeclaredVariableWarning new
		node: variableNode;
		compilationContext: compilationContext;
		signal