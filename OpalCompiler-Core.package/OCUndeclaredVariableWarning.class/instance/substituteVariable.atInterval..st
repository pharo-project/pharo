correcting
substituteVariable: varName atInterval: anInterval 
	self 
		substituteWord: varName
		wordInterval: anInterval
		offset: 0.
	self methodNode source: self requestor text.
	node replaceWith:((RBVariableNode named: varName) binding: (node owningScope lookupVar: varName)).
	^ (node owningScope lookupVar: varName)
		ifNil: [self error: 'should be found'].