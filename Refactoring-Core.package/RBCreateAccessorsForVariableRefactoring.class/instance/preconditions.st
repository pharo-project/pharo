preconditions
preconditions
	^classVariable 
		ifTrue: [RBCondition definesClassVariable: variableName asSymbol in: class]
		ifFalse: [RBCondition definesInstanceVariable: variableName in: class]