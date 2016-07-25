preconditions
preconditions
	^ RBCondition
		withBlock: [ 
			(class hierarchyDefinesInstanceVariable: variableName)
				ifTrue: [ self refactoringError: 'Already defined in hierarchy' ].true ]