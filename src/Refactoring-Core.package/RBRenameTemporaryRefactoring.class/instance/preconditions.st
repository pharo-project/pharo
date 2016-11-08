preconditions
preconditions
	^ (RBCondition
		withBlock: [ | methodSource |
			interval first > interval last
				ifTrue: [ self refactoringFailure: 'You must select a variable name' ].
			methodSource := class sourceCodeFor: selector.
			methodSource size >= interval last
				ifFalse: [ self refactoringFailure: 'Invalid range for variable' ].
			oldName := methodSource copyFrom: interval first to: interval last.
			true ]) & (RBCondition isValidInstanceVariableName: newName for: class) & (RBCondition definesSelector: selector in: class)
		& (RBCondition definesInstanceVariable: newName in: class) not & (RBCondition definesClassVariable: newName in: class) not