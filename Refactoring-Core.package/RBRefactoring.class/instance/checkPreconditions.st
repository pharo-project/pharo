preconditions
checkPreconditions
	| conditions block |
	conditions := self preconditions.
	conditions check 
		ifFalse: 
			[block := conditions errorBlock.
			block notNil 
				ifTrue: [self refactoringError: conditions errorString with: block]
				ifFalse: [self refactoringError: conditions errorString]]