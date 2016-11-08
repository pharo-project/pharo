preconditions
checkPreconditions
	| conditions block |
	conditions := self preconditions.
	conditions check
		ifFalse: [ block := conditions errorBlock.
			block
				ifNotNil: [ self refactoringError: conditions errorString with: block ]
				ifNil: [ self refactoringError: conditions errorString ] ]