private-method
privateRenameMessageFor: aMethodOrMessageNode
	|  newMethodName oldMethodName selector oldArguments argumentPermutation |
	selector := aMethodOrMessageNode selector.
	oldArguments := aMethodOrMessageNode arguments collect: #name.
	oldMethodName := RBMethodName selector: selector arguments: oldArguments.
	(newMethodName := self requestMethodNameFor: oldMethodName) ifNil: [ RefactoringAborted signal ].	
	argumentPermutation := newMethodName arguments collect: [ :each | oldArguments indexOf: each ].
	^ RBRenameMethodRefactoring
		model: environment
		renameMethod: selector
		in: aMethodOrMessageNode methodNode methodClass
		to: newMethodName selector
		permutation: argumentPermutation