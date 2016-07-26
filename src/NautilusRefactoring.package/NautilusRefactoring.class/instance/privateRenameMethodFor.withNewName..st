private-method
privateRenameMethodFor: aMethod withNewName: newMethodName
	| class selector  tempOldArguments argumentPermutation |
	class := aMethod methodClass.
	selector := aMethod selector.
	tempOldArguments := aMethod ast arguments collect: #name.
	argumentPermutation := newMethodName arguments collect: [ :each | tempOldArguments indexOf: each ].
	^ RBRenameMethodRefactoring
		model: environment
		renameMethod: selector
		in: class
		to: newMethodName selector
		permutation: argumentPermutation