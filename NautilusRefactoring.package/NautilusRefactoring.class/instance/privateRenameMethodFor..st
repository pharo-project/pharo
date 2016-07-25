private-method
privateRenameMethodFor: aMethod
	|  newMethodName oldMethodName selector oldArguments |
	selector := aMethod selector.
	oldArguments := aMethod ast arguments collect: #name.
	oldMethodName := RBMethodName selector: selector arguments: oldArguments.
	(newMethodName := self requestMethodNameFor: oldMethodName) ifNil: [ RefactoringAborted signal ].
	^ self privateRenameMethodFor: aMethod withNewName: newMethodName
