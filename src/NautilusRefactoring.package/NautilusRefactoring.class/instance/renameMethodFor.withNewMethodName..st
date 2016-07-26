method
renameMethodFor: aMethod withNewMethodName: aMethodName
	self performRefactoringFor: #privateRenameMethodFor:withNewName: with: aMethod with: aMethodName