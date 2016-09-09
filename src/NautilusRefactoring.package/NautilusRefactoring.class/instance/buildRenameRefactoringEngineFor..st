class
buildRenameRefactoringEngineFor: aClass

	| newName |
	newName := (UIManager default
			request: 'Enter the new class name:'
			initialAnswer: aClass name).
	newName isEmptyOrNil ifTrue: [ ^ RefactoringAborted signal ].
	^ RBRenameClassRefactoring
		model: RBClassModelFactory rbNamespace new
		rename: aClass
		to: newName