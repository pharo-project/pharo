Trait {
	#name : #TRefactoringCommandSupport,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #initialization }
TRefactoringCommandSupport >> initializeDefaultOptionsOf: refactoring [

	NautilusRefactoring new refactoringOptions: refactoring
]
