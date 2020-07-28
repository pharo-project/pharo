Extension { #name : #ClassVariable }

{ #category : #'*NautilusRefactoring' }
ClassVariable >> executeRenameRefactoringBy: aNautilusRefactoring [
	aNautilusRefactoring renameClassVarNamed: self name from: self definingClass
]
