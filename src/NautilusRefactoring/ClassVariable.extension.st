Extension { #name : #ClassVariable }

{ #category : #'*NautilusRefactoring' }
ClassVariable >> executeRenameRefactoringBy: aNautilusRefactoring inClass: aClass [
	aNautilusRefactoring renameClassVarNamed: self name from: aClass
]
