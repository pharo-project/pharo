Extension { #name : #Slot }

{ #category : #'*NautilusRefactoring' }
Slot >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameInstVarNamed: self name asString from: self definingClass
]
