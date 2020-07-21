Extension { #name : #GlobalVariable }

{ #category : #'*NautilusRefactoring' }
GlobalVariable >> executeRenameRefactoringBy: aNautilusRefactoring [
	aNautilusRefactoring renameClass: self definingClass 
]
