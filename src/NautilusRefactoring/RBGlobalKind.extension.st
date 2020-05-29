Extension { #name : #RBGlobalKind }

{ #category : #'*NautilusRefactoring' }
RBGlobalKind >> executeRenameRefactoringBy: aNautilusRefactoring [

	self binding variable executeRenameRefactoringBy: aNautilusRefactoring inClass: self methodNode methodClass
]
