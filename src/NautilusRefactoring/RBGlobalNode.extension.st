Extension { #name : #RBGlobalNode }

{ #category : #'*NautilusRefactoring' }
RBGlobalNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	self binding executeRenameRefactoringBy: aNautilusRefactoring inClass: self methodNode methodClass
]
