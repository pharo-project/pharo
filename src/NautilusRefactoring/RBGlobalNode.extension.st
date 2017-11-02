Extension { #name : #RBGlobalNode }

{ #category : #'*NautilusRefactoring' }
RBGlobalNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	self binding variable executeRenameRefactoringBy: aNautilusRefactoring inClass: self methodNode methodClass
]
