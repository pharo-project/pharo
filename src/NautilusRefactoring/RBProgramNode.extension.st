Extension { #name : #RBProgramNode }

{ #category : #'*NautilusRefactoring' }
RBProgramNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	self inform: self class name, ' not supports rename'
]
