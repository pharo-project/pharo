Extension { #name : #RBArgumentNode }

{ #category : #'*NautilusRefactoring' }
RBArgumentNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameTemporaryNamed: self name asString Between: (self start to: self stop) from: self methodNode
]
