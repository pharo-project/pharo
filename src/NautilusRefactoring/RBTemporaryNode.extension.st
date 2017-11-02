Extension { #name : #RBTemporaryNode }

{ #category : #'*NautilusRefactoring' }
RBTemporaryNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameTemporaryNamed: self name asString Between: (self start to: self stop) from: self methodNode
]
