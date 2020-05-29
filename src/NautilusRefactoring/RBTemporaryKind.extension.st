Extension { #name : #RBTemporaryKind }

{ #category : #'*NautilusRefactoring' }
RBTemporaryKind >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameTemporaryNamed: self name asString Between: (self start to: self stop) from: self methodNode
]
