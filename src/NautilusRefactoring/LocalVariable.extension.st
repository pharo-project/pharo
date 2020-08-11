Extension { #name : #LocalVariable }

{ #category : #'*NautilusRefactoring' }
LocalVariable >> executeRenameRefactoringBy: aNautilusRefactoring for: aNode [

	aNautilusRefactoring renameTemporaryNamed: self name asString Between: (aNode start to: aNode stop) from: aNode methodNode
]
