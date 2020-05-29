Extension { #name : #RBInstanceVariableKind }

{ #category : #'*NautilusRefactoring' }
RBInstanceVariableKind >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameInstVarNamed: self name asString from: self methodNode methodClass
]
