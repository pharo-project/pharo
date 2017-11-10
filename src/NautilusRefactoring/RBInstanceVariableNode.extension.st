Extension { #name : #RBInstanceVariableNode }

{ #category : #'*NautilusRefactoring' }
RBInstanceVariableNode >> executeRenameRefactoringBy: aNautilusRefactoring [

	aNautilusRefactoring renameInstVarNamed: self name asString from: self methodNode methodClass
]
