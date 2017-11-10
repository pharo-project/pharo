Extension { #name : #RBRenameInstanceVariableRefactoring }

{ #category : #'*Epicea-Refactorings' }
RBRenameInstanceVariableRefactoring >> asEpiceaEvent [
	^ EpRenameInstanceVariableRefactoring 
		rename: variableName
		to: newName
		in: class name
]

{ #category : #'*Epicea-Refactorings' }
RBRenameInstanceVariableRefactoring >> propagateTransformation [
	self renameReferences
]
